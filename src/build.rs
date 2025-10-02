pub mod task;
pub mod trace;

use crate::{
    build::trace::{Trace, fetch_traces, insert_trace},
    util::Xxhash as _,
};
use async_recursion::async_recursion;
use bytes::Bytes;
use camino::Utf8Path;
use serde::{Deserialize, Serialize};
use sqlx::SqlitePool;
use std::{
    collections::{HashMap, HashSet},
    hash::{BuildHasherDefault, Hash},
    str,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};
use tokio::sync::SetOnce;
use twox_hash::XxHash3_64;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Key {
    ReadFile(Arc<Utf8Path>),
    Concat(Arc<Utf8Path>),
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Value {
    Bytes(Bytes),
}

struct BuildContext<K = Key, V = Value> {
    db: SqlitePool,
    done: papaya::HashMap<K, SetOnce<()>>,
    store: papaya::HashMap<K, V>,
    debug_use_stubs: AtomicBool,
    debug_task_count: AtomicUsize,
}

impl BuildContext {
    fn new(db: SqlitePool) -> Arc<Self> {
        Arc::new(Self {
            db,
            done: papaya::HashMap::new(),
            store: papaya::HashMap::new(),
            debug_use_stubs: AtomicBool::new(false),
            debug_task_count: AtomicUsize::new(0),
        })
    }

    #[async_recursion]
    async fn realize(self: Arc<Self>, key: Key) -> anyhow::Result<Value> {
        let done = self.done.pin_owned();

        let mut is_done = true;

        let barrier = done.get_or_insert_with(key.clone(), || {
            is_done = false;
            SetOnce::new()
        });

        if is_done {
            barrier.wait().await;
            // SAFETY: The key is marked as done, so it has already been built, and its value is
            // present in the store.
            let value = self.store.pin().get(&key).unwrap().clone();
            return Ok(value);
        }

        let value = if let Some(value) = self.clone().fetch(&key).await? {
            value
        } else {
            self.debug_task_count.fetch_add(1, Ordering::SeqCst);
            self.clone().build(&key).await?
        };

        self.store.pin().insert(key.clone(), value.clone());

        // SAFETY: The key has not been marked as done yet, and no other tasks will attempt to.
        barrier.set(()).unwrap();

        Ok(value)
    }

    async fn fetch(self: Arc<Self>, key: &Key) -> anyhow::Result<Option<Value>> {
        let traces = fetch_traces(&self.db, Some(key)).await?;

        let mut matches = HashSet::new();

        // TODO: Check traces concurrently
        'trace: for trace in traces {
            debug_assert_eq!(&trace.key, key);

            // TODO: Realize trace deps concurrently
            for (dep_key, dep_value_hash) in trace.deps {
                let dep_value = self.clone().realize(dep_key).await?;
                if dep_value_hash != dep_value.xxhash() {
                    continue 'trace;
                }
            }

            matches.insert(trace.value);
        }

        if let Some(store_value) = self.store.pin().get(key)
            && matches.contains(store_value)
        {
            Ok(Some(store_value.clone()))
        } else if let Some(cached_value) = matches.drain().next() {
            Ok(Some(cached_value))
        } else {
            Ok(None)
        }
    }

    async fn build(self: Arc<Self>, key: &Key) -> anyhow::Result<Value> {
        let task_cx = Arc::new(TaskContext::new(self.clone()));

        // If a task is impure or cheaper to rebuild than to cache, mark it as volatile to skip
        // recording traces.
        //
        // For example: reading a file directly (i.e. not via an intermediate file I/O task).
        let mut volatile = false;

        let value = match key {
            Key::ReadFile(path) => {
                volatile = true;
                let bytes = task::task_read_file(task_cx.clone(), path).await?;
                Value::Bytes(bytes)
            }
            Key::Concat(path) => {
                let bytes = task::task_concat(task_cx.clone(), path).await?;
                Value::Bytes(bytes)
            }
        };

        if !volatile {
            insert_trace(
                &self.db,
                &Trace {
                    key: key.clone(),
                    deps: task_cx.deps(),
                    value: value.clone(),
                },
            )
            .await?;
        }

        Ok(value)
    }
}

pub struct TaskContext<K = Key, V = Value> {
    build_cx: Arc<BuildContext<K, V>>,
    deps: papaya::HashMap<K, u64>,
}

impl TaskContext {
    fn new(build_cx: Arc<BuildContext>) -> Self {
        Self {
            build_cx,
            deps: papaya::HashMap::new(),
        }
    }

    fn deps(&self) -> HashMap<Key, u64, BuildHasherDefault<XxHash3_64>> {
        let mut deps =
            HashMap::with_capacity_and_hasher(self.deps.len(), BuildHasherDefault::default());

        for (key, value_hash) in &self.deps.pin() {
            deps.insert(key.clone(), *value_hash);
        }

        deps
    }

    pub async fn realize(&self, key: Key) -> anyhow::Result<Value> {
        let value = self.build_cx.clone().realize(key.clone()).await?;
        self.deps.pin().insert(key, value.xxhash());
        Ok(value)
    }

    pub fn use_stubs(&self) -> bool {
        self.build_cx.debug_use_stubs.load(Ordering::SeqCst)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use similar_asserts::assert_eq;

    #[tokio::test]
    async fn test_stubs() -> anyhow::Result<()> {
        let db = SqlitePool::connect(":memory:").await?;
        trace::db_migrate(&db).await?;

        let cx = BuildContext::new(db);
        cx.debug_use_stubs.store(true, Ordering::SeqCst);

        let path = Utf8Path::new("/files");

        let result = cx.clone().realize(Key::Concat(Arc::from(path))).await?;
        let expected_result = Value::Bytes(Bytes::from("AAAA\nAAAA\nBBBB\n"));
        assert_eq!(result, expected_result);

        let expected_store = papaya::HashMap::new();
        expected_store.pin().insert(
            Key::ReadFile(Arc::from(Utf8Path::new("/files"))),
            Value::Bytes(Bytes::from("/files/a\n/files/a\n/files/b\n")),
        );
        expected_store.pin().insert(
            Key::ReadFile(Arc::from(Utf8Path::new("/files/a"))),
            Value::Bytes(Bytes::from("AAAA\n")),
        );
        expected_store.pin().insert(
            Key::ReadFile(Arc::from(Utf8Path::new("/files/b"))),
            Value::Bytes(Bytes::from("BBBB\n")),
        );
        expected_store.pin().insert(
            Key::Concat(Arc::from(Utf8Path::new("/files"))),
            Value::Bytes(Bytes::from("AAAA\nAAAA\nBBBB\n")),
        );
        assert_eq!(cx.store, expected_store);

        let expected_done = papaya::HashMap::new();
        for key in expected_store.pin().keys() {
            expected_done
                .pin()
                .insert(key.clone(), SetOnce::new_with(Some(())));
        }
        assert_eq!(cx.done, expected_done);

        let traces = fetch_traces::<Key, Value>(&cx.db, None).await?;

        #[expect(clippy::unreadable_literal)]
        let expected_traces = vec![Trace {
            key: Key::Concat(Arc::from(Utf8Path::new("/files"))),
            deps: {
                let mut deps = HashMap::default();
                deps.insert(
                    Key::ReadFile(Arc::from(Utf8Path::new("/files"))),
                    7034874801995377595,
                );
                deps.insert(
                    Key::ReadFile(Arc::from(Utf8Path::new("/files/a"))),
                    6110124553518527577,
                );
                deps.insert(
                    Key::ReadFile(Arc::from(Utf8Path::new("/files/b"))),
                    4445544808285449819,
                );
                deps
            },
            value: Value::Bytes(Bytes::from("AAAA\nAAAA\nBBBB\n")),
        }];
        assert_eq!(traces, expected_traces);

        // 3 `read_file`s, 1 `concat`
        assert_eq!(cx.debug_task_count.load(Ordering::SeqCst), 4);

        let db = Arc::into_inner(cx).unwrap().db;

        let cx = BuildContext::new(db);
        cx.debug_use_stubs.store(true, Ordering::SeqCst);

        let result = cx.clone().realize(Key::Concat(Arc::from(path))).await?;

        // Second run should produce the same results...
        assert_eq!(result, expected_result);
        assert_eq!(cx.store, expected_store);
        assert_eq!(cx.done, expected_done);
        let traces = fetch_traces::<Key, Value>(&cx.db, None).await?;
        assert_eq!(traces, expected_traces);
        // ...but not run any non-volatile tasks, because they're cached.
        // 3 `read_file`s (volatile), 1 `concat` (non-volatile)
        assert_eq!(cx.debug_task_count.load(Ordering::SeqCst), 3);

        Ok(())
    }
}
