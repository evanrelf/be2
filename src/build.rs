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

struct BuildContext {
    db: SqlitePool,
    done: papaya::HashMap<Key, SetOnce<()>>,
    store: papaya::HashMap<Key, Value>,
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

    fn task_cx(self: Arc<Self>) -> TaskContext {
        TaskContext::new(self.clone())
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
        let mut cached_values = self.clone().construct(key).await?;

        if let Some(store_value) = self.store.pin().get(key)
            && cached_values.contains(store_value)
        {
            Ok(Some(store_value.clone()))
        } else if let Some(cached_value) = cached_values.drain().next() {
            Ok(Some(cached_value))
        } else {
            Ok(None)
        }
    }

    async fn build(self: Arc<Self>, key: &Key) -> anyhow::Result<Value> {
        let value = match key {
            Key::ReadFile(path) => {
                let bytes = task::task_read_file(self.task_cx(), path).await?;
                Value::Bytes(bytes)
            }
            Key::Concat(path) => {
                let bytes = task::task_concat(self.task_cx(), path).await?;
                Value::Bytes(bytes)
            }
        };

        // TODO: Track task deps
        // self.record(Trace {
        //     key: key.clone(),
        //     deps,
        //     value: value.clone(),
        // });

        Ok(value)
    }

    async fn record(&self, trace: &Trace<Key, Value>) -> anyhow::Result<()> {
        insert_trace(&self.db, trace).await?;

        Ok(())
    }

    async fn construct(self: Arc<Self>, key: &Key) -> anyhow::Result<HashSet<Value>> {
        let traces = fetch_traces(&self.db, Some(key)).await?;

        let mut matches = HashSet::new();

        'trace: for trace in traces {
            debug_assert_eq!(&trace.key, key);

            for (dep_key, dep_value_hash) in trace.deps {
                let dep_value = self.clone().realize(dep_key).await?;
                if dep_value_hash != dep_value.xxhash() {
                    continue 'trace;
                }
            }

            matches.insert(trace.value);
        }

        Ok(matches)
    }
}

pub struct TaskContext {
    build_cx: Arc<BuildContext>,
    deps: HashMap<Key, u64, BuildHasherDefault<XxHash3_64>>,
}

impl TaskContext {
    fn new(build_cx: Arc<BuildContext>) -> Self {
        Self {
            build_cx,
            deps: HashMap::default(),
        }
    }

    pub fn task_cx(&self) -> TaskContext {
        TaskContext::new(self.build_cx.clone())
    }

    pub async fn realize(&self, key: Key) -> anyhow::Result<Value> {
        self.build_cx.clone().realize(key).await
    }

    pub fn use_stubs(&self) -> bool {
        self.build_cx.debug_use_stubs.load(Ordering::SeqCst)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_stubs() -> anyhow::Result<()> {
        let db = SqlitePool::connect(":memory:").await?;
        trace::db_migrate(&db).await?;

        let cx = BuildContext::new(db);
        cx.debug_use_stubs.store(true, Ordering::SeqCst);

        let path = Utf8Path::new("/files");

        let result = cx.clone().realize(Key::Concat(Arc::from(path))).await?;
        assert_eq!(result, Value::Bytes(Bytes::from("AAAA\nAAAA\nBBBB\n")));

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

        assert_eq!(cx.debug_task_count.load(Ordering::SeqCst), 4);

        Ok(())
    }
}
