use crate::build::{db, hash::Xxhash as _, task, trace::Trace};
use async_recursion::async_recursion;
use bytes::Bytes;
use camino::Utf8Path;
use serde::{Deserialize, Serialize};
use sqlx::SqlitePool;
use std::{
    collections::HashSet,
    hash::Hash,
    str,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};
use tokio::sync::SetOnce;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Key {
    Which(Arc<str>),
    ReadFile(Arc<Utf8Path>),
    Concat(Arc<Utf8Path>),
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Value {
    Path(Arc<Utf8Path>),
    Bytes(Bytes),
}

pub struct Context {
    db: SqlitePool,
    done: papaya::HashMap<Key, SetOnce<()>>,
    store: papaya::HashMap<Key, Value>,
    debug_use_stubs: AtomicBool,
    debug_task_count: AtomicUsize,
}

impl Context {
    pub fn new(db: SqlitePool) -> Self {
        Self {
            db,
            done: papaya::HashMap::new(),
            store: papaya::HashMap::new(),
            debug_use_stubs: AtomicBool::new(false),
            debug_task_count: AtomicUsize::new(0),
        }
    }

    #[async_recursion]
    pub async fn build(&self, key: &Key) -> anyhow::Result<Value> {
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
            let value = self.store.pin().get(key).unwrap().clone();
            return Ok(value);
        }

        let mut cached_values = self.construct(key).await?;

        #[expect(clippy::let_and_return)]
        let value = if let Some(store_value) = self.store.pin().get(key)
            && cached_values.contains(store_value)
        {
            store_value.clone()
        } else if let Some(cached_value) = cached_values.drain().next() {
            cached_value
        } else {
            self.debug_task_count.fetch_add(1, Ordering::SeqCst);

            // TODO: Track task deps
            let value = match key {
                Key::Which(name) => {
                    let path = if self.debug_use_stubs.load(Ordering::SeqCst) {
                        task::task_which_stub(self, name).await?
                    } else {
                        task::task_which(self, name).await?
                    };
                    Value::Path(path)
                }
                Key::ReadFile(path) => {
                    let bytes = if self.debug_use_stubs.load(Ordering::SeqCst) {
                        task::task_read_file_stub(self, path).await?
                    } else {
                        task::task_read_file(self, path).await?
                    };
                    Value::Bytes(bytes)
                }
                Key::Concat(path) => {
                    let bytes = task::task_concat(self, path).await?;
                    Value::Bytes(bytes)
                }
            };

            // let deps = todo!();

            // self.record(Trace {
            //     key: key.clone(),
            //     deps,
            //     value: value.clone(),
            // });

            value
        };

        self.store.pin().insert(key.clone(), value.clone());

        // SAFETY: The key has not been marked as done yet, and no other tasks will attempt to.
        barrier.set(()).unwrap();

        Ok(value)
    }

    async fn record(&self, trace: &Trace<Key, Value>) -> anyhow::Result<()> {
        db::insert_trace(&self.db, trace).await?;

        Ok(())
    }

    async fn construct(&self, key: &Key) -> anyhow::Result<HashSet<Value>> {
        let traces = db::fetch_traces(&self.db, Some(key)).await?;

        let mut matches = HashSet::new();

        'trace: for trace in traces {
            debug_assert_eq!(&trace.key, key);

            for (dep_key, dep_value_hash) in trace.deps {
                let dep_value = self.build(&dep_key).await?;
                if dep_value_hash != dep_value.xxhash() {
                    continue 'trace;
                }
            }

            matches.insert(trace.value);
        }

        Ok(matches)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_stubs() -> anyhow::Result<()> {
        let db = SqlitePool::connect(":memory:").await?;
        db::migrate(&db).await?;

        let cx = Context::new(db);
        cx.debug_use_stubs.store(true, Ordering::SeqCst);

        let path = Utf8Path::new("/files");

        let result = task::concat(&cx, path).await?;
        assert_eq!(result, Bytes::from("AAAA\nAAAA\nBBBB\n"));

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
