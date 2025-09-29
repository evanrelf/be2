use crate::{db, task, trace::Trace};
use bytes::Bytes;
use camino::Utf8Path;
use serde::{Deserialize, Serialize};
use sqlx::SqlitePool;
use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher as _},
    str,
    sync::Arc,
};
use twox_hash::XxHash3_64;

#[derive(Clone, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Key {
    Which(Arc<str>),
    ReadFile(Arc<Utf8Path>),
}

#[derive(Clone, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Value {
    Path(Arc<Utf8Path>),
    Bytes(Bytes),
}

impl Value {
    fn xxhash(&self) -> u64 {
        let mut hasher = XxHash3_64::default();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

pub struct Context {
    db: SqlitePool,
    done: HashSet<Key>,
    store: HashMap<Key, Value>,
}

impl Context {
    pub async fn build(&mut self, key: &Key) -> anyhow::Result<Value> {
        if self.done.contains(key) {
            // SAFETY: If a key is marked as done, it has already been built, and its value is
            // present in the store.
            let value = self.store.get(key).unwrap().clone();
            return Ok(value);
        }

        let mut cached_values = self.construct(key).await?;

        #[expect(clippy::let_and_return)]
        let value = if let Some(store_value) = self.store.get(key)
            && cached_values.contains(store_value)
        {
            store_value.clone()
        } else if let Some(cached_value) = cached_values.drain().next() {
            cached_value
        } else {
            // TODO: Track task deps
            let value = match key {
                Key::Which(name) => {
                    let path = task::task_which(self, name).await?;
                    Value::Path(path)
                }
                Key::ReadFile(path) => {
                    let bytes = task::task_read_file(self, path).await?;
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

        self.store.insert(key.clone(), value.clone());
        self.done.insert(key.clone());

        Ok(value)
    }

    async fn record(&mut self, trace: &Trace<Key, Value>) -> anyhow::Result<()> {
        db::insert_trace(&self.db, trace).await?;

        Ok(())
    }

    async fn construct(&mut self, key: &Key) -> anyhow::Result<HashSet<Value>> {
        let traces = db::fetch_traces(&self.db, Some(key)).await?;

        let mut matches = HashSet::new();

        'trace: for trace in traces {
            if trace.key != *key {
                continue 'trace;
            }

            for (dep_key, dep_value_hash) in trace.deps {
                if dep_value_hash != Box::pin(self.build(&dep_key)).await?.xxhash() {
                    continue 'trace;
                }
            }

            matches.insert(trace.value);
        }

        Ok(matches)
    }
}
