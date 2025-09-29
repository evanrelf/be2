use crate::{db, task};
use bytes::Bytes;
use camino::Utf8Path;
use serde::{Deserialize, Serialize};
use sqlx::SqlitePool;
use std::{
    collections::{HashMap, HashSet},
    hash::{BuildHasherDefault, Hash, Hasher as _},
    str,
    sync::Arc,
};
use twox_hash::XxHash3_64;

#[derive(Clone, Deserialize, Eq, Hash, PartialEq, Serialize)]
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

pub struct Trace<K, V> {
    pub key: K,
    pub deps: HashMap<K, u64, BuildHasherDefault<XxHash3_64>>,
    pub value: V,
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

    async fn record(&mut self, trace: Trace<Key, Value>) -> anyhow::Result<()> {
        insert_trace(&self.db, trace).await?;

        Ok(())
    }

    async fn construct(&mut self, key: &Key) -> anyhow::Result<HashSet<Value>> {
        let traces = fetch_traces(&self.db, Some(key)).await?;

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

impl<K, V> TryFrom<Trace<K, V>> for db::Trace
where
    K: Serialize,
    V: Serialize,
{
    type Error = anyhow::Error;
    fn try_from(trace: Trace<K, V>) -> Result<Self, Self::Error> {
        let mut key_buf = Vec::new();
        ciborium::into_writer(&trace.key, &mut key_buf)?;
        let key = Bytes::from(key_buf);

        let mut deps = HashMap::default();
        for (dep_key, dep_value_hash) in trace.deps {
            let mut dep_key_buf = Vec::new();
            ciborium::into_writer(&dep_key, &mut dep_key_buf)?;
            let dep_key = Bytes::from(dep_key_buf);
            deps.insert(dep_key, dep_value_hash);
        }

        let mut value_buf = Vec::new();
        ciborium::into_writer(&trace.value, &mut value_buf)?;
        let value = Bytes::from(value_buf);

        Ok(db::Trace { key, deps, value })
    }
}

impl<K, V> TryFrom<db::Trace> for Trace<K, V>
where
    K: for<'de> Deserialize<'de> + Eq + Hash,
    V: for<'de> Deserialize<'de>,
{
    type Error = anyhow::Error;
    fn try_from(db_trace: db::Trace) -> Result<Self, Self::Error> {
        let key = ciborium::from_reader(&db_trace.key[..])?;

        let mut deps = HashMap::default();
        for (db_dep_key, dep_value_hash) in db_trace.deps {
            let dep_key = ciborium::from_reader(&db_dep_key[..])?;
            deps.insert(dep_key, dep_value_hash);
        }

        let value = ciborium::from_reader(&db_trace.value[..])?;

        Ok(Trace { key, deps, value })
    }
}

async fn fetch_traces<K, V>(db: &SqlitePool, key: Option<&K>) -> anyhow::Result<Vec<Trace<K, V>>>
where
    K: for<'de> Deserialize<'de> + Eq + Hash + Serialize,
    V: for<'de> Deserialize<'de>,
{
    let db_key = if let Some(key) = key {
        let mut db_key_buf = Vec::new();
        ciborium::into_writer(key, &mut db_key_buf)?;
        let db_key = Bytes::from(db_key_buf);
        Some(db_key)
    } else {
        None
    };

    let db_traces = db::fetch_traces(db, db_key.as_ref()).await?;

    let mut traces = Vec::with_capacity(db_traces.len());

    for db_trace in db_traces {
        let trace = Trace::try_from(db_trace)?;
        traces.push(trace);
    }

    Ok(traces)
}

async fn insert_trace<K, V>(db: &SqlitePool, trace: Trace<K, V>) -> anyhow::Result<i64>
where
    K: Serialize,
    V: Serialize,
{
    let db_trace = db::Trace::try_from(trace)?;
    let trace_id = db::insert_trace(db, &db_trace).await?;
    Ok(trace_id)
}
