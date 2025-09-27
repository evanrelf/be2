use crate::db;
use bytes::Bytes;
use camino::Utf8PathBuf;
use serde::{Deserialize, Serialize};
use sqlx::SqlitePool;
use std::{
    collections::{HashMap, HashSet},
    hash::BuildHasherDefault,
    str,
};
use twox_hash::XxHash3_64;

// TODO: Make `Key` and `Value` types cheap to clone.

#[derive(Deserialize, Serialize)]
pub enum Key {
    Which(String),
    File(Utf8PathBuf),
}

#[derive(Deserialize, Serialize)]
pub enum Value {
    Path(Utf8PathBuf),
    Bytes(Vec<u8>),
}

pub struct Trace {
    pub key: Key,
    pub deps: HashMap<Key, u64, BuildHasherDefault<XxHash3_64>>,
    pub value: Value,
}

impl TryFrom<Trace> for db::Trace {
    type Error = anyhow::Error;
    fn try_from(trace: Trace) -> Result<Self, Self::Error> {
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

pub struct Context {
    db: SqlitePool,
    done: HashSet<Key>,
    store: HashMap<Key, Value>,
}

impl Context {
    #[expect(clippy::unused_async)]
    pub async fn build(&mut self, _key: &Key) -> anyhow::Result<Value> {
        todo!()
    }

    async fn record(&mut self, trace: Trace) -> anyhow::Result<()> {
        let db_trace = db::Trace::try_from(trace)?;
        db::insert_trace(&self.db, &db_trace).await?;
        Ok(())
    }

    #[expect(clippy::unused_async)]
    async fn construct(&mut self, _key: &Key) -> anyhow::Result<HashSet<Value>> {
        todo!()
    }
}
