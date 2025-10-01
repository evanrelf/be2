use crate::util::Xxhash as _;
use serde::{Deserialize, Serialize};
use sqlx::{Row as _, SqlitePool};
use std::{
    collections::HashMap,
    hash::{BuildHasherDefault, Hash, Hasher},
};
use twox_hash::XxHash3_64;

pub trait Key: for<'de> Deserialize<'de> + Clone + Hash + Ord + Serialize {}

impl<T> Key for T where T: for<'de> Deserialize<'de> + Clone + Hash + Ord + Serialize {}

pub trait Value: for<'de> Deserialize<'de> + Clone + Eq + Hash + Serialize {}

impl<T> Value for T where T: for<'de> Deserialize<'de> + Clone + Eq + Hash + Serialize {}

#[derive(Debug, PartialEq)]
pub struct Trace<K, V>
where
    K: Key,
    V: Value,
{
    pub key: K,
    pub deps: HashMap<K, u64, BuildHasherDefault<XxHash3_64>>,
    pub value: V,
}

impl<K, V> Hash for Trace<K, V>
where
    K: Key,
    V: Value,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.hash(state);
        let mut deps = Vec::from_iter(&self.deps);
        deps.sort_unstable();
        for dep in &deps {
            dep.hash(state);
        }
        self.value.hash(state);
    }
}

pub async fn db_migrate(db: &SqlitePool) -> sqlx::Result<()> {
    sqlx::query(
        "
        create table if not exists traces (
            id integer primary key,
            key blob not null,
            value blob not null,
            trace_hash blob not null unique
        ) strict;

        create table if not exists trace_deps (
            trace_id integer not null references traces on delete cascade,
            dep_key blob not null,
            dep_value_hash blob not null,
            unique (trace_id, dep_key)
        ) strict;

        create index if not exists idx_traces_key on traces(key);

        create trigger if not exists forbid_trace_update
        before update on traces
        begin
            select raise(abort, 'traces are immutable');
        end;

        create trigger if not exists forbid_trace_deps_update
        before update on trace_deps
        begin
            select raise(abort, 'trace dependencies are immutable');
        end;

        create trigger if not exists forbid_trace_deps_delete
        before delete on trace_deps
        when (select count(*) from traces where id = old.trace_id) > 0
        begin
            select raise(abort, 'trace dependencies cannot be deleted directly');
        end;
        ",
    )
    .execute(db)
    .await?;

    Ok(())
}

pub async fn fetch_traces<K, V>(
    db: &SqlitePool,
    key: Option<&K>,
) -> anyhow::Result<Vec<Trace<K, V>>>
where
    K: Key,
    V: Value,
{
    let trace_rows = if let Some(key) = key {
        let mut key_bytes = Vec::new();
        ciborium::into_writer(key, &mut key_bytes)?;

        sqlx::query("select id, null, value, trace_hash from traces where key = $1")
            .bind(&key_bytes)
            .fetch_all(db)
            .await?
    } else {
        sqlx::query("select id, key, value, trace_hash from traces")
            .fetch_all(db)
            .await?
    };

    let mut traces = Vec::with_capacity(trace_rows.len());

    for trace_row in trace_rows {
        let trace_id: i64 = trace_row.get(0);
        let key: K = if let Some(key) = key {
            key.clone()
        } else {
            let key: Vec<u8> = trace_row.get(1);
            ciborium::from_reader(&key[..])?
        };
        let value: Vec<u8> = trace_row.get(2);
        let value: V = ciborium::from_reader(&value[..])?;

        let deps_rows =
            sqlx::query("select dep_key, dep_value_hash from trace_deps where trace_id = $1")
                .bind(trace_id)
                .fetch_all(db)
                .await?;

        let mut deps =
            HashMap::with_capacity_and_hasher(deps_rows.len(), BuildHasherDefault::new());

        for deps_row in deps_rows {
            let dep_key: Vec<u8> = deps_row.get(0);
            let dep_key = ciborium::from_reader(&dep_key[..])?;
            let dep_value_hash: Vec<u8> = deps_row.get(1);
            let dep_value_hash: [u8; 8] = match dep_value_hash.try_into() {
                Ok(value) => value,
                Err(bytes) => anyhow::bail!("expected 8 bytes, found {} bytes", bytes.len()),
            };
            let dep_value_hash = u64::from_le_bytes(dep_value_hash);
            deps.insert(dep_key, dep_value_hash);
        }

        let trace_hash: Vec<u8> = trace_row.get(3);
        let trace_hash: [u8; 8] = match trace_hash.try_into() {
            Ok(value) => value,
            Err(bytes) => anyhow::bail!("expected 8 bytes, found {} bytes", bytes.len()),
        };
        let trace_hash = u64::from_le_bytes(trace_hash);

        let trace = Trace { key, deps, value };

        debug_assert_eq!(trace_hash, trace.xxhash());

        traces.push(trace);
    }

    Ok(traces)
}

pub async fn insert_trace<K, V>(db: &SqlitePool, trace: &Trace<K, V>) -> anyhow::Result<i64>
where
    K: Key,
    V: Value,
{
    let mut tx = db.begin().await?;

    let mut hasher = XxHash3_64::default();
    trace.hash(&mut hasher);
    let trace_hash = &hasher.finish().to_le_bytes()[..];

    let mut key_bytes = Vec::new();
    ciborium::into_writer(&trace.key, &mut key_bytes)?;

    let mut value_bytes = Vec::new();
    ciborium::into_writer(&trace.value, &mut value_bytes)?;

    sqlx::query("insert or ignore into traces (key, value, trace_hash) values ($1, $2, $3)")
        .bind(&key_bytes)
        .bind(&value_bytes)
        .bind(trace_hash)
        .execute(&mut *tx)
        .await?;

    let row = sqlx::query("select id, changes() == 0 as is_dupe from traces where trace_hash = $3")
        .bind(&key_bytes)
        .bind(&value_bytes)
        .bind(trace_hash)
        .fetch_one(&mut *tx)
        .await?;

    let trace_id = row.get(0);
    let is_dupe = row.get(1);

    if is_dupe {
        tracing::warn!(trace_id, "Trace already exists in database");

        tx.rollback().await?;

        return Ok(trace_id);
    }

    for (dep_key, dep_value_hash) in &trace.deps {
        let mut dep_key_bytes = Vec::new();
        ciborium::into_writer(dep_key, &mut dep_key_bytes)?;

        let dep_value_hash = &dep_value_hash.to_le_bytes()[..];

        sqlx::query(
            "insert into trace_deps (trace_id, dep_key, dep_value_hash) values ($1, $2, $3)",
        )
        .bind(trace_id)
        .bind(dep_key_bytes)
        .bind(dep_value_hash)
        .execute(&mut *tx)
        .await?;
    }

    tx.commit().await?;

    Ok(trace_id)
}

#[cfg(test)]
mod tests {
    use super::*;
    use bytes::Bytes;

    #[tokio::test]
    async fn test_trace_roundtrip() -> anyhow::Result<()> {
        let db = SqlitePool::connect(":memory:").await?;
        db_migrate(&db).await?;

        let key = Bytes::from("password");
        let mut deps = HashMap::default();
        deps.insert(Bytes::from("answer"), 42);
        let value = Bytes::from("hunter2");
        let trace = Trace { key, deps, value };

        // Traces are de-duplicated
        let trace_id_1 = insert_trace(&db, &trace).await?;
        let trace_id_2 = insert_trace(&db, &trace).await?;
        assert_eq!(trace_id_1, trace_id_2);

        let expected_traces = vec![trace];

        let actual_traces = fetch_traces(&db, None).await?;

        assert_eq!(expected_traces, actual_traces);

        Ok(())
    }
}
