use camino::Utf8Path;
use sqlx::{
    Row as _, SqlitePool,
    sqlite::{SqliteConnectOptions, SqliteJournalMode, SqliteSynchronous},
};
use std::{collections::HashMap, str::FromStr as _};

pub async fn connect(path: &Utf8Path) -> anyhow::Result<SqlitePool> {
    let sqlite = SqlitePool::connect_with(
        SqliteConnectOptions::from_str(&format!("sqlite://{path}"))?
            .create_if_missing(true)
            .journal_mode(SqliteJournalMode::Wal)
            .synchronous(SqliteSynchronous::Normal),
    )
    .await?;

    Ok(sqlite)
}

pub async fn migrate(db: &SqlitePool) -> anyhow::Result<()> {
    sqlx::query(
        "
        create table if not exists store (
            key blob primary key,
            value blob not null
        ) strict;

        create table if not exists traces (
            id integer primary key,
            key blob not null,
            value blob not null
        ) strict;

        create table if not exists trace_deps (
            trace_id integer not null references traces,
            key blob not null,
            value_hash blob not null,
            unique (trace_id, key, value_hash)
        ) strict;
        ",
    )
    .execute(db)
    .await?;

    Ok(())
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Key(pub Vec<u8>);

#[derive(Clone, Debug, PartialEq)]
pub struct Value(pub Vec<u8>);

#[derive(Debug, PartialEq)]
pub struct Hash(pub u64);

#[derive(Debug, PartialEq)]
pub struct Trace {
    pub key: Key,
    pub value: Value,
    pub deps: HashMap<Key, Hash>,
}

pub async fn fetch(db: &SqlitePool, key: &Key) -> anyhow::Result<Option<Value>> {
    let bytes = sqlx::query_scalar("select value from store where key = $1")
        .bind(&key.0)
        .fetch_optional(db)
        .await?;

    Ok(bytes.map(Value))
}

pub async fn fetch_all(db: &SqlitePool) -> anyhow::Result<HashMap<Key, Value>> {
    let rows = sqlx::query("select key, value from store")
        .fetch_all(db)
        .await?;

    let mut store = HashMap::with_capacity(rows.len());

    for row in rows {
        let key = Key(row.get(0));
        let value = Value(row.get(1));

        store.insert(key, value);
    }

    Ok(store)
}

pub async fn insert(db: &SqlitePool, key: &Key, value: &Value) -> anyhow::Result<()> {
    sqlx::query("insert into store (key, value) values ($1, $2)")
        .bind(&key.0)
        .bind(&value.0)
        .execute(db)
        .await?;

    Ok(())
}

pub async fn fetch_traces(db: &SqlitePool) -> anyhow::Result<Vec<Trace>> {
    let trace_rows = sqlx::query("select id, key, value from traces")
        .fetch_all(db)
        .await?;

    let mut traces = Vec::with_capacity(trace_rows.len());

    for trace_row in trace_rows {
        let trace_id: i64 = trace_row.get(0);
        let key = Key(trace_row.get(1));
        let value = Value(trace_row.get(2));

        let trace_deps_rows =
            sqlx::query("select key, value_hash from trace_deps where trace_id = $1")
                .bind(trace_id)
                .fetch_all(db)
                .await?;

        let mut deps = HashMap::with_capacity(trace_deps_rows.len());

        for trace_deps_row in trace_deps_rows {
            let key = Key(trace_deps_row.get(0));
            let value: Vec<u8> = trace_deps_row.get(1);
            let value: [u8; 8] = match value.try_into() {
                Ok(value) => value,
                Err(bytes) => anyhow::bail!("expected 8 bytes, found {} bytes", bytes.len()),
            };
            let value = Hash(u64::from_le_bytes(value));
            deps.insert(key, value);
        }

        traces.push(Trace { key, value, deps });
    }

    Ok(traces)
}

pub async fn insert_trace(db: &SqlitePool, trace: &Trace) -> anyhow::Result<()> {
    let mut tx = db.begin().await?;

    let trace_id: i64 =
        sqlx::query_scalar("insert into traces (key, value) values ($1, $2) returning id")
            .bind(&trace.key.0)
            .bind(&trace.value.0)
            .fetch_one(&mut *tx)
            .await?;

    for (key, value_hash) in &trace.deps {
        let value_hash = &value_hash.0.to_le_bytes()[..];

        sqlx::query("insert into trace_deps (trace_id, key, value_hash) values ($1, $2, $3)")
            .bind(trace_id)
            .bind(&key.0)
            .bind(value_hash)
            .execute(&mut *tx)
            .await?;
    }

    tx.commit().await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_store_roundtrip() -> anyhow::Result<()> {
        let db = SqlitePool::connect(":memory:").await?;
        migrate(&db).await?;

        let key = Key(Vec::from(b"password"));
        let value = Value(Vec::from(b"hunter2"));

        insert(&db, &key, &value).await?;

        let mut expected_store = HashMap::new();
        expected_store.insert(key, value);

        let actual_store = fetch_all(&db).await?;

        assert_eq!(expected_store, actual_store);

        Ok(())
    }

    #[tokio::test]
    async fn test_trace_roundtrip() -> anyhow::Result<()> {
        let db = SqlitePool::connect(":memory:").await?;
        migrate(&db).await?;

        let key = Key(Vec::from(b"password"));
        let value = Value(Vec::from(b"hunter2"));
        let deps = HashMap::new();
        let trace = Trace { key, value, deps };

        insert_trace(&db, &trace).await?;

        let expected_traces = vec![trace];

        let actual_traces = fetch_traces(&db).await?;

        assert_eq!(expected_traces, actual_traces);

        Ok(())
    }
}
