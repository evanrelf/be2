use bytes::Bytes;
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

#[derive(Debug, PartialEq)]
pub struct Trace {
    pub key: Bytes,
    pub value: Bytes,
    pub deps: HashMap<Bytes, u64>,
}

pub async fn fetch_traces(db: &SqlitePool, key: Option<&Bytes>) -> anyhow::Result<Vec<Trace>> {
    let trace_rows = if let Some(key) = key {
        sqlx::query("select id, null, value from traces where key = $1")
            .bind(&key[..])
            .fetch_all(db)
            .await?
    } else {
        sqlx::query("select id, key, value from traces")
            .fetch_all(db)
            .await?
    };

    let mut traces = Vec::with_capacity(trace_rows.len());

    for trace_row in trace_rows {
        let trace_id: i64 = trace_row.get(0);
        let key = if let Some(key) = key {
            key.clone()
        } else {
            let key: Vec<u8> = trace_row.get(1);
            Bytes::from(key)
        };
        let value: Vec<u8> = trace_row.get(2);
        let value = Bytes::from(value);

        let deps_rows = sqlx::query("select key, value_hash from trace_deps where trace_id = $1")
            .bind(trace_id)
            .fetch_all(db)
            .await?;

        let mut deps = HashMap::with_capacity(deps_rows.len());

        for deps_row in deps_rows {
            let dep_key: Vec<u8> = deps_row.get(0);
            let dep_key = Bytes::from(dep_key);
            let dep_value: Vec<u8> = deps_row.get(1);
            let dep_value: [u8; 8] = match dep_value.try_into() {
                Ok(value) => value,
                Err(bytes) => anyhow::bail!("expected 8 bytes, found {} bytes", bytes.len()),
            };
            let dep_value = u64::from_le_bytes(dep_value);
            deps.insert(dep_key, dep_value);
        }

        traces.push(Trace { key, value, deps });
    }

    Ok(traces)
}

pub async fn insert_trace(db: &SqlitePool, trace: &Trace) -> anyhow::Result<()> {
    let mut tx = db.begin().await?;

    let trace_id: i64 =
        sqlx::query_scalar("insert into traces (key, value) values ($1, $2) returning id")
            .bind(&trace.key[..])
            .bind(&trace.value[..])
            .fetch_one(&mut *tx)
            .await?;

    for (dep_key, dep_value_hash) in &trace.deps {
        let dep_value_hash = &dep_value_hash.to_le_bytes()[..];

        sqlx::query("insert into trace_deps (trace_id, key, value_hash) values ($1, $2, $3)")
            .bind(trace_id)
            .bind(&dep_key[..])
            .bind(dep_value_hash)
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
    async fn test_trace_roundtrip() -> anyhow::Result<()> {
        let db = SqlitePool::connect(":memory:").await?;
        migrate(&db).await?;

        let key = Bytes::from("password");
        let value = Bytes::from("hunter2");
        let deps = HashMap::new();
        let trace = Trace { key, value, deps };

        insert_trace(&db, &trace).await?;

        let expected_traces = vec![trace];

        let actual_traces = fetch_traces(&db, None).await?;

        assert_eq!(expected_traces, actual_traces);

        Ok(())
    }
}
