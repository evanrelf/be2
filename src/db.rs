use bytes::Bytes;
use camino::Utf8Path;
use sqlx::{
    Row as _, SqlitePool,
    sqlite::{SqliteConnectOptions, SqliteJournalMode, SqliteSynchronous},
};
use std::{
    collections::HashMap,
    hash::{BuildHasherDefault, Hash, Hasher},
    str::FromStr as _,
};
use twox_hash::XxHash3_64;

pub async fn connect(path: &Utf8Path) -> sqlx::Result<SqlitePool> {
    let sqlite = SqlitePool::connect_with(
        SqliteConnectOptions::from_str(&format!("sqlite://{path}"))?
            .create_if_missing(true)
            .journal_mode(SqliteJournalMode::Wal)
            .synchronous(SqliteSynchronous::Normal),
    )
    .await?;

    Ok(sqlite)
}

pub async fn migrate(db: &SqlitePool) -> sqlx::Result<()> {
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

#[derive(Debug, PartialEq)]
pub struct Trace {
    pub key: Bytes,
    pub deps: HashMap<Bytes, u64, BuildHasherDefault<XxHash3_64>>,
    pub value: Bytes,
}

impl Hash for Trace {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.hash(state);
        for dep in &self.deps {
            dep.hash(state);
        }
        self.value.hash(state);
    }
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

        let deps_rows =
            sqlx::query("select dep_key, dep_value_hash from trace_deps where trace_id = $1")
                .bind(trace_id)
                .fetch_all(db)
                .await?;

        let mut deps =
            HashMap::with_capacity_and_hasher(deps_rows.len(), BuildHasherDefault::new());

        for deps_row in deps_rows {
            let dep_key: Vec<u8> = deps_row.get(0);
            let dep_key = Bytes::from(dep_key);
            let dep_value_hash: Vec<u8> = deps_row.get(1);
            let dep_value_hash: [u8; 8] = match dep_value_hash.try_into() {
                Ok(value) => value,
                Err(bytes) => anyhow::bail!("expected 8 bytes, found {} bytes", bytes.len()),
            };
            let dep_value_hash = u64::from_le_bytes(dep_value_hash);
            deps.insert(dep_key, dep_value_hash);
        }

        traces.push(Trace { key, deps, value });
    }

    Ok(traces)
}

pub async fn insert_trace(db: &SqlitePool, trace: &Trace) -> sqlx::Result<i64> {
    let mut tx = db.begin().await?;

    let mut hasher = XxHash3_64::default();
    trace.hash(&mut hasher);
    let trace_hash = &hasher.finish().to_le_bytes()[..];

    let row = sqlx::query(
        "
        insert or ignore into traces (key, value, trace_hash) values ($1, $2, $3);

        select id, changes() == 0 as dupe from traces where trace_hash = $3;
        ",
    )
    .bind(&trace.key[..])
    .bind(&trace.value[..])
    .bind(trace_hash)
    .fetch_one(&mut *tx)
    .await?;

    let trace_id = row.get(0);
    let dupe = row.get(1);

    if dupe {
        return Ok(trace_id);
    }

    for (dep_key, dep_value_hash) in &trace.deps {
        let dep_value_hash = &dep_value_hash.to_le_bytes()[..];

        sqlx::query(
            "insert into trace_deps (trace_id, dep_key, dep_value_hash) values ($1, $2, $3)",
        )
        .bind(trace_id)
        .bind(&dep_key[..])
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

    #[tokio::test]
    async fn test_trace_roundtrip() -> anyhow::Result<()> {
        let db = SqlitePool::connect(":memory:").await?;
        migrate(&db).await?;

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
