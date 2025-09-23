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
        create table if not exists products (
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

#[derive(Eq, Hash, PartialEq)]
pub struct Key(pub Vec<u8>);

pub struct Value(pub Vec<u8>);

pub struct Hash(pub u64);

pub struct Product {
    pub key: Key,
    pub value: Value,
}

pub struct Trace {
    pub key: Key,
    pub value: Value,
    pub deps: HashMap<Key, Hash>,
}

pub async fn select_products(db: &SqlitePool) -> anyhow::Result<HashMap<Key, Value>> {
    let rows = sqlx::query("select key, value from products")
        .fetch_all(db)
        .await?;

    let mut products = HashMap::with_capacity(rows.len());

    for row in rows {
        let key = Key(row.get(0));
        let value = Value(row.get(1));

        products.insert(key, value);
    }

    Ok(products)
}

// TODO: Handle when primary key constraint is violated on insert. Overwrite existing entry?
pub async fn insert_product(db: &SqlitePool, product: &Product) -> anyhow::Result<()> {
    sqlx::query("insert into products (key, value) values ($1, $2)")
        .bind(&product.key.0)
        .bind(&product.value.0)
        .execute(db)
        .await?;

    Ok(())
}

pub async fn select_traces(db: &SqlitePool) -> anyhow::Result<Vec<Trace>> {
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
            // TODO: `blob` -> `u64`
            // let value: Vec<u8> = trace_deps_row.get(1);
            // let value: [u8; 8] = value.try_into()?;
            // let value = Hash(u64::from_le_bytes(value));
            let value = Hash(42);
            deps.insert(key, value);
        }

        traces.push(Trace { key, value, deps });
    }

    Ok(traces)
}

// TODO: Handle when unique constraint is violated on insert. Do nothing?
pub async fn insert_trace(db: &SqlitePool, trace: &Trace) -> anyhow::Result<()> {
    let mut tx = db.begin().await?;

    let trace_id: i64 =
        sqlx::query_scalar("insert into traces (key, value) values ($1, $2) returning id")
            .bind(&trace.key.0)
            .bind(&trace.value.0)
            .fetch_one(&mut *tx)
            .await?;

    for (key, value_hash) in &trace.deps {
        // TODO: `u64` -> `blob`
        let _ = value_hash;
        let value_hash: Vec<u8> = vec![0x42];

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
