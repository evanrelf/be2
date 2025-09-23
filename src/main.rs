#![allow(dead_code)]
#![allow(clippy::unused_async)]

use camino::{Utf8Path, Utf8PathBuf};
use clap::Parser as _;
use etcetera::app_strategy::{AppStrategy as _, AppStrategyArgs, Xdg};
use serde::{Deserialize, Serialize};
use sqlx::{
    Row as _, SqlitePool,
    sqlite::{SqliteConnectOptions, SqliteJournalMode, SqliteSynchronous},
};
use std::{collections::HashMap, str::FromStr as _};
use tokio::fs;

#[derive(clap::Parser)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    Lint,
    Clean,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    tracing_subscriber::fmt::init();

    let xdg = Xdg::new(AppStrategyArgs {
        top_level_domain: String::from("com"),
        author: String::from("Evan Relf"),
        app_name: String::from("Be2"),
    })?;

    let cache_dir = Utf8PathBuf::try_from(xdg.cache_dir())?;

    fs::create_dir_all(&cache_dir).await?;

    let sqlite_path = cache_dir.join("store.sqlite");

    match args.command {
        Command::Lint => {
            let sqlite = sqlite_connect(&sqlite_path).await?;
            sqlite_migrate(&sqlite).await?;

            tracing::info!("totally linting right now...");
        }
        Command::Clean => {
            if fs::try_exists(&sqlite_path).await? {
                fs::remove_file(&sqlite_path).await?;
            }
        }
    }

    Ok(())
}

async fn sqlite_connect(path: &Utf8Path) -> anyhow::Result<SqlitePool> {
    let sqlite = SqlitePool::connect_with(
        SqliteConnectOptions::from_str(&format!("sqlite://{path}"))?
            .create_if_missing(true)
            .journal_mode(SqliteJournalMode::Wal)
            .synchronous(SqliteSynchronous::Normal),
    )
    .await?;

    Ok(sqlite)
}

async fn sqlite_migrate(sqlite: &SqlitePool) -> anyhow::Result<()> {
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
    .execute(sqlite)
    .await?;

    Ok(())
}

#[derive(Deserialize, Eq, Hash, PartialEq, Serialize)]
enum Key {
    Which(String),
    ReadFile(Utf8PathBuf),
    Format(Vec<u8>),
    Lint(Vec<u8>),
}

#[derive(Deserialize, Serialize)]
enum Value {
    Path(Utf8PathBuf),
    Bytes(Vec<u8>),
}

#[derive(Deserialize, Serialize)]
struct Hash(u64);

struct Store {
    products: HashMap<Key, Value>,
    traces: Vec<Trace>,
}

struct Product {
    key: Key,
    value: Value,
}

struct Trace {
    key: Key,
    value: Value,
    deps: HashMap<Key, Hash>,
}

async fn select_store(sqlite: &SqlitePool) -> anyhow::Result<Store> {
    let products = select_products(sqlite).await?;
    let traces = select_traces(sqlite).await?;

    Ok(Store { products, traces })
}

// TODO: Should this handle merging in-memory store with database store? Or should it assume an
// empty database, and we leave merging / flushing changes to another function?
async fn insert_store(sqlite: &SqlitePool, store: Store) -> anyhow::Result<()> {
    for (key, value) in store.products {
        let product = Product { key, value };
        insert_product(sqlite, &product).await?;
    }

    for trace in store.traces {
        insert_trace(sqlite, &trace).await?;
    }

    Ok(())
}

async fn select_products(sqlite: &SqlitePool) -> anyhow::Result<HashMap<Key, Value>> {
    let rows = sqlx::query("select key, value from products")
        .fetch_all(sqlite)
        .await?;

    let mut products = HashMap::with_capacity(rows.len());

    for row in rows {
        let key = serde_json::from_slice(row.get(0))?;
        let value = serde_json::from_slice(row.get(1))?;

        products.insert(key, value);
    }

    Ok(products)
}

// TODO: Handle when primary key constraint is violated on insert. Overwrite existing entry?
async fn insert_product(sqlite: &SqlitePool, product: &Product) -> anyhow::Result<()> {
    let key = serde_json::to_vec(&product.key)?;
    let value = serde_json::to_vec(&product.value)?;

    sqlx::query("insert into products (key, value) values ($1, $2)")
        .bind(key)
        .bind(value)
        .execute(sqlite)
        .await?;

    Ok(())
}

async fn select_traces(sqlite: &SqlitePool) -> anyhow::Result<Vec<Trace>> {
    let trace_rows = sqlx::query("select id, key, value from traces")
        .fetch_all(sqlite)
        .await?;

    let mut traces = Vec::with_capacity(trace_rows.len());

    for trace_row in trace_rows {
        let trace_id: i64 = trace_row.get(0);
        let key = serde_json::from_slice(trace_row.get(1))?;
        let value = serde_json::from_slice(trace_row.get(2))?;

        let trace_deps_rows =
            sqlx::query("select key, value_hash from trace_deps where trace_id = $1")
                .bind(trace_id)
                .fetch_all(sqlite)
                .await?;

        let mut deps = HashMap::with_capacity(trace_deps_rows.len());

        for trace_deps_row in trace_deps_rows {
            let key = serde_json::from_slice(trace_deps_row.get(0))?;
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
async fn insert_trace(sqlite: &SqlitePool, trace: &Trace) -> anyhow::Result<()> {
    let key = serde_json::to_vec(&trace.key)?;
    let value = serde_json::to_vec(&trace.value)?;

    let mut transaction = sqlite.begin().await?;

    let trace_id: i64 =
        sqlx::query_scalar("insert into traces (key, value) values ($1, $2) returning id")
            .bind(key)
            .bind(value)
            .fetch_one(&mut *transaction)
            .await?;

    for (key, value_hash) in &trace.deps {
        let key = serde_json::to_vec(&key)?;
        // TODO: `u64` -> `blob`
        let _ = value_hash;
        let value_hash: Vec<u8> = vec![0x42];

        sqlx::query("insert into trace_deps (trace_id, key, value_hash) values ($1, $2, $3)")
            .bind(trace_id)
            .bind(key)
            .bind(value_hash)
            .execute(&mut *transaction)
            .await?;
    }

    transaction.commit().await?;

    Ok(())
}

// Represent changes to build system code by making all builds depend on binary as input!

// Need to represent volatile (i.e. uncached) tasks, such as querying compiler version.

// Build systems à la carte
// (https://www.microsoft.com/en-us/research/wp-content/uploads/2018/03/build-systems.pdf)
//
// * Scheduling algorithm: Suspending (§4.1.3)
// * Rebuilding strategy: Verifying or constructive traces (§4.2.2 or §4.2.3)
//
// Allows for dynamic/monadic dependencies. Suspending seems like the best fit for async Rust, and
// therefore likely the most ergonomic to write.

/*

type Rebuilder c i k v = k -> v -> Task c k v -> Task (MonadState i) k v

type Scheduler c i j k v = Rebuilder c j k v -> Build c i k v

type Build c i k v = Tasks c k v -> k -> Store i k v -> Store i k v

data Store i k v = Store { info :: i, values :: k -> v }

type Tasks c k v = k -> Maybe (Task c k v)

type Task c k v = forall f. c f => (k -> f v) -> f v

shake :: (Ord k, Hashable v) => Build Monad (VT k v) k v
shake = suspending vtRebuilder

suspending :: forall i k v. Ord k => Scheduler Monad i i k v

vtRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (VT k v) k v

newtype VT k v = VT [Trace k v (Hash v)]

---

-- A request
data Key

-- A response
data Value

type Rebuilder = Key -> Value -> Task -> Task'

type Scheduler = Rebuilder -> Build

type Build = (Key -> Maybe Task) -> Key -> Store -> Store

data Store = Store { info :: [Trace], values :: Map Key Value }

type Task = forall f. Monad f => (Key -> f Value) -> f Value

type Task' = forall f. MonadState [Trace] f => (Key -> f Value) -> f Value

data Trace = Trace
  { key :: Key
  , depends :: [(Key, Hash)]
  , result  :: Hash
  }

*/

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_sqlite_migrate() -> anyhow::Result<()> {
        let sqlite = SqlitePool::connect(":memory:").await?;
        sqlite_migrate(&sqlite).await?;
        Ok(())
    }

    // NOTE: Generated by an LLM
    #[tokio::test]
    async fn test_store_roundtrip() -> anyhow::Result<()> {
        let sqlite = SqlitePool::connect(":memory:").await?;
        sqlite_migrate(&sqlite).await?;

        let original_store = Store {
            products: {
                let mut products = HashMap::new();
                products.insert(
                    Key::Which(String::from("cargo")),
                    Value::Path(Utf8PathBuf::from("/usr/bin/cargo")),
                );
                products.insert(
                    Key::ReadFile(Utf8PathBuf::from("Cargo.toml")),
                    Value::Bytes(Vec::from(b"[package]\nname = \"test\"")),
                );
                products
            },
            traces: vec![
                Trace {
                    key: Key::Format(Vec::from(b"fn main() { }")),
                    value: Value::Bytes(Vec::from(b"fn main() {}\n")),
                    deps: {
                        let mut deps = HashMap::new();
                        deps.insert(Key::Which(String::from("rustfmt")), Hash(12345));
                        deps
                    },
                },
                Trace {
                    key: Key::Lint(b"fn main() {}".to_vec()),
                    value: Value::Bytes(Vec::new()),
                    deps: {
                        let mut deps = HashMap::new();
                        deps.insert(Key::Which(String::from("clippy")), Hash(67890));
                        deps
                    },
                },
            ],
        };

        insert_store(&sqlite, original_store).await?;
        let retrieved_store = select_store(&sqlite).await?;

        assert_eq!(retrieved_store.products.len(), 2);
        assert_eq!(retrieved_store.traces.len(), 2);

        assert!(
            retrieved_store
                .products
                .contains_key(&Key::Which(String::from("cargo")))
        );
        assert!(
            retrieved_store
                .products
                .contains_key(&Key::ReadFile(Utf8PathBuf::from("Cargo.toml")))
        );

        let format_trace = retrieved_store
            .traces
            .iter()
            .find(|t| matches!(t.key, Key::Format(_)))
            .unwrap();
        assert_eq!(format_trace.deps.len(), 1);
        assert!(
            format_trace
                .deps
                .contains_key(&Key::Which(String::from("rustfmt")))
        );

        let lint_trace = retrieved_store
            .traces
            .iter()
            .find(|t| matches!(t.key, Key::Lint(_)))
            .unwrap();
        assert_eq!(lint_trace.deps.len(), 1);
        assert!(
            lint_trace
                .deps
                .contains_key(&Key::Which(String::from("clippy")))
        );

        Ok(())
    }
}
