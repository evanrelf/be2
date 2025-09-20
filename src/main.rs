#![allow(dead_code)]
#![allow(clippy::unused_async)]

use camino::Utf8PathBuf;
use clap::Parser as _;
use etcetera::app_strategy::{AppStrategy as _, AppStrategyArgs, Xdg};
use sqlx::{
    SqlitePool,
    sqlite::{SqliteConnectOptions, SqliteJournalMode, SqliteSynchronous},
};
use std::str::FromStr as _;
use tokio::fs;

#[derive(clap::Parser)]
struct Args {}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let _args = Args::parse();

    tracing_subscriber::fmt::init();

    let xdg = Xdg::new(AppStrategyArgs {
        top_level_domain: String::from("com"),
        author: String::from("Evan Relf"),
        app_name: String::from("Be2"),
    })?;

    let cache_dir = Utf8PathBuf::try_from(xdg.cache_dir())?;

    fs::create_dir_all(&cache_dir).await?;

    let sqlite_path = cache_dir.join("cache.sqlite");

    let _sqlite = SqlitePool::connect_with(
        SqliteConnectOptions::from_str(&format!("sqlite://{sqlite_path}"))?
            .create_if_missing(true)
            .journal_mode(SqliteJournalMode::Wal)
            .synchronous(SqliteSynchronous::Normal),
    )
    .await?;

    tracing::info!("Hello, world!");

    Ok(())
}

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

use std::collections::HashMap;

#[derive(PartialEq)]
struct Hash(u64);

struct Trace {
    key: Key,
    depends: HashMap<Key, Hash>,
    result: Hash,
}

#[derive(PartialEq)]
enum Key {
    Which(String),
    ReadFile(Utf8PathBuf),
    Format(Vec<u8>),
    Lint(Vec<u8>),
}

enum Value {
    Path(Utf8PathBuf),
    Bytes(Vec<u8>),
}

impl Value {
    fn hash(&self) -> Hash {
        todo!()
    }
}

#[derive(Default)]
struct Store {
    traces: Vec<Trace>,
    values: HashMap<Key, Value>,
}

impl Store {
    fn new() -> Store {
        Self::default()
    }

    // `recordVT`
    fn trace(&mut self, trace: Trace) {
        self.traces.push(trace);
    }

    // TODO: Should `fetch_hash` be implicitly available?
    // `vertifyVT`
    fn is_fresh(&self, key: &Key, hash: &Hash, fetch_hash: fn(&Key) -> Hash) -> bool {
        self.traces.iter().any(|trace| {
            if *key != trace.key || *hash != trace.result {
                return false;
            }
            trace.depends.iter().all(|(dep_key, dep_hash)| {
                let current_hash = fetch_hash(dep_key);
                *dep_hash == current_hash
            })
        })
    }
}
