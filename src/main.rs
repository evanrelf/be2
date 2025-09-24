#![allow(dead_code)]

mod build;
mod db;

use camino::Utf8PathBuf;
use clap::Parser as _;
use etcetera::app_strategy::{AppStrategy as _, AppStrategyArgs, Xdg};
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
            let sqlite = db::connect(&sqlite_path).await?;
            db::migrate(&sqlite).await?;

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
