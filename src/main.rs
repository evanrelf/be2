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

struct Key;

struct Time;

macro_rules! need {
    ($expr:expr) => {{ $expr }};
}

struct Output<T> {
    created: Key,
    time: Time,
    dependencies: Vec<Key>,
    value: T,
}

#[derive(Debug)]
enum Person {
    Evan,
}

#[derive(Debug)]
enum Language {
    English,
    Spanish,
}

async fn task_person() -> Person {
    Person::Evan
}

async fn task_language(person: &Person) -> Language {
    match person {
        Person::Evan => Language::English,
    }
}

async fn task_greeting() -> String {
    let person = need!(task_person().await);
    let language = task_language(&person).await;

    let name = match person {
        Person::Evan => "Evan",
    };

    match language {
        Language::English => format!("Hello, {name}!"),
        Language::Spanish => format!("¡Hola, {name}!"),
    }
}
