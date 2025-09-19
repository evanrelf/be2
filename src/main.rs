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
