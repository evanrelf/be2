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

    let sqlite_path = cache_dir.join("cache.sqlite");

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
