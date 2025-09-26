#![allow(dead_code)]

mod build2;
mod cli;
mod db;
mod format;
mod lint;
mod old_build;
mod util;

use crate::cli::{Args, Command};
use camino::Utf8PathBuf;
use clap::Parser as _;
use etcetera::app_strategy::{AppStrategy as _, AppStrategyArgs, Xdg};
use tokio::fs;

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

    let db_path = cache_dir.join("cache.sqlite");

    match &args.command {
        Command::Format(args) => format::run(args).await?,
        Command::Lint(args) => lint::run(args).await?,
        Command::Clean => {
            if fs::try_exists(&db_path).await? {
                fs::remove_file(&db_path).await?;
            }
        }
    }

    Ok(())
}
