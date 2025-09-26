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
    Format,
    Clean,
}

struct Env {
    args: Args,
    xdg: Xdg,
    db_path: Utf8PathBuf,
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

    let db_path = cache_dir.join("cache.sqlite");

    let env = Env { args, xdg, db_path };

    match env.args.command {
        Command::Format => run_format(env).await?,
        Command::Clean => run_clean(env).await?,
    }

    Ok(())
}

async fn run_format(env: Env) -> anyhow::Result<()> {
    let db = db::connect(&env.db_path).await?;
    db::migrate(&db).await?;

    tracing::info!("totally formatting right now...");

    Ok(())
}

async fn run_clean(env: Env) -> anyhow::Result<()> {
    if fs::try_exists(&env.db_path).await? {
        fs::remove_file(&env.db_path).await?;
    }

    Ok(())
}
