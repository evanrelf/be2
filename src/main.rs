use clap::Parser as _;
use etcetera::app_strategy::{AppStrategy as _, AppStrategyArgs, Xdg};
use tokio::fs;

#[derive(clap::Parser)]
struct Args {}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let _args = Args::parse();

    let xdg = Xdg::new(AppStrategyArgs {
        top_level_domain: String::from("com"),
        author: String::from("Evan Relf"),
        app_name: String::from("Be"),
    })?;

    let xdg_cache_dir = xdg.cache_dir();

    fs::create_dir_all(&xdg_cache_dir).await?;

    tracing_subscriber::fmt::init();

    tracing::info!("Hello, world!");

    Ok(())
}
