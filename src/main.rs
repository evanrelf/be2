use clap::Parser as _;

#[derive(clap::Parser)]
struct Args {}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let _args = Args::parse();

    tracing_subscriber::fmt::init();

    tracing::info!("Hello, world!");

    Ok(())
}
