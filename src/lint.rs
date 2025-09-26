use crate::cli::lint::Args;

pub async fn run(_args: Args) -> anyhow::Result<()> {
    tracing::info!("totally linting right now...");

    Ok(())
}
