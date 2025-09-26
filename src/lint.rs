use crate::cli::lint::Args;

#[expect(clippy::unused_async)]
pub async fn run(_args: Args) -> anyhow::Result<()> {
    tracing::info!("totally linting right now...");

    Ok(())
}
