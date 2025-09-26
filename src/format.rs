use crate::cli::format::Args;

#[expect(clippy::unused_async)]
pub async fn run(_args: Args) -> anyhow::Result<()> {
    tracing::info!("totally formatting right now...");

    Ok(())
}
