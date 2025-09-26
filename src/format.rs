use crate::cli::format::Args;

pub async fn run(_args: Args) -> anyhow::Result<()> {
    tracing::info!("totally formatting right now...");

    Ok(())
}
