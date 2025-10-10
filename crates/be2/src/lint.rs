use crate::util::flatten;
use camino::Utf8PathBuf;
use tracing::Instrument as _;

#[derive(clap::Args)]
pub struct Args {
    #[command(subcommand)]
    pub command: Option<Command>,
}

#[derive(clap::Subcommand)]
pub enum Command {
    /// Lint Haskell code
    Haskell(HaskellArgs),
}

#[derive(clap::Args, Default)]
pub struct HaskellArgs {
    /// Only lint specific paths
    #[arg(group = "input")]
    pub paths: Vec<Utf8PathBuf>,

    /// Lint code piped to `stdin`
    #[arg(long, group = "input")]
    pub stdin: bool,
}

pub async fn run(args: &Args) -> anyhow::Result<()> {
    match &args.command {
        Some(Command::Haskell(args)) => run_haskell(args).await?,
        None => {
            let haskell = tokio::spawn(
                async {
                    let args = HaskellArgs::default();
                    run_haskell(&args).await
                }
                .in_current_span(),
            );
            flatten(haskell).await?;
        }
    }

    Ok(())
}

#[expect(clippy::unused_async)]
pub async fn run_haskell(_args: &HaskellArgs) -> anyhow::Result<()> {
    todo!()
}
