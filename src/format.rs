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
    /// Format Haskell code
    Haskell(HaskellArgs),

    /// Format Nix code
    Nix(NixArgs),
}

#[derive(clap::Args, Default)]
pub struct HaskellArgs {
    /// Only format specific paths
    #[arg(group = "input")]
    pub paths: Vec<Utf8PathBuf>,

    /// Format code piped to `stdin`
    #[arg(long, group = "input")]
    pub stdin: bool,
}

#[derive(clap::Args, Default)]
pub struct NixArgs {
    /// Only format specific paths
    #[arg(group = "input")]
    pub paths: Vec<Utf8PathBuf>,

    /// Format code piped to `stdin`
    #[arg(long, group = "input")]
    pub stdin: bool,
}

pub async fn run(args: &Args) -> anyhow::Result<()> {
    match &args.command {
        Some(Command::Haskell(args)) => run_haskell(args).await?,
        Some(Command::Nix(args)) => run_nix(args).await?,
        None => {
            let haskell = tokio::spawn(
                async {
                    let args = HaskellArgs::default();
                    run_haskell(&args).await
                }
                .in_current_span(),
            );
            let nix = tokio::spawn(
                async {
                    let args = NixArgs::default();
                    run_nix(&args).await
                }
                .in_current_span(),
            );
            tokio::try_join!(flatten(haskell), flatten(nix))?;
        }
    }

    Ok(())
}

#[expect(clippy::unused_async)]
pub async fn run_haskell(_args: &HaskellArgs) -> anyhow::Result<()> {
    todo!()
}

#[expect(clippy::unused_async)]
pub async fn run_nix(_args: &NixArgs) -> anyhow::Result<()> {
    todo!()
}
