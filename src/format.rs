use crate::{
    cli::format::{Args, Command, HaskellArgs, NixArgs},
    util::flatten,
};

pub async fn run(args: &Args) -> anyhow::Result<()> {
    match &args.command {
        Some(Command::Haskell(args)) => run_haskell(args).await?,
        Some(Command::Nix(args)) => run_nix(args).await?,
        None => {
            let haskell = tokio::spawn(async {
                let args = HaskellArgs::default();
                run_haskell(&args).await
            });
            let nix = tokio::spawn(async {
                let args = NixArgs::default();
                run_nix(&args).await
            });
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
