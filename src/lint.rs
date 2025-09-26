use crate::{
    cli::lint::{Args, Command, HaskellArgs},
    util::flatten,
};

pub async fn run(args: &Args) -> anyhow::Result<()> {
    match &args.command {
        Some(Command::Haskell(args)) => run_haskell(args).await?,
        None => {
            let haskell = tokio::spawn(async {
                let args = HaskellArgs::default();
                run_haskell(&args).await
            });
            flatten(haskell).await?;
        }
    }

    Ok(())
}

#[expect(clippy::unused_async)]
pub async fn run_haskell(_args: &HaskellArgs) -> anyhow::Result<()> {
    todo!()
}
