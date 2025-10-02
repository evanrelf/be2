use crate::{
    build::{BuildSystem, Task, TaskContext},
    util::flatten,
};
use bytes::Bytes;
use camino::{Utf8Path, Utf8PathBuf};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::fs;
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

struct FormatSystem;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
enum Key {
    ReadFile(Arc<Utf8Path>),
    Fourmolu(Bytes),
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
enum Value {
    Bytes(Bytes),
}

async fn read_file(
    cx: Arc<TaskContext<FormatSystem>>,
    path: impl AsRef<Utf8Path>,
) -> anyhow::Result<Bytes> {
    let key = Key::ReadFile(Arc::from(path.as_ref()));
    let value = cx.realize(key).await?;
    #[expect(irrefutable_let_patterns)]
    let Value::Bytes(bytes) = value else {
        unreachable!()
    };
    Ok(bytes)
}

async fn task_read_file(
    _cx: Arc<TaskContext<FormatSystem>>,
    path: impl AsRef<Utf8Path>,
) -> anyhow::Result<Bytes> {
    let path = path.as_ref();
    let bytes = fs::read(&path).await?;
    Ok(Bytes::from(bytes))
}

async fn fourmolu(cx: Arc<TaskContext<FormatSystem>>, bytes: Bytes) -> anyhow::Result<Bytes> {
    let key = Key::Fourmolu(bytes);
    let value = cx.realize(key).await?;
    #[expect(irrefutable_let_patterns)]
    let Value::Bytes(bytes) = value else {
        unreachable!()
    };
    Ok(bytes)
}

#[expect(clippy::unused_async)]
async fn task_fourmolu(
    _cx: Arc<TaskContext<FormatSystem>>,
    _bytes: Bytes,
) -> anyhow::Result<Bytes> {
    todo!()
}

impl BuildSystem for FormatSystem {
    type Key = Key;
    type Value = Value;
    fn tasks(cx: Arc<TaskContext<FormatSystem>>, key: Key) -> Task<Value> {
        match key {
            Key::ReadFile(path) => Box::pin(async move {
                let bytes = task_read_file(cx, path).await?;
                let value = Value::Bytes(bytes);
                let volatile = true;
                Ok((value, volatile))
            }),
            Key::Fourmolu(bytes) => Box::pin(async move {
                let bytes = task_fourmolu(cx, bytes).await?;
                let value = Value::Bytes(bytes);
                let volatile = false;
                Ok((value, volatile))
            }),
        }
    }
}
