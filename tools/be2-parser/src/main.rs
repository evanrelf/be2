mod common;
mod haskell;

use camino::Utf8PathBuf;
use clap::Parser as _;
use std::{fs, io};

#[derive(clap::ValueEnum, Clone)]
enum Language {
    Haskell,
}

#[derive(clap::Parser)]
struct Args {
    path: Option<Utf8PathBuf>,
    #[arg(long)]
    lang: Language,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let input = if let Some(path) = &args.path {
        fs::read_to_string(path)?
    } else {
        io::read_to_string(io::stdin())?
    };
    let input: &'static str = Box::leak(Box::new(input));
    match &args.lang {
        Language::Haskell => {
            let output = haskell::parse(input)?;
            serde_json::to_writer(io::stdout(), &output)?;
        }
    }
    Ok(())
}
