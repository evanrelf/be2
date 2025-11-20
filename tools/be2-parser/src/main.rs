mod common;
mod haskell;

use crate::common::{node_text, query_structured};
use camino::Utf8PathBuf;
use clap::Parser as _;
use std::{collections::HashMap, fs, io};

#[derive(clap::ValueEnum, Clone)]
enum Language {
    Haskell,
}

#[derive(clap::Parser)]
struct Args {
    #[arg(long)]
    lang: Language,
    #[arg(long)]
    query: Option<String>,
    path: Option<Utf8PathBuf>,
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
            let cx = haskell::init(input)?;
            if let Some(query) = &args.query {
                let match_maps = query_structured(&cx, query)?;
                let mut output = Vec::with_capacity(match_maps.len());
                for match_map in match_maps {
                    let mut entry = HashMap::with_capacity(match_map.len());
                    for (capture_name, nodes) in match_map {
                        let texts: Vec<_> = nodes.iter()
                            .map(|node| node_text(&cx, node))
                            .collect();
                        entry.insert(capture_name, texts);
                    }
                    output.push(entry);
                }
                serde_json::to_writer(io::stdout(), &output)?;
            } else {
                let output = haskell::parse(&cx)?;
                serde_json::to_writer(io::stdout(), &output)?;
            }
        }
    }
    Ok(())
}
