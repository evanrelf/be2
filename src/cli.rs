#[derive(clap::Parser)]
#[command(disable_help_subcommand = true)]
pub struct Args {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(clap::Subcommand)]
pub enum Command {
    /// Format code
    Format(format::Args),

    /// Lint code
    Lint(lint::Args),

    /// Delete cache
    Clean,
}

pub mod format {
    use camino::Utf8PathBuf;

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
}

pub mod lint {
    use camino::Utf8PathBuf;

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
}
