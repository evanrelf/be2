pub mod format;

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

    /// Delete cache
    Clean,
}
