use clap::Parser as _;

#[derive(clap::Parser)]
struct Args {}

#[tokio::main]
async fn main() {
    let _args = Args::parse();

    println!("Hello, world!");
}
