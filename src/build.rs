use camino::{Utf8Path, Utf8PathBuf};
use tokio::fs;

enum Key {
    Which(String),
    ReadFile(Utf8PathBuf),
}

enum Value {
    Path(Utf8PathBuf),
    Bytes(Vec<u8>),
}

async fn read_file(path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let bytes = fs::read(&path).await?;
    Ok(bytes)
}

/// Given the path to a file containing newline separated paths, concatenate the contents of the
/// files at those paths.
async fn concat(path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let paths = {
        let bytes = read_file(path).await?;
        let string = str::from_utf8(&bytes)?;
        string.lines().map(Utf8PathBuf::from).collect::<Vec<_>>()
    };

    let mut output = Vec::new();

    for path in paths {
        let bytes = read_file(&path).await?;
        output.extend(bytes);
    }

    Ok(output)
}
