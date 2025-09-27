use crate::build::{Context, Key, Value};
use bytes::Bytes;
use camino::{Utf8Path, Utf8PathBuf};
use std::{str, sync::Arc};
use tokio::{fs, process::Command};

pub async fn which(cx: &mut Context, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let Value::Path(bytes) = cx.build(&Key::Which(Arc::from(name))).await? else {
        unreachable!()
    };
    Ok(bytes)
}

pub async fn task_which(_cx: &mut Context, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let output = Command::new("which").arg(name).output().await?;

    if !output.status.success() {
        anyhow::bail!("Failed to run `which`");
    }

    let string = str::from_utf8(&output.stdout)?.trim();

    let path = Utf8PathBuf::from(string);

    Ok(path)
}

#[expect(clippy::unused_async)]
pub async fn task_which_stub(_cx: &mut Context, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let path = match name {
        "sh" => Utf8PathBuf::from("/bin/sh"),
        "vim" => Utf8PathBuf::from("/usr/bin/vim"),
        _ => anyhow::bail!("Failed to find path to `{name}`"),
    };

    Ok(path)
}

pub async fn read_file(cx: &mut Context, path: impl AsRef<Utf8Path>) -> anyhow::Result<Bytes> {
    let path = path.as_ref();
    let Value::Bytes(bytes) = cx.build(&Key::ReadFile(path.to_owned())).await? else {
        unreachable!()
    };
    Ok(bytes)
}

pub async fn task_read_file(_cx: &mut Context, path: &Utf8Path) -> anyhow::Result<Bytes> {
    let bytes = fs::read(&path).await?;
    Ok(Bytes::from(bytes))
}

#[expect(clippy::unused_async)]
pub async fn task_read_file_stub(_cx: &mut Context, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let bytes = match path.as_str() {
        "/files" => Vec::from(b"/files/a\n/files/a\n/files/b\n"),
        "/files/a" => Vec::from(b"AAAA\n"),
        "/files/b" => Vec::from(b"BBBB\n"),
        "/dev/null" => Vec::new(),
        _ => anyhow::bail!("Failed to read file at '{path}'"),
    };
    Ok(bytes)
}

pub async fn concat(cx: &mut Context, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let paths = {
        let bytes = read_file(cx, path).await?;
        let string = str::from_utf8(&bytes)?;
        string.lines().map(Utf8PathBuf::from).collect::<Vec<_>>()
    };

    let mut output = Vec::new();

    for path in paths {
        let bytes = read_file(cx, path).await?;
        output.extend(bytes);
    }

    Ok(output)
}
