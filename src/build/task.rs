use crate::{
    build::system::{BuildContext, Key, Value},
    util::flatten,
};
use bytes::Bytes;
use camino::{Utf8Path, Utf8PathBuf};
use std::{str, sync::Arc};
use tokio::{fs, process::Command};

pub async fn which(cx: Arc<BuildContext>, name: &str) -> anyhow::Result<Arc<Utf8Path>> {
    let key = Key::Which(Arc::from(name));
    let value = cx.build(key).await?;
    let Value::Path(path) = value else {
        unreachable!()
    };
    Ok(path)
}

pub async fn task_which(_cx: Arc<BuildContext>, name: &str) -> anyhow::Result<Arc<Utf8Path>> {
    let output = Command::new("which").arg(name).output().await?;

    if !output.status.success() {
        anyhow::bail!("Failed to run `which`");
    }

    let string = str::from_utf8(&output.stdout)?.trim();

    let path = Arc::from(Utf8Path::new(string));

    Ok(path)
}

#[expect(clippy::unused_async)]
pub async fn task_which_stub(_cx: Arc<BuildContext>, name: &str) -> anyhow::Result<Arc<Utf8Path>> {
    let path = match name {
        "sh" => Arc::from(Utf8Path::new("/bin/sh")),
        "vim" => Arc::from(Utf8Path::new("/usr/bin/vim")),
        _ => anyhow::bail!("Failed to find path to `{name}`"),
    };

    Ok(path)
}

pub async fn read_file(cx: Arc<BuildContext>, path: impl AsRef<Utf8Path>) -> anyhow::Result<Bytes> {
    let key = Key::ReadFile(Arc::from(path.as_ref()));
    let value = cx.build(key).await?;
    let Value::Bytes(bytes) = value else {
        unreachable!()
    };
    Ok(bytes)
}

pub async fn task_read_file(_cx: Arc<BuildContext>, path: &Utf8Path) -> anyhow::Result<Bytes> {
    let bytes = fs::read(&path).await?;
    Ok(Bytes::from(bytes))
}

#[expect(clippy::unused_async)]
pub async fn task_read_file_stub(_cx: Arc<BuildContext>, path: &Utf8Path) -> anyhow::Result<Bytes> {
    let bytes = match path.as_str() {
        "/files" => Vec::from(b"/files/a\n/files/a\n/files/b\n"),
        "/files/a" => Vec::from(b"AAAA\n"),
        "/files/b" => Vec::from(b"BBBB\n"),
        "/dev/null" => Vec::new(),
        _ => anyhow::bail!("Failed to read file at '{path}'"),
    };
    Ok(Bytes::from(bytes))
}

pub async fn concat(cx: Arc<BuildContext>, path: &Utf8Path) -> anyhow::Result<Bytes> {
    let key = Key::Concat(Arc::from(path));
    let value = cx.build(key).await?;
    let Value::Bytes(path) = value else {
        unreachable!()
    };
    Ok(path)
}

pub async fn task_concat(cx: Arc<BuildContext>, path: &Utf8Path) -> anyhow::Result<Bytes> {
    let paths = {
        let bytes = read_file(cx.clone(), path).await?;
        let string = str::from_utf8(&bytes)?;
        string.lines().map(Utf8PathBuf::from).collect::<Vec<_>>()
    };

    let mut handles = Vec::with_capacity(paths.len());

    for path in paths {
        let handle = tokio::spawn(read_file(cx.clone(), path));
        handles.push(handle);
    }

    let mut output = Vec::new();

    for handle in handles {
        let bytes = flatten(handle).await?;
        output.extend(bytes);
    }

    Ok(Bytes::from(output))
}
