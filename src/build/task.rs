use crate::{
    build::{BuildContext, Key, Value},
    util::flatten,
};
use bytes::Bytes;
use camino::{Utf8Path, Utf8PathBuf};
use std::{
    str,
    sync::{Arc, atomic::Ordering},
};
use tokio::fs;
use tracing::Instrument as _;

pub async fn read_file(cx: Arc<BuildContext>, path: impl AsRef<Utf8Path>) -> anyhow::Result<Bytes> {
    let key = Key::ReadFile(Arc::from(path.as_ref()));
    let value = cx.realize(key).await?;
    #[expect(irrefutable_let_patterns)]
    let Value::Bytes(bytes) = value else {
        unreachable!()
    };
    Ok(bytes)
}

pub async fn task_read_file(cx: Arc<BuildContext>, path: &Utf8Path) -> anyhow::Result<Bytes> {
    if cx.debug_use_stubs.load(Ordering::SeqCst) {
        let bytes = match path.as_str() {
            "/files" => Vec::from(b"/files/a\n/files/a\n/files/b\n"),
            "/files/a" => Vec::from(b"AAAA\n"),
            "/files/b" => Vec::from(b"BBBB\n"),
            "/dev/null" => Vec::new(),
            _ => anyhow::bail!("Failed to read file at '{path}'"),
        };
        Ok(Bytes::from(bytes))
    } else {
        let bytes = fs::read(&path).await?;
        Ok(Bytes::from(bytes))
    }
}

pub async fn concat(cx: Arc<BuildContext>, path: impl AsRef<Utf8Path>) -> anyhow::Result<Bytes> {
    let key = Key::Concat(Arc::from(path.as_ref()));
    let value = cx.realize(key).await?;
    #[expect(irrefutable_let_patterns)]
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
        let handle = tokio::spawn(read_file(cx.clone(), path).in_current_span());
        handles.push(handle);
    }

    let mut output = Vec::new();

    for handle in handles {
        let bytes = flatten(handle).await?;
        output.extend(bytes);
    }

    Ok(Bytes::from(output))
}
