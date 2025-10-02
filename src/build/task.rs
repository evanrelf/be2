use crate::{
    build::{TaskContext, TestKey, TestValue},
    util::flatten,
};
use bytes::Bytes;
use camino::{Utf8Path, Utf8PathBuf};
use std::{str, sync::Arc};
use tokio::fs;
use tracing::Instrument as _;

pub async fn read_file(cx: Arc<TaskContext>, path: impl AsRef<Utf8Path>) -> anyhow::Result<Bytes> {
    let key = TestKey::ReadFile(Arc::from(path.as_ref()));
    let value = cx.realize(key).await?;
    #[expect(irrefutable_let_patterns)]
    let TestValue::Bytes(bytes) = value else {
        unreachable!()
    };
    Ok(bytes)
}

pub async fn task_read_file(
    cx: Arc<TaskContext>,
    path: impl AsRef<Utf8Path>,
) -> anyhow::Result<Bytes> {
    let path = path.as_ref();
    if cx.use_stubs() {
        let bytes = match path.as_str() {
            "/files" => Bytes::from("/files/a\n/files/a\n/files/b\n"),
            "/files/a" => Bytes::from("AAAA\n"),
            "/files/b" => Bytes::from("BBBB\n"),
            "/dev/null" => Bytes::new(),
            _ => anyhow::bail!("Failed to read file at '{path}'"),
        };
        Ok(bytes)
    } else {
        let bytes = fs::read(&path).await?;
        Ok(Bytes::from(bytes))
    }
}

pub async fn concat(cx: Arc<TaskContext>, path: impl AsRef<Utf8Path>) -> anyhow::Result<Bytes> {
    let key = TestKey::Concat(Arc::from(path.as_ref()));
    let value = cx.realize(key).await?;
    #[expect(irrefutable_let_patterns)]
    let TestValue::Bytes(path) = value else {
        unreachable!()
    };
    Ok(path)
}

pub async fn task_concat(
    cx: Arc<TaskContext>,
    path: impl AsRef<Utf8Path>,
) -> anyhow::Result<Bytes> {
    let path = path.as_ref();
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
