use crate::build::{BuildSystem, TaskContext, TaskFut};
use bytes::Bytes;
use camino::{Utf8Path, Utf8PathBuf};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::fs;

enum Be {}

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
enum Key {
    ReadFile(Arc<Utf8Path>),
    Canonicalize(Arc<Utf8Path>),
    Which(Arc<str>),
    Fourmolu {
        path: Option<Arc<Utf8Path>>,
        bytes: Bytes,
    },
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
enum Value {
    Bytes(Bytes),
    Path(Arc<Utf8Path>),
}

impl BuildSystem for Be {
    type Key = Key;

    type Value = Value;

    fn tasks(cx: Arc<TaskContext<Self>>, key: Key) -> TaskFut<Value> {
        match key {
            Key::ReadFile(path) => Box::pin(async move {
                let bytes = task_read_file(cx, path).await?;
                let value = Value::Bytes(bytes);
                let volatile = true;
                Ok((value, volatile))
            }),
            Key::Canonicalize(path) => Box::pin(async move {
                let path = task_canonicalize(cx, path).await?;
                let value = Value::Path(path);
                let volatile = true;
                Ok((value, volatile))
            }),
            Key::Which(name) => Box::pin(async move {
                let path = task_which(cx, name).await?;
                let value = Value::Path(path);
                let volatile = true;
                Ok((value, volatile))
            }),
            Key::Fourmolu { path, bytes } => Box::pin(async move {
                let bytes = task_fourmolu(cx, path, bytes).await?;
                let value = Value::Bytes(bytes);
                let volatile = false;
                Ok((value, volatile))
            }),
        }
    }
}

async fn read_file(cx: Arc<TaskContext<Be>>, path: impl AsRef<Utf8Path>) -> anyhow::Result<Bytes> {
    let key = Key::ReadFile(Arc::from(path.as_ref()));
    let value = cx.realize(key).await?;
    let Value::Bytes(bytes) = value else {
        unreachable!()
    };
    Ok(bytes)
}

async fn task_read_file(
    _cx: Arc<TaskContext<Be>>,
    path: impl AsRef<Utf8Path>,
) -> anyhow::Result<Bytes> {
    let path = path.as_ref();
    let bytes = fs::read(&path).await?;
    Ok(Bytes::from(bytes))
}

async fn canonicalize(
    cx: Arc<TaskContext<Be>>,
    path: impl AsRef<Utf8Path>,
) -> anyhow::Result<Arc<Utf8Path>> {
    let key = Key::Canonicalize(Arc::from(path.as_ref()));
    let value = cx.realize(key).await?;
    let Value::Path(path) = value else {
        unreachable!()
    };
    Ok(path)
}

async fn task_canonicalize(
    _cx: Arc<TaskContext<Be>>,
    path: impl AsRef<Utf8Path>,
) -> anyhow::Result<Arc<Utf8Path>> {
    let path = fs::canonicalize(path.as_ref()).await?;
    let path = Utf8PathBuf::try_from(path).unwrap();
    let path = Arc::from(path.as_path());
    Ok(path)
}

async fn which(cx: Arc<TaskContext<Be>>, name: impl AsRef<str>) -> anyhow::Result<Arc<Utf8Path>> {
    let key = Key::Which(Arc::from(name.as_ref()));
    let value = cx.realize(key).await?;
    let Value::Path(path) = value else {
        unreachable!()
    };
    Ok(path)
}

#[expect(clippy::unused_async)]
async fn task_which(
    _cx: Arc<TaskContext<Be>>,
    _name: impl AsRef<str>,
) -> anyhow::Result<Arc<Utf8Path>> {
    todo!()
}

async fn fourmolu(
    cx: Arc<TaskContext<Be>>,
    path: Option<Arc<Utf8Path>>,
    bytes: Bytes,
) -> anyhow::Result<Bytes> {
    let key = Key::Fourmolu { path, bytes };
    let value = cx.realize(key).await?;
    let Value::Bytes(bytes) = value else {
        unreachable!()
    };
    Ok(bytes)
}

#[expect(clippy::unused_async)]
async fn task_fourmolu(
    _cx: Arc<TaskContext<Be>>,
    _path: Option<Arc<Utf8Path>>,
    _bytes: Bytes,
) -> anyhow::Result<Bytes> {
    todo!()
}
