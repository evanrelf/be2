use crate::db;
use bytes::Bytes;
use camino::{Utf8Path, Utf8PathBuf};
use serde::{Deserialize, Serialize};
use sqlx::SqlitePool;
use std::{
    collections::{HashMap, HashSet},
    hash::BuildHasherDefault,
    str,
};
use tokio::{fs, process::Command};
use twox_hash::XxHash3_64;

// TODO: Make `Key` and `Value` types cheap to clone.

#[derive(Deserialize, Serialize)]
enum Key {
    Which(String),
    File(Utf8PathBuf),
}

#[derive(Deserialize, Serialize)]
enum Value {
    Path(Utf8PathBuf),
    Bytes(Vec<u8>),
}

struct Trace {
    key: Key,
    deps: HashMap<Key, u64, BuildHasherDefault<XxHash3_64>>,
    value: Value,
}

impl TryFrom<Trace> for db::Trace {
    type Error = anyhow::Error;
    fn try_from(trace: Trace) -> Result<Self, Self::Error> {
        let mut key_buf = Vec::new();
        ciborium::into_writer(&trace.key, &mut key_buf)?;
        let key = Bytes::from(key_buf);

        let mut deps = HashMap::default();
        for (dep_key, dep_value_hash) in trace.deps {
            let mut dep_key_buf = Vec::new();
            ciborium::into_writer(&dep_key, &mut dep_key_buf)?;
            let dep_key = Bytes::from(dep_key_buf);
            deps.insert(dep_key, dep_value_hash);
        }

        let mut value_buf = Vec::new();
        ciborium::into_writer(&trace.value, &mut value_buf)?;
        let value = Bytes::from(value_buf);

        Ok(db::Trace { key, deps, value })
    }
}

struct Context {
    db: SqlitePool,
    done: HashSet<Key>,
    store: HashMap<Key, Value>,
}

impl Context {
    async fn build(&mut self, _key: &Key) -> anyhow::Result<Value> {
        todo!()
    }

    async fn record(&mut self, trace: Trace) -> anyhow::Result<()> {
        let db_trace = db::Trace::try_from(trace)?;
        db::insert_trace(&self.db, &db_trace).await?;
        Ok(())
    }

    async fn construct(&mut self, _key: &Key) -> anyhow::Result<HashSet<Value>> {
        todo!()
    }
}

async fn which(cx: &mut Context, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let Value::Path(bytes) = cx.build(&Key::Which(name.to_owned())).await? else {
        unreachable!()
    };
    Ok(bytes)
}

async fn task_which(_cx: &mut Context, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let output = Command::new("which").arg(name).output().await?;

    if !output.status.success() {
        anyhow::bail!("Failed to run `which`");
    }

    let string = str::from_utf8(&output.stdout)?.trim();

    let path = Utf8PathBuf::from(string);

    Ok(path)
}

#[expect(clippy::unused_async)]
async fn task_which_stub(_cx: &mut Context, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let path = match name {
        "sh" => Utf8PathBuf::from("/bin/sh"),
        "vim" => Utf8PathBuf::from("/usr/bin/vim"),
        _ => anyhow::bail!("Failed to find path to `{name}`"),
    };

    Ok(path)
}

async fn read_file(cx: &mut Context, path: impl AsRef<Utf8Path>) -> anyhow::Result<Vec<u8>> {
    let path = path.as_ref();
    let Value::Bytes(bytes) = cx.build(&Key::File(path.to_owned())).await? else {
        unreachable!()
    };
    Ok(bytes)
}

async fn task_read_file(_cx: &mut Context, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let bytes = fs::read(&path).await?;
    Ok(bytes)
}

#[expect(clippy::unused_async)]
async fn task_read_file_stub(_cx: &mut Context, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let bytes = match path.as_str() {
        "/files" => Vec::from(b"/files/a\n/files/a\n/files/b\n"),
        "/files/a" => Vec::from(b"AAAA\n"),
        "/files/b" => Vec::from(b"BBBB\n"),
        "/dev/null" => Vec::new(),
        _ => anyhow::bail!("Failed to read file at '{path}'"),
    };
    Ok(bytes)
}

async fn concat(cx: &mut Context, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
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
