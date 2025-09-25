use camino::{Utf8Path, Utf8PathBuf};
use std::{
    collections::HashMap,
    str,
    sync::atomic::{AtomicUsize, Ordering},
};
use tokio::{fs, process::Command};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Key {
    Which(String),
    ReadFile(Utf8PathBuf),
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Path(Utf8PathBuf),
    Bytes(Vec<u8>),
}

impl Value {
    fn hash(&self) -> Hash {
        todo!()
    }
}

#[derive(PartialEq)]
struct Hash(u64);

struct Trace {
    key: Key,
    value: Value,
    deps: HashMap<Key, Hash>,
}

#[derive(Default)]
struct Store {
    products: papaya::HashMap<Key, Value>,
    traces: boxcar::Vec<Trace>,
}

/// Global context used for the duration of the build. Stores build products, constructive traces,
/// etc.
#[derive(Default)]
struct BuildCtx {
    debug_task_counter: AtomicUsize,
    store: Store,
}

impl BuildCtx {
    fn new() -> Self {
        Self::default()
    }

    /// Get the value associated with the given key. Either retrieves a cached value from the cache,
    /// or kicks off a task to produce the value.
    async fn fetch(&self, key: &Key) -> anyhow::Result<Value> {
        if let Some(value) = self.store.products.pin().get(key) {
            return Ok(value.clone());
        }
        let value = match key {
            Key::Which(name) => {
                // let path = task_which(self, name).await?;
                let path = task_which_stub(self, name).await?;
                Value::Path(path)
            }
            Key::ReadFile(path) => {
                // let bytes = task_read_file(self, path).await?;
                let bytes = task_read_file_stub(self, path).await?;
                Value::Bytes(bytes)
            }
        };
        self.debug_task_counter.fetch_add(1, Ordering::SeqCst);
        self.store.products.pin().insert(key.clone(), value.clone());
        Ok(value)
    }

    // https://hackage.haskell.org/package/build-1.1/docs/src/Build.Trace.html#isDirtyCT
    fn is_dirty(&self, key: &Key) -> bool {
        let products = self.store.products.pin();
        for (_index, trace) in &self.store.traces {
            let key_match = trace.key == *key;
            let value_match = trace.value == *products.get(key).unwrap();
            let deps_match = trace
                .deps
                .iter()
                .all(|(dep_key, dep_hash)| products.get(dep_key).unwrap().hash() == *dep_hash);
            if key_match && value_match && deps_match {
                return false;
            }
        }
        true
    }

    // https://hackage.haskell.org/package/build-1.1/docs/src/Build.Trace.html#recordCT
    fn record(&self) {
        todo!()
    }

    // https://hackage.haskell.org/package/build-1.1/docs/src/Build.Trace.html#constructCT
    #[expect(clippy::unused_async)]
    async fn construct(&self, _key: &Key) -> anyhow::Result<Vec<Value>> {
        todo!()
    }
}

async fn which(ctx: &BuildCtx, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let Value::Path(bytes) = ctx.fetch(&Key::Which(name.to_owned())).await? else {
        unreachable!()
    };
    Ok(bytes)
}

async fn task_which(_ctx: &BuildCtx, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let output = Command::new("which").arg(name).output().await?;

    if !output.status.success() {
        anyhow::bail!("Failed to run `which`");
    }

    let string = str::from_utf8(&output.stdout)?.trim();

    let path = Utf8PathBuf::from(string);

    Ok(path)
}

#[expect(clippy::unused_async)]
async fn task_which_stub(_ctx: &BuildCtx, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let path = match name {
        "sh" => Utf8PathBuf::from("/bin/sh"),
        "vim" => Utf8PathBuf::from("/usr/bin/vim"),
        _ => anyhow::bail!("Failed to find path to `{name}`"),
    };

    Ok(path)
}

async fn read_file(ctx: &BuildCtx, path: impl AsRef<Utf8Path>) -> anyhow::Result<Vec<u8>> {
    let path = path.as_ref();
    let Value::Bytes(bytes) = ctx.fetch(&Key::ReadFile(path.to_owned())).await? else {
        unreachable!()
    };
    Ok(bytes)
}

async fn task_read_file(_ctx: &BuildCtx, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let bytes = fs::read(&path).await?;
    Ok(bytes)
}

#[expect(clippy::unused_async)]
async fn task_read_file_stub(_ctx: &BuildCtx, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let bytes = match path.as_str() {
        "/files" => Vec::from(b"/files/a\n/files/a\n/files/b\n"),
        "/files/a" => Vec::from(b"AAAA\n"),
        "/files/b" => Vec::from(b"BBBB\n"),
        "/dev/null" => Vec::new(),
        _ => anyhow::bail!("Failed to read file at '{path}'"),
    };
    Ok(bytes)
}

/// Given the path to a file containing newline separated paths, concatenate the contents of the
/// files at those paths.
async fn concat(ctx: &BuildCtx, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let paths = {
        let bytes = read_file(ctx, path).await?;
        let string = str::from_utf8(&bytes)?;
        string.lines().map(Utf8PathBuf::from).collect::<Vec<_>>()
    };

    let mut output = Vec::new();

    for path in paths {
        let bytes = read_file(ctx, path).await?;
        output.extend(bytes);
    }

    // TODO: Make the context `Send` + `Sync` so tasks can run concurrently.

    // let mut handles = Vec::with_capacity(paths.len());

    // for path in paths {
    //     let handle = tokio::spawn(read_file(ctx, path));
    //     handles.push(handle);
    // }

    // for handle in handles {
    //     let bytes = handle.await??;
    //     output.extend(bytes);
    // }

    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test() -> anyhow::Result<()> {
        let ctx = BuildCtx::new();
        let path = Utf8PathBuf::from("/files");
        let result = concat(&ctx, &path).await?;
        assert_eq!(&result, b"AAAA\nAAAA\nBBBB\n");
        let expected_products = papaya::HashMap::new();
        expected_products.pin().insert(
            Key::ReadFile(Utf8PathBuf::from("/files")),
            Value::Bytes(Vec::from(b"/files/a\n/files/a\n/files/b\n")),
        );
        expected_products.pin().insert(
            Key::ReadFile(Utf8PathBuf::from("/files/a")),
            Value::Bytes(Vec::from(b"AAAA\n")),
        );
        expected_products.pin().insert(
            Key::ReadFile(Utf8PathBuf::from("/files/b")),
            Value::Bytes(Vec::from(b"BBBB\n")),
        );
        assert_eq!(ctx.store.products, expected_products);
        // Should match number of files read, not number of file reads; subsequent reads should be
        // cached.
        assert_eq!(ctx.debug_task_counter.load(Ordering::SeqCst), 3);
        Ok(())
    }
}
