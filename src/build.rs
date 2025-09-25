use camino::{Utf8Path, Utf8PathBuf};
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    str,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};
use tokio::{fs, process::Command};
use twox_hash::XxHash3_64;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Key {
    Which(String),
    ReadFile(Utf8PathBuf),
}

#[derive(Clone, Debug, Hash, PartialEq)]
enum Value {
    Path(Utf8PathBuf),
    Bytes(Vec<u8>),
}

impl Value {
    fn hash(&self) -> u64 {
        let mut hasher = XxHash3_64::default();
        Hash::hash(&self, &mut hasher);
        hasher.finish()
    }
}

struct Trace {
    key: Key,
    value: Value,
    deps: HashMap<Key, u64>,
}

/// Global context used for the duration of the build. Stores build products, constructive traces,
/// etc.
#[derive(Default)]
struct BuildCtx {
    debug_task_counter: AtomicUsize,
    done: papaya::HashSet<Key>,
    store: papaya::HashMap<Key, Value>,
    traces: boxcar::Vec<Trace>,
}

impl BuildCtx {
    fn new() -> Arc<Self> {
        Arc::new(Self::default())
    }

    /// Get the value associated with the given key. Either retrieves a previously cached value, or
    /// kicks off a task to produce the value.
    async fn build(self: Arc<Self>, key: &Key) -> anyhow::Result<Value> {
        let debug_stub = true;
        if self.done.pin().contains(key) {
            let store = self.store.pin();
            let value = store.get(key).unwrap();
            return Ok(value.clone());
        }
        let ctx = Arc::clone(&self);
        let value = match key {
            Key::Which(name) => {
                let path = if debug_stub {
                    task_which_stub(ctx, name).await?
                } else {
                    task_which(ctx, name).await?
                };
                Value::Path(path)
            }
            Key::ReadFile(path) => {
                let bytes = if debug_stub {
                    task_read_file_stub(ctx, path).await?
                } else {
                    task_read_file(ctx, path).await?
                };
                Value::Bytes(bytes)
            }
        };
        self.done.pin().insert(key.clone());
        self.store.pin().insert(key.clone(), value.clone());
        self.debug_task_counter.fetch_add(1, Ordering::SeqCst);
        Ok(value)
    }

    // https://hackage.haskell.org/package/build-1.1/docs/src/Build.Trace.html#recordCT
    fn record(self: Arc<Self>, trace: Trace) {
        self.traces.push(trace);
    }

    // https://hackage.haskell.org/package/build-1.1/docs/src/Build.Trace.html#constructCT
    async fn construct(self: Arc<Self>, key: &Key) -> anyhow::Result<Vec<Value>> {
        let mut values = Vec::new();
        'trace: for (_index, trace) in &self.traces {
            if trace.key != *key {
                continue;
            }
            for (dep_key, dep_hash) in &trace.deps {
                let ctx = Arc::clone(&self);
                let hash = ctx.build(dep_key).await?.hash();
                if *dep_hash != hash {
                    continue 'trace;
                }
            }
            values.push(trace.value.clone());
        }
        Ok(values)
    }
}

async fn which(ctx: Arc<BuildCtx>, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let Value::Path(bytes) = ctx.build(&Key::Which(name.to_owned())).await? else {
        unreachable!()
    };
    Ok(bytes)
}

async fn task_which(_ctx: Arc<BuildCtx>, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let output = Command::new("which").arg(name).output().await?;

    if !output.status.success() {
        anyhow::bail!("Failed to run `which`");
    }

    let string = str::from_utf8(&output.stdout)?.trim();

    let path = Utf8PathBuf::from(string);

    Ok(path)
}

#[expect(clippy::unused_async)]
async fn task_which_stub(_ctx: Arc<BuildCtx>, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let path = match name {
        "sh" => Utf8PathBuf::from("/bin/sh"),
        "vim" => Utf8PathBuf::from("/usr/bin/vim"),
        _ => anyhow::bail!("Failed to find path to `{name}`"),
    };

    Ok(path)
}

async fn read_file(ctx: Arc<BuildCtx>, path: impl AsRef<Utf8Path>) -> anyhow::Result<Vec<u8>> {
    let path = path.as_ref();
    let Value::Bytes(bytes) = ctx.build(&Key::ReadFile(path.to_owned())).await? else {
        unreachable!()
    };
    Ok(bytes)
}

async fn task_read_file(_ctx: Arc<BuildCtx>, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let bytes = fs::read(&path).await?;
    Ok(bytes)
}

#[expect(clippy::unused_async)]
async fn task_read_file_stub(_ctx: Arc<BuildCtx>, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
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
async fn concat(ctx: Arc<BuildCtx>, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let paths = {
        let bytes = read_file(Arc::clone(&ctx), path).await?;
        let string = str::from_utf8(&bytes)?;
        string.lines().map(Utf8PathBuf::from).collect::<Vec<_>>()
    };

    let mut output = Vec::new();

    let mut handles = Vec::with_capacity(paths.len());

    for path in paths {
        let handle = tokio::spawn(read_file(Arc::clone(&ctx), path));
        handles.push(handle);
    }

    for handle in handles {
        let bytes = handle.await??;
        output.extend(bytes);
    }

    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test() -> anyhow::Result<()> {
        let ctx = Arc::new(BuildCtx::new());
        let path = Utf8PathBuf::from("/files");

        let result = concat(Arc::clone(&ctx), &path).await?;
        assert_eq!(&result, b"AAAA\nAAAA\nBBBB\n");

        let expected_store = papaya::HashMap::new();
        expected_store.pin().insert(
            Key::ReadFile(Utf8PathBuf::from("/files")),
            Value::Bytes(Vec::from(b"/files/a\n/files/a\n/files/b\n")),
        );
        expected_store.pin().insert(
            Key::ReadFile(Utf8PathBuf::from("/files/a")),
            Value::Bytes(Vec::from(b"AAAA\n")),
        );
        expected_store.pin().insert(
            Key::ReadFile(Utf8PathBuf::from("/files/b")),
            Value::Bytes(Vec::from(b"BBBB\n")),
        );
        assert_eq!(ctx.store, expected_store);

        let expected_done = papaya::HashSet::new();
        for key in expected_store.pin().keys() {
            expected_done.pin().insert(key.clone());
        }
        assert_eq!(ctx.done, expected_done);

        // Should match number of files read, not number of file reads; subsequent reads should be
        // cached.
        assert_eq!(ctx.debug_task_counter.load(Ordering::SeqCst), 3);

        Ok(())
    }
}
