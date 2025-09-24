use camino::{Utf8Path, Utf8PathBuf};
use std::{collections::HashMap, str};
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

/// Global context used for the duration of the build. Stores build products, constructive traces,
/// etc.
#[derive(Default)]
struct BuildCtx {
    debug_task_counter: usize,
    products: HashMap<Key, Value>,
    traces: Vec<Trace>,
}

impl BuildCtx {
    fn new() -> Self {
        Self::default()
    }

    /// Get the value associated with the given key. Either retrieves a cached value from the cache,
    /// or kicks off a task to produce the value.
    async fn fetch(&mut self, key: &Key) -> anyhow::Result<Value> {
        if let Some(value) = self.products.get(key) {
            return Ok(value.clone());
        }
        let value = match key {
            Key::Which(name) => {
                let mut ctx = TaskCtx::from(&mut *self);
                // let path = task_which(&mut ctx, name).await?;
                let path = task_which_stub(&mut ctx, name).await?;
                Value::Path(path)
            }
            Key::ReadFile(path) => {
                let mut ctx = TaskCtx::from(&mut *self);
                // let bytes = task_read_file(&mut ctx, path).await?;
                let bytes = task_read_file_stub(&mut ctx, path).await?;
                Value::Bytes(bytes)
            }
        };
        self.debug_task_counter += 1;
        self.products.insert(key.clone(), value.clone());
        Ok(value)
    }

    // https://hackage.haskell.org/package/build-1.1/docs/src/Build.Trace.html#isDirtyCT
    fn is_dirty(&self, key: &Key) -> bool {
        for trace in &self.traces {
            let key_match = trace.key == *key;
            let value_match = trace.value == *self.products.get(key).unwrap();
            let deps_match = trace
                .deps
                .iter()
                .all(|(dep_key, dep_hash)| self.products.get(dep_key).unwrap().hash() == *dep_hash);
            if key_match && value_match && deps_match {
                return false;
            }
        }
        true
    }

    // https://hackage.haskell.org/package/build-1.1/docs/src/Build.Trace.html#recordCT
    fn record(&mut self) {
        todo!()
    }

    // https://hackage.haskell.org/package/build-1.1/docs/src/Build.Trace.html#constructCT
    #[expect(clippy::unused_async)]
    async fn construct(&self, _key: &Key) -> anyhow::Result<Vec<Value>> {
        todo!()
    }
}

/// Local context used for the duration of a task. Extends the global build context, tracking a
/// task's dynamic dependencies.
struct TaskCtx<'a> {
    build_ctx: &'a mut BuildCtx,
    deps: HashMap<Key, Value>,
}

impl TaskCtx<'_> {
    /// Thin wrapper around `BuildCtx::fetch`, adding task dependency tracking.
    async fn fetch(&mut self, key: &Key) -> anyhow::Result<Value> {
        let value = self.build_ctx.fetch(key).await?;
        self.deps.insert(key.clone(), value.clone());
        Ok(value)
    }
}

impl<'a> From<&'a mut BuildCtx> for TaskCtx<'a> {
    fn from(build_ctx: &'a mut BuildCtx) -> Self {
        Self {
            build_ctx,
            deps: HashMap::new(),
        }
    }
}

async fn which(ctx: &mut TaskCtx<'_>, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let Value::Path(bytes) = ctx.fetch(&Key::Which(name.to_owned())).await? else {
        unreachable!()
    };
    Ok(bytes)
}

async fn task_which(_ctx: &mut TaskCtx<'_>, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let output = Command::new("which").arg(name).output().await?;

    if !output.status.success() {
        anyhow::bail!("Failed to run `which`");
    }

    let string = str::from_utf8(&output.stdout)?.trim();

    let path = Utf8PathBuf::from(string);

    Ok(path)
}

#[expect(clippy::unused_async)]
async fn task_which_stub(_ctx: &mut TaskCtx<'_>, name: &str) -> anyhow::Result<Utf8PathBuf> {
    let path = match name {
        "sh" => Utf8PathBuf::from("/bin/sh"),
        "vim" => Utf8PathBuf::from("/usr/bin/vim"),
        _ => anyhow::bail!("Failed to find path to `{name}`"),
    };

    Ok(path)
}

async fn read_file(ctx: &mut TaskCtx<'_>, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let Value::Bytes(bytes) = ctx.fetch(&Key::ReadFile(path.to_owned())).await? else {
        unreachable!()
    };
    Ok(bytes)
}

async fn task_read_file(_ctx: &mut TaskCtx<'_>, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let bytes = fs::read(&path).await?;
    Ok(bytes)
}

#[expect(clippy::unused_async)]
async fn task_read_file_stub(_ctx: &mut TaskCtx<'_>, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
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
async fn concat(ctx: &mut TaskCtx<'_>, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
    let paths = {
        let bytes = read_file(ctx, path).await?;
        let string = str::from_utf8(&bytes)?;
        string.lines().map(Utf8PathBuf::from).collect::<Vec<_>>()
    };

    let mut output = Vec::new();

    for path in paths {
        let bytes = read_file(ctx, &path).await?;
        output.extend(bytes);
    }

    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test() -> anyhow::Result<()> {
        let mut build_ctx = BuildCtx::new();
        let mut task_ctx = TaskCtx::from(&mut build_ctx);
        let path = Utf8PathBuf::from("/files");
        let result = concat(&mut task_ctx, &path).await?;
        assert_eq!(&result, b"AAAA\nAAAA\nBBBB\n");
        let mut expected_products = HashMap::new();
        expected_products.insert(
            Key::ReadFile(Utf8PathBuf::from("/files")),
            Value::Bytes(Vec::from(b"/files/a\n/files/a\n/files/b\n")),
        );
        expected_products.insert(
            Key::ReadFile(Utf8PathBuf::from("/files/a")),
            Value::Bytes(Vec::from(b"AAAA\n")),
        );
        expected_products.insert(
            Key::ReadFile(Utf8PathBuf::from("/files/b")),
            Value::Bytes(Vec::from(b"BBBB\n")),
        );
        assert_eq!(build_ctx.products, expected_products);
        // Should match number of files read, not number of file reads; subsequent reads should be
        // cached.
        assert_eq!(build_ctx.debug_task_counter, 3);
        Ok(())
    }
}
