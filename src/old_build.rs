use camino::{Utf8Path, Utf8PathBuf};
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    str,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};
use tokio::{fs, process::Command, sync::OnceCell};
use twox_hash::XxHash3_64;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Key {
    Which(String),
    File(Utf8PathBuf),
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
    debug_stubs: AtomicBool,
    debug_task_counter: AtomicUsize,
    done: papaya::HashSet<Key>,
    store: papaya::HashMap<Key, OnceCell<Value>>,
    traces: boxcar::Vec<Trace>,
}

impl BuildCtx {
    fn new() -> Arc<Self> {
        Arc::new(Self::default())
    }

    /// Get the value associated with the given key. Either retrieves a previously cached value, or
    /// kicks off a task to produce the value.
    async fn build(self: Arc<Self>, key: &Key) -> anyhow::Result<Value> {
        if self.done.pin().contains(key) {
            let store = self.store.pin();
            // SAFETY: If a key is marked as "done," it has already been built, and its value is
            // present in the store.
            let thunk = store.get(key).unwrap();
            let value = thunk.get().unwrap().clone();
            return Ok(value);
        }
        let ctx = Arc::clone(&self);
        let store = self.store.pin_owned();
        let thunk = store.get_or_insert(key.clone(), OnceCell::new());
        let value = thunk
            .get_or_try_init(|| async {
                let value = match key {
                    Key::Which(name) => {
                        let path = if self.debug_stubs.load(Ordering::SeqCst) {
                            task_which_stub(ctx, name).await?
                        } else {
                            task_which(ctx, name).await?
                        };
                        Value::Path(path)
                    }
                    Key::File(path) => {
                        let bytes = if self.debug_stubs.load(Ordering::SeqCst) {
                            task_read_file_stub(ctx, path).await?
                        } else {
                            task_read_file(ctx, path).await?
                        };
                        Value::Bytes(bytes)
                    }
                };
                anyhow::Ok(value)
            })
            .await?
            .clone();
        drop(store);
        self.done.pin().insert(key.clone());
        self.debug_task_counter.fetch_add(1, Ordering::SeqCst);
        Ok(value.clone())
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
    let Value::Bytes(bytes) = ctx.build(&Key::File(path.to_owned())).await? else {
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

// Represent changes to build system code by making all builds depend on binary as input!

// Need to represent volatile (i.e. uncached) tasks, such as querying compiler version.

// Build systems à la carte
// (https://www.microsoft.com/en-us/research/wp-content/uploads/2018/03/build-systems.pdf)
//
// * Scheduling algorithm: Suspending (§4.1.3)
// * Rebuilding strategy: Verifying or constructive traces (§4.2.2 or §4.2.3)
//
// Allows for dynamic/monadic dependencies. Suspending seems like the best fit for async Rust, and
// therefore likely the most ergonomic to write.

/*

type Rebuilder c i k v = k -> v -> Task c k v -> Task (MonadState i) k v

type Scheduler c i j k v = Rebuilder c j k v -> Build c i k v

type Build c i k v = Tasks c k v -> k -> Store i k v -> Store i k v

data Store i k v = Store { info :: i, values :: k -> v }

type Tasks c k v = k -> Maybe (Task c k v)

type Task c k v = forall f. c f => (k -> f v) -> f v

shake :: (Ord k, Hashable v) => Build Monad (VT k v) k v
shake = suspending vtRebuilder

suspending :: forall i k v. Ord k => Scheduler Monad i i k v

vtRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (VT k v) k v

newtype VT k v = VT [Trace k v (Hash v)]

---

-- A request
data Key

-- A response
data Value

type Rebuilder = Key -> Value -> Task -> Task'

type Scheduler = Rebuilder -> Build

type Build = (Key -> Maybe Task) -> Key -> Store -> Store

data Store = Store { info :: [Trace], values :: Map Key Value }

type Task = forall f. Monad f => (Key -> f Value) -> f Value

type Task' = forall f. MonadState [Trace] f => (Key -> f Value) -> f Value

data Trace = Trace
  { key :: Key
  , depends :: [(Key, Hash)]
  , result  :: Hash
  }

*/
