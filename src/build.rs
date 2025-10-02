use crate::{
    trace::{self, Trace, fetch_traces, insert_trace},
    util::Xxhash as _,
};
use async_recursion::async_recursion;
use sqlx::SqlitePool;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::BuildHasherDefault,
    pin::Pin,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};
use tokio::sync::SetOnce;
use twox_hash::XxHash3_64;

pub trait Key: trace::Key + Debug + Send + Sync + 'static {}

impl<T> Key for T where T: trace::Key + Debug + Send + Sync + 'static {}

pub trait Value: trace::Value + Debug + Send + Sync + 'static {}

impl<T> Value for T where T: trace::Value + Debug + Send + Sync + 'static {}

type Task<V> = Pin<Box<dyn Future<Output = anyhow::Result<(V, bool)>> + Send>>;

pub trait BuildSystem: Sized + 'static {
    type Key: Key;

    type Value: Value;

    fn tasks(cx: Arc<TaskContext<Self>>, key: Self::Key) -> Task<Self::Value>;

    async fn build(db: SqlitePool, key: Self::Key) -> anyhow::Result<Self::Value> {
        let state = State::<Self>::new(db);
        let value = state.clone().realize(key).await?;
        Ok(value)
    }
}

struct State<T: BuildSystem> {
    db: SqlitePool,
    done: papaya::HashMap<T::Key, SetOnce<()>>,
    store: papaya::HashMap<T::Key, T::Value>,
    debug_use_stubs: AtomicBool,
    debug_task_count: AtomicUsize,
}

impl<T: BuildSystem> State<T> {
    fn new(db: SqlitePool) -> Arc<Self> {
        Arc::new(Self {
            db,
            done: papaya::HashMap::new(),
            store: papaya::HashMap::new(),
            debug_use_stubs: AtomicBool::new(false),
            debug_task_count: AtomicUsize::new(0),
        })
    }

    #[async_recursion]
    async fn realize(self: Arc<Self>, key: T::Key) -> anyhow::Result<T::Value> {
        let done = self.done.pin_owned();

        let mut is_done = true;

        let barrier = done.get_or_insert_with(key.clone(), || {
            is_done = false;
            SetOnce::new()
        });

        if is_done {
            barrier.wait().await;
            // SAFETY: The key is marked as done, so it has already been built, and its value is
            // present in the store.
            let value = self.store.pin().get(&key).unwrap().clone();
            return Ok(value);
        }

        let value = if let Some(value) = self.clone().fetch(&key).await? {
            value
        } else {
            self.debug_task_count.fetch_add(1, Ordering::SeqCst);
            self.clone().build(&key).await?
        };

        self.store.pin().insert(key.clone(), value.clone());

        // SAFETY: The key has not been marked as done yet, and no other tasks will attempt to.
        barrier.set(()).unwrap();

        Ok(value)
    }

    async fn fetch(self: Arc<Self>, key: &T::Key) -> anyhow::Result<Option<T::Value>> {
        let traces = fetch_traces(&self.db, Some(key)).await?;

        let mut matches = HashSet::new();

        // TODO: Check traces concurrently
        'trace: for trace in traces {
            debug_assert_eq!(&trace.key, key);

            // TODO: Realize trace deps concurrently
            for (dep_key, dep_value_hash) in trace.deps {
                let dep_value = self.clone().realize(dep_key).await?;
                if dep_value_hash != dep_value.xxhash() {
                    continue 'trace;
                }
            }

            matches.insert(trace.value);
        }

        if let Some(store_value) = self.store.pin().get(key)
            && matches.contains(store_value)
        {
            Ok(Some(store_value.clone()))
        } else if let Some(cached_value) = matches.drain().next() {
            Ok(Some(cached_value))
        } else {
            Ok(None)
        }
    }

    async fn build(self: Arc<Self>, key: &T::Key) -> anyhow::Result<T::Value> {
        let task_cx = Arc::new(TaskContext::new(self.clone()));

        // If a task is impure or cheaper to rebuild than to cache, mark it as volatile to skip
        // recording traces.
        //
        // For example: reading a file directly (i.e. not via an intermediate file I/O task).
        let (value, volatile) = T::tasks(task_cx.clone(), key.clone()).await?;

        if !volatile {
            insert_trace(
                &self.db,
                &Trace {
                    key: key.clone(),
                    deps: task_cx.deps(),
                    value: value.clone(),
                },
            )
            .await?;
        }

        Ok(value)
    }
}

pub struct TaskContext<T: BuildSystem> {
    state: Arc<State<T>>,
    deps: papaya::HashMap<T::Key, u64>,
}

impl<T: BuildSystem> TaskContext<T> {
    fn new(state: Arc<State<T>>) -> Self {
        Self {
            state,
            deps: papaya::HashMap::new(),
        }
    }

    fn deps(&self) -> HashMap<T::Key, u64, BuildHasherDefault<XxHash3_64>> {
        let mut deps =
            HashMap::with_capacity_and_hasher(self.deps.len(), BuildHasherDefault::default());

        for (key, value_hash) in &self.deps.pin() {
            deps.insert(key.clone(), *value_hash);
        }

        deps
    }

    pub async fn realize(&self, key: T::Key) -> anyhow::Result<T::Value> {
        let value = self.state.clone().realize(key.clone()).await?;
        self.deps.pin().insert(key, value.xxhash());
        Ok(value)
    }

    pub fn use_stubs(&self) -> bool {
        self.state.debug_use_stubs.load(Ordering::SeqCst)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::flatten;
    use bytes::Bytes;
    use camino::{Utf8Path, Utf8PathBuf};
    use serde::{Deserialize, Serialize};
    use similar_asserts::assert_eq;
    use std::{str, sync::Arc};
    use tokio::fs;
    use tracing::Instrument as _;

    #[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
    enum TestKey {
        ReadFile(Arc<Utf8Path>),
        Concat(Arc<Utf8Path>),
    }

    #[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
    enum TestValue {
        Bytes(Bytes),
    }

    struct TestBuildSystem;

    impl BuildSystem for TestBuildSystem {
        type Key = TestKey;

        type Value = TestValue;

        fn tasks(
            cx: Arc<TaskContext<TestBuildSystem>>,
            key: TestKey,
        ) -> Pin<Box<dyn Future<Output = anyhow::Result<(TestValue, bool)>> + Send>> {
            match key {
                TestKey::ReadFile(path) => Box::pin(async move {
                    let bytes = task_read_file(cx, path).await?;
                    let value = TestValue::Bytes(bytes);
                    let volatile = true;
                    Ok((value, volatile))
                }),
                TestKey::Concat(path) => Box::pin(async move {
                    let bytes = task_concat(cx, path).await?;
                    let value = TestValue::Bytes(bytes);
                    let volatile = false;
                    Ok((value, volatile))
                }),
            }
        }
    }

    async fn read_file(
        cx: Arc<TaskContext<TestBuildSystem>>,
        path: impl AsRef<Utf8Path>,
    ) -> anyhow::Result<Bytes> {
        let key = TestKey::ReadFile(Arc::from(path.as_ref()));
        let value = cx.realize(key).await?;
        #[expect(irrefutable_let_patterns)]
        let TestValue::Bytes(bytes) = value else {
            unreachable!()
        };
        Ok(bytes)
    }

    async fn task_read_file(
        cx: Arc<TaskContext<TestBuildSystem>>,
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

    async fn concat(
        cx: Arc<TaskContext<TestBuildSystem>>,
        path: impl AsRef<Utf8Path>,
    ) -> anyhow::Result<Bytes> {
        let key = TestKey::Concat(Arc::from(path.as_ref()));
        let value = cx.realize(key).await?;
        #[expect(irrefutable_let_patterns)]
        let TestValue::Bytes(path) = value else {
            unreachable!()
        };
        Ok(path)
    }

    async fn task_concat(
        cx: Arc<TaskContext<TestBuildSystem>>,
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

    #[tokio::test]
    async fn test_stubs() -> anyhow::Result<()> {
        let db = SqlitePool::connect(":memory:").await?;
        trace::db_migrate(&db).await?;

        let state = State::<TestBuildSystem>::new(db);
        state.debug_use_stubs.store(true, Ordering::SeqCst);

        let path = Utf8Path::new("/files");

        let result = state
            .clone()
            .realize(TestKey::Concat(Arc::from(path)))
            .await?;
        let expected_result = TestValue::Bytes(Bytes::from("AAAA\nAAAA\nBBBB\n"));
        assert_eq!(result, expected_result);

        let expected_store = papaya::HashMap::new();
        expected_store.pin().insert(
            TestKey::ReadFile(Arc::from(Utf8Path::new("/files"))),
            TestValue::Bytes(Bytes::from("/files/a\n/files/a\n/files/b\n")),
        );
        expected_store.pin().insert(
            TestKey::ReadFile(Arc::from(Utf8Path::new("/files/a"))),
            TestValue::Bytes(Bytes::from("AAAA\n")),
        );
        expected_store.pin().insert(
            TestKey::ReadFile(Arc::from(Utf8Path::new("/files/b"))),
            TestValue::Bytes(Bytes::from("BBBB\n")),
        );
        expected_store.pin().insert(
            TestKey::Concat(Arc::from(Utf8Path::new("/files"))),
            TestValue::Bytes(Bytes::from("AAAA\nAAAA\nBBBB\n")),
        );
        assert_eq!(state.store, expected_store);

        let expected_done = papaya::HashMap::new();
        for key in expected_store.pin().keys() {
            expected_done
                .pin()
                .insert(key.clone(), SetOnce::new_with(Some(())));
        }
        assert_eq!(state.done, expected_done);

        let traces = fetch_traces::<TestKey, TestValue>(&state.db, None).await?;

        #[expect(clippy::unreadable_literal)]
        let expected_traces = vec![Trace {
            key: TestKey::Concat(Arc::from(Utf8Path::new("/files"))),
            deps: {
                let mut deps = HashMap::default();
                deps.insert(
                    TestKey::ReadFile(Arc::from(Utf8Path::new("/files"))),
                    7034874801995377595,
                );
                deps.insert(
                    TestKey::ReadFile(Arc::from(Utf8Path::new("/files/a"))),
                    6110124553518527577,
                );
                deps.insert(
                    TestKey::ReadFile(Arc::from(Utf8Path::new("/files/b"))),
                    4445544808285449819,
                );
                deps
            },
            value: TestValue::Bytes(Bytes::from("AAAA\nAAAA\nBBBB\n")),
        }];
        assert_eq!(traces, expected_traces);

        // 3 `read_file`s, 1 `concat`
        assert_eq!(state.debug_task_count.load(Ordering::SeqCst), 4);

        let db = Arc::into_inner(state).unwrap().db;

        let state = State::<TestBuildSystem>::new(db);
        state.debug_use_stubs.store(true, Ordering::SeqCst);

        let result = state
            .clone()
            .realize(TestKey::Concat(Arc::from(path)))
            .await?;

        // Second run should produce the same results...
        assert_eq!(result, expected_result);
        assert_eq!(state.store, expected_store);
        assert_eq!(state.done, expected_done);
        let traces = fetch_traces::<TestKey, TestValue>(&state.db, None).await?;
        assert_eq!(traces, expected_traces);
        // ...but not run any non-volatile tasks, because they're cached.
        // 3 `read_file`s (volatile), 1 `concat` (non-volatile)
        assert_eq!(state.debug_task_count.load(Ordering::SeqCst), 3);

        Ok(())
    }
}
