{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Low-level polymorphic build engine implementing incremental computation.
--
-- This module implements the core scheduling and rebuilding logic for the build
-- system, following the "Build Systems à la Carte" paper. It provides:
--
-- * Concurrent task execution with STM-based coordination
-- * Early cutoff optimization via trace validation
-- * In-memory memoization (store)
-- * SQLite-backed persistent caching
--
-- The build engine is polymorphic over key/value types and delegates task
-- execution to a user-provided function. For a higher-level DSL, see
-- "Be.Core.Build.Dynamic".
module Be.Core.Build.Static
  ( BuildState (..)
  , newBuildState
  , TaskState (..)
  , newTaskState
  , taskStateRealize
  )
where

import Be.Core.Hash (Hash, hash)
import Be.Core.Trace (Trace (..), fetchTraces, insertTrace)
import Be.Core.Value (Value)
import Control.Exception (assert)
import Control.Exception.Annotated.UnliftIO qualified as Exception
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.String ()  -- For IsString instance only
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (trace)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (forConcurrently_, race_)

-- | Global build state shared across all tasks.
--
-- This contains the core infrastructure for coordinating concurrent builds:
--
-- * 'tasks': User-provided task execution function
-- * 'connection': SQLite connection for persistent trace storage
-- * 'done': Barriers signaling build completion (success or exception)
-- * 'store': In-memory cache of successfully computed values only
-- * 'debugTaskCount': Counter for testing/debugging (tracks non-cached task executions)
--
-- The barrier holds @Either SomeException ()@ rather than @Either SomeException v@
-- to avoid duplicating values. On success, the value is stored only in 'store'.
-- Waiters read the barrier, then look up the value in 'store' if successful.
data BuildState k v = BuildState
  { tasks :: TaskState k v -> k -> IO (v, Bool)
  , connection :: SQLite.Connection
  , done :: TVar (HashMap k (TMVar (Either SomeException ())))
  , store :: TVar (HashMap k v)
  , debugTaskCount :: TVar Int
  }

newBuildState
  :: SQLite.Connection
  -> (TaskState k v -> k -> IO (v, Bool))
  -> STM (BuildState k v)
newBuildState connection tasks = do
  done <- newTVar HashMap.empty
  store <- newTVar HashMap.empty
  debugTaskCount <- newTVar 0
  pure BuildState{ tasks, connection, done, store, debugTaskCount }

-- | Main entry point: realize a key to a value.
--
-- This implements the core build algorithm with the following properties:
--
-- 1. **Thread-safe**: Multiple threads can request the same key concurrently.
--    The first thread to request a key inserts a barrier (TMVar) into 'done',
--    preventing duplicate work. Other threads wait on the barrier.
--
-- 2. **Early cutoff**: Before executing a task, we attempt to restore from cache
--    via 'buildStateFetch', which validates traces from previous builds.
--
-- 3. **Memoization**: Once computed successfully, the value is stored in both 'store'
--    (in-memory) and persisted to SQLite (unless volatile).
--
-- 4. **Exception handling**: If a task throws, the exception is stored in the barrier
--    so all concurrent waiters receive it. Exceptions are NOT persisted to the trace
--    database, so subsequent build sessions will retry the task. However, within a
--    single build session, the exception is cached to ensure all concurrent threads
--    see consistent failure.
buildStateRealize :: (Value k, Value v) => BuildState k v -> k -> IO v
buildStateRealize buildState key = do
  eBarrier <- atomically do
    done <- readTVar buildState.done
    case HashMap.lookup key done of
      Just barrier -> pure (Right barrier)
      Nothing -> do
        barrier <- newEmptyTMVar
        modifyTVar' buildState.done (HashMap.insert key barrier)
        pure (Left barrier)

  case eBarrier of
    Right barrier -> do
      -- Another thread is building or has built this key. Wait for result.
      -- Read both barrier and store in same STM transaction to ensure consistency.
      result <- atomically do
        readTMVar barrier >>= \case
          Right () -> do
            store <- readTVar buildState.store
            -- SAFETY: The barrier is filled only after the value is inserted into store.
            -- Both operations happen in the same STM transaction in the builder.
            let value = fromMaybe (error "unreachable") (HashMap.lookup key store)
            pure (Right value)
          Left exception -> pure (Left exception)

      case result of
        Right value -> pure value
        Left exception -> Exception.throw exception

    Left barrier -> do
      -- We're responsible for building this key.
      eValue <- Exception.try @SomeException do
        buildStateFetch buildState key >>= \case
          Just value -> pure value
          Nothing -> do
            atomically $ modifyTVar' buildState.debugTaskCount (+ 1)
            buildStateBuild buildState key

      atomically do
        case eValue of
          Right value -> do
            -- Store value first, then signal success
            modifyTVar' buildState.store (HashMap.insert key value)
            putTMVar barrier (Right ())
          Left exception ->
            -- Signal failure without storing
            putTMVar barrier (Left exception)

      case eValue of
        Right value -> pure value
        Left exception -> Exception.throw exception

-- | Attempt to restore a value from cache (early cutoff optimization).
--
-- This implements the "verifying trace" rebuilder from Build Systems à la Carte:
--
-- 1. Load all traces from SQLite that match this key
-- 2. For each trace, verify that all dependencies still have the same hash
-- 3. If any trace is valid (all dep hashes match), return its cached value
--
-- The function prefers values already in the store, then falls back to any valid
-- cached value from disk.
--
-- **Exception handling**: Dependencies are realized during trace validation. These
-- should normally be cache hits (deps were already built when the trace was created).
-- If a dependency throws during validation, we annotate the exception with context
-- indicating this is unexpected, then rethrow it. The calling code will treat it
-- like any other build failure.
buildStateFetch :: (Value k, Value v) => BuildState k v -> k -> IO (Maybe v)
buildStateFetch buildState key = do
  traces <- fetchTraces buildState.connection (Just key)

  matches :: HashSet v <-
    HashSet.fromList . map (.value) <$> do
      traces & filterM \trace -> do
        assert (hash trace.key == hash key) $ pure ()
        HashMap.toList trace.deps & allConcurrently \(depKey, depValueHash) -> do
          -- Dependencies should already be built (they were recorded in the trace).
          -- If one throws here, it's unexpected - annotate and rethrow.
          depValue <-
            Exception.checkpoint "While validating cached trace" $
            Exception.checkpoint (fromString $ "Dependency: " ++ show depKey) $
            Exception.checkpoint "This dependency was previously built successfully (it's in the trace)" $
              buildStateRealize buildState depKey
          pure (depValueHash == hash depValue)

  store <- readTVarIO buildState.store

  if| Just storeValue <- HashMap.lookup key store
    , HashSet.member storeValue matches ->
        pure (Just storeValue)
    | Just cachedValue <- viaNonEmpty head (HashSet.toList matches) ->
        pure (Just cachedValue)
    | otherwise ->
        pure Nothing

buildStateBuild :: (Value k, Value v) => BuildState k v -> k -> IO v
buildStateBuild buildState key = do
  taskState <- atomically $ newTaskState buildState
  (value, volatile) <- buildState.tasks taskState key
  when (not volatile) do
    deps <- readTVarIO taskState.deps
    _traceId <- insertTrace buildState.connection Trace{ key, deps, value }
    pure ()
  pure value

-- | Per-task state for tracking dependencies.
--
-- Each task gets its own TaskState during execution. The 'deps' field accumulates
-- all dependencies realized during task execution, along with their hashes. This
-- is used to construct the trace that will be persisted to SQLite.
data TaskState k v = TaskState
  { buildState :: BuildState k v
  , deps :: TVar (HashMap k Hash)
  }

newTaskState :: BuildState k v -> STM (TaskState k v)
newTaskState buildState = do
  deps <- newTVar HashMap.empty
  pure TaskState{ buildState, deps }

taskStateRealize :: (Value k, Value v) => TaskState k v -> k -> IO v
taskStateRealize taskState key = do
  value <- buildStateRealize taskState.buildState key
  atomically $ modifyTVar' taskState.deps $ HashMap.insert key (hash value)
  pure value

-- | Concurrent "all" predicate with short-circuit evaluation.
--
-- Evaluates a predicate on all elements in parallel. Returns True only if all
-- predicates return True. Short-circuits on the first False.
--
-- Implementation: Uses an MVar as a failure signal. The first thread to observe
-- False writes to the MVar, causing all other threads to abort via 'race_'.
--
-- REVIEW: This is a clever concurrent algorithm! However, the name might be
-- confusing - "allConcurrently" suggests it's like 'forConcurrently', but it's
-- actually a parallel predicate evaluator. Consider renaming to 'allConcurrentlyM'
-- or 'andConcurrently' to better indicate the short-circuit behavior.
allConcurrently
  :: (Foldable f, MonadUnliftIO m)
  => (a -> m Bool) -> f a -> m Bool
allConcurrently f xs = do
  m <- newEmptyMVar
  forConcurrently_ xs \x ->
    race_
      (unlessM (f x) (void (tryPutMVar m ())))
      (readMVar m)
  isNothing <$> tryReadMVar m
