{-# LANGUAGE MultiWayIf #-}

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
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (trace)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (forConcurrently_, race_)

data BuildState k v = BuildState
  { tasks :: TaskState k v -> k -> IO (v, Bool)
  , connection :: SQLite.Connection
  , done :: TVar (HashMap k (TMVar ()))
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
    Right barrier -> atomically do
      readTMVar barrier
      store <- readTVar buildState.store
      -- SAFETY: The key is marked as done, so it has already been built, and
      -- its value is present in the store.
      let value = fromMaybe (error "unreachable") (HashMap.lookup key store)
      pure value

    Left barrier -> do
      value <-
        buildStateFetch buildState key >>= \case
          Just value -> pure value
          Nothing -> do
            atomically $ modifyTVar' buildState.debugTaskCount (+ 1)
            buildStateBuild buildState key
      atomically do
        modifyTVar' buildState.store (HashMap.insert key value)
        -- SAFETY: The key has not been marked as done yet, and no other tasks
        -- will attempt to.
        putTMVar barrier ()
      pure value

buildStateFetch :: (Value k, Value v) => BuildState k v -> k -> IO (Maybe v)
buildStateFetch buildState key = do
  traces <- fetchTraces buildState.connection (Just key)

  matches :: HashSet v <-
    HashSet.fromList . map (.value) <$> do
      traces & filterM \trace -> do
        assert (hash trace.key == hash key) $ pure ()
        HashMap.toList trace.deps & allConcurrently \(depKey, depValueHash) -> do
          depValue <- buildStateRealize buildState depKey
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
