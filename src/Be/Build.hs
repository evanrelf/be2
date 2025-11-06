{-# LANGUAGE MultiWayIf #-}

module Be.Build
  ( State (..)
  , newState
  , stateRealize
  , TaskContext (..)
  , taskContextRealize
  )
where

import Be.Hash (Hash, hash)
import Be.Trace (Trace (..), fetchTraces, insertTrace)
import Be.Value (Value)
import Control.Exception (assert)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (State, state, trace)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (forConcurrently_, race_)

data State k v = State
  { tasks :: TaskContext k v -> k -> IO (v, Bool)
  , connection :: SQLite.Connection
  , done :: TVar (HashMap k (TMVar ()))
  , store :: TVar (HashMap k v)
  , debugTaskCount :: TVar Int
  }

newState
  :: SQLite.Connection
  -> (TaskContext k v -> k -> IO (v, Bool))
  -> STM (State k v)
newState connection tasks = do
  done <- newTVar HashMap.empty
  store <- newTVar HashMap.empty
  debugTaskCount <- newTVar 0
  pure State{ tasks, connection, done, store, debugTaskCount }

stateRealize :: (Value k, Value v) => State k v -> k -> IO v
stateRealize state key = do
  eBarrier <- atomically do
    done <- readTVar state.done
    case HashMap.lookup key done of
      Just barrier -> pure (Right barrier)
      Nothing -> do
        barrier <- newEmptyTMVar
        modifyTVar' state.done (HashMap.insert key barrier)
        pure (Left barrier)

  case eBarrier of
    Right barrier -> atomically do
      readTMVar barrier
      store <- readTVar state.store
      -- SAFETY: The key is marked as done, so it has already been built, and
      -- its value is present in the store.
      let value = fromMaybe (error "unreachable") (HashMap.lookup key store)
      pure value

    Left barrier -> do
      value <-
        stateFetch state key >>= \case
          Just value -> pure value
          Nothing -> do
            atomically $ modifyTVar' state.debugTaskCount (+ 1)
            stateBuild state key
      atomically do
        modifyTVar' state.store (HashMap.insert key value)
        -- SAFETY: The key has not been marked as done yet, and no other tasks
        -- will attempt to.
        putTMVar barrier ()
      pure value

stateFetch :: forall k v. (Value k, Value v) => State k v -> k -> IO (Maybe v)
stateFetch state key = do
  traces :: [Trace k v] <- fetchTraces state.connection (Just key)

  matches :: HashSet v <-
    HashSet.fromList . map (.value) <$> do
      traces & filterM \trace -> do
        assert (hash trace.key == hash key) $ pure ()
        HashMap.toList trace.deps & allConcurrently \(depKey, depValueHash) -> do
          depValue <- stateRealize state depKey
          pure (depValueHash == hash depValue)

  store <- readTVarIO state.store

  if| Just storeValue <- HashMap.lookup key store
    , HashSet.member storeValue matches ->
        pure (Just storeValue)
    | Just cachedValue <- viaNonEmpty head (HashSet.toList matches) ->
        pure (Just cachedValue)
    | otherwise ->
        pure Nothing

stateBuild :: forall k v. (Value k, Value v) => State k v -> k -> IO v
stateBuild state key = do
  taskContext <- atomically $ newTaskContext state
  (value, volatile) <- state.tasks taskContext key
  when (not volatile) do
    deps <- readTVarIO taskContext.deps
    _traceId <- insertTrace state.connection Trace{ key, deps, value }
    pure ()
  pure value

data TaskContext k v = TaskContext
  { state :: State k v
  , deps :: TVar (HashMap k Hash)
  }

newTaskContext :: State k v -> STM (TaskContext k v)
newTaskContext state = do
  deps <- newTVar HashMap.empty
  pure TaskContext{ state, deps }

taskContextRealize :: (Value k, Value v) => TaskContext k v -> k -> IO v
taskContextRealize taskContext key = do
  value <- stateRealize taskContext.state key
  atomically $ modifyTVar' taskContext.deps $ HashMap.insert key (hash value)
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
