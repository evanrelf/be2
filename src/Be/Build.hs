{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

module Be.Build
  ( BuildSystem (..)
  , State (..)
  , newState
  , stateRealize
  , TaskContext (..)
  , taskContextRealize
  )
where

import Be.Hash (Hash, hash)
import Be.Trace (IsKey, IsValue, Trace (..), fetchTraces, insertTrace)
import Control.Exception (assert)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Database.SQLite.Simple qualified as Sqlite
import Prelude hiding (State, state, trace)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (forConcurrently_, race_)

class (IsKey (Key a), IsValue (Value a)) => BuildSystem a where
  type Key a :: Type
  type Value a :: Type

type Tasks b = TaskContext b -> Key b -> IO (Value b, Bool)

data State b = State
  { tasks :: Tasks b
  , connection :: Sqlite.Connection
  , done :: TVar (Map (Key b) (TMVar ()))
  , store :: TVar (Map (Key b) (Value b))
  , debugTaskCount :: TVar Int
  }

newState :: Sqlite.Connection -> Tasks b -> STM (State b)
newState connection tasks = do
  done <- newTVar Map.empty
  store <- newTVar Map.empty
  debugTaskCount <- newTVar 0
  pure State{ tasks, connection, done, store, debugTaskCount }

stateRealize :: BuildSystem b => State b -> Key b -> IO (Value b)
stateRealize state key = do
  eBarrier <- atomically do
    done <- readTVar state.done
    case Map.lookup key done of
      Just barrier -> pure (Right barrier)
      Nothing -> do
        barrier <- newEmptyTMVar
        modifyTVar' state.done (Map.insert key barrier)
        pure (Left barrier)

  case eBarrier of
    Right barrier -> atomically do
      readTMVar barrier
      store <- readTVar state.store
      -- SAFETY: The key is marked as done, so it has already been built, and
      -- its value is present in the store.
      let value = fromMaybe (error "unreachable") (Map.lookup key store)
      pure value

    Left barrier -> do
      value <-
        stateFetch state key >>= \case
          Just value -> pure value
          Nothing -> do
            atomically $ modifyTVar' state.debugTaskCount (+ 1)
            stateBuild state key
      atomically do
        modifyTVar' state.store (Map.insert key value)
        -- SAFETY: The key has not been marked as done yet, and no other tasks
        -- will attempt to.
        putTMVar barrier ()
      pure value

stateFetch
  :: forall b. BuildSystem b
  => State b -> Key b -> IO (Maybe (Value b))
stateFetch state key = do
  traces :: [Trace (Key b) (Value b)] <- fetchTraces state.connection (Just key)

  matches :: Set (Value b) <-
    Set.fromList . map (.value) <$> do
      traces & filterM \trace -> do
        assert (hash trace.key == hash key) $ pure ()
        Map.assocs trace.deps & allConcurrently \(depKey, depValueHash) -> do
          depValue <- stateRealize state depKey
          pure (depValueHash == hash depValue)

  store <- readTVarIO state.store

  if| Just storeValue <- Map.lookup key store, Set.member storeValue matches ->
        pure (Just storeValue)
    | Just cachedValue <- Set.lookupMin matches ->
        pure (Just cachedValue)
    | otherwise ->
        pure Nothing

stateBuild :: forall b. BuildSystem b => State b -> Key b -> IO (Value b)
stateBuild state key = do
  taskContext <- atomically $ newTaskContext state
  (value, volatile) <- state.tasks taskContext key
  when (not volatile) do
    deps <- readTVarIO taskContext.deps
    _traceId <- insertTrace state.connection Trace{ key, deps, value }
    pure ()
  pure value

data TaskContext b = TaskContext
  { state :: State b
  , deps :: TVar (Map (Key b) Hash)
  }

newTaskContext :: State b -> STM (TaskContext b)
newTaskContext state = do
  deps <- newTVar Map.empty
  pure TaskContext{ state, deps }

taskContextRealize :: BuildSystem b => TaskContext b -> Key b -> IO (Value b)
taskContextRealize taskContext key = do
  value <- stateRealize taskContext.state key
  atomically $ modifyTVar' taskContext.deps $ Map.insert key (hash value)
  pure value

allConcurrently :: (Foldable f, MonadUnliftIO m) => (a -> m Bool) -> f a -> m Bool
allConcurrently f xs = do
  m <- newEmptyMVar
  forConcurrently_ xs \x ->
    race_
      (unlessM (f x) (void (tryPutMVar m ())))
      (readMVar m)
  isNothing <$> tryReadMVar m
