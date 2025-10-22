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
import Prelude hiding (State)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (forConcurrently_, race_)

class (IsKey (Key a), IsValue (Value a)) => BuildSystem a where
  type Key a :: Type
  type Value a :: Type
  tasks :: TaskContext a -> Key a -> IO (Value a, Bool)

data State b = State
  { connection :: Sqlite.Connection
  , done :: TVar (Map (Key b) (TMVar ()))
  , store :: TVar (Map (Key b) (Value b))
  }

newState :: BuildSystem b => Sqlite.Connection -> STM (State b)
newState connection = do
  done <- newTVar Map.empty
  store <- newTVar Map.empty
  pure State{ connection, done, store }

stateRealize :: BuildSystem b => State b -> Key b -> IO (Value b)
stateRealize state key = do
  -- TODO: Stuff

  value <-
    stateFetch state key >>= \case
      Just value -> pure value
      Nothing -> stateBuild state key

  -- TODO: Insert (key, value) into store

  -- TODO: Mark key as done (populate TMVar)

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
  (value, volatile) <- tasks taskContext key
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

anyConcurrently :: (Foldable f, MonadUnliftIO m) => (a -> m Bool) -> f a -> m Bool
anyConcurrently f xs = do
  m <- newEmptyMVar
  forConcurrently_ xs \x ->
    race_
      (whenM (f x) (void (tryPutMVar m ())))
      (readMVar m)
  isJust <$> tryReadMVar m

allConcurrently :: (Foldable f, MonadUnliftIO m) => (a -> m Bool) -> f a -> m Bool
allConcurrently f xs = do
  m <- newEmptyMVar
  forConcurrently_ xs \x ->
    race_
      (unlessM (f x) (void (tryPutMVar m ())))
      (readMVar m)
  isNothing <$> tryReadMVar m
