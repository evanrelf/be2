{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Be.Task
  ( Task (..)
  , discoverTasks
  , CurryN (..)
  )
where

import Be.Build (TaskContext, taskContextRealize)
import Be.Value (SomeValue, Value, fromSomeValue', toSomeValue)
import Data.HashMap.Strict qualified as HashMap
import DiscoverInstances (SomeDict, SomeDictOf (..), discoverInstances)
import Language.Haskell.TH qualified as TH
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (SomeTypeRep, someTypeRep)
import VarArgs ((:->:))

type Task :: Type -> Constraint
class
  ( Typeable a
  , Value (TaskKey a)
  , Value (TaskValue a)
  , Coercible (TaskKey a) (TupleArgs (TaskArgs a))
  , Coercible (TaskValue a) (TaskResult a)
  , CurryN (TaskArgs a)
  )
  => Task a where
  type TaskArgs a :: [Type]

  type TaskResult a :: Type

  data TaskKey a :: Type

  data TaskValue a :: Type

  taskRealize :: proxy a -> TaskContext SomeValue SomeValue -> TaskArgs a :->: IO (TaskResult a)
  taskRealize _ taskContext = curryN \args -> do
    let argsToKey :: TupleArgs (TaskArgs a) -> TaskKey a
        argsToKey = coerce
    let valueToResult :: TaskValue a -> TaskResult a
        valueToResult = coerce
    someValue <- taskContextRealize taskContext (toSomeValue (argsToKey args))
    pure (valueToResult (fromSomeValue' someValue))

  taskBuild :: proxy a -> TaskContext SomeValue SomeValue -> TaskArgs a :->: IO (TaskResult a)

taskRegistry :: IORef (HashMap SomeTypeRep (SomeDict Task))
taskRegistry = unsafePerformIO $ newIORef HashMap.empty
{-# NOINLINE taskRegistry #-}

discoverTasks :: TH.Code TH.Q (IO ())
discoverTasks = [|| do
    let dicts :: [SomeDict Task]
        dicts = $$discoverInstances
    let tasks = dicts
          & map (\dict@(SomeDictOf proxy) -> (someTypeRep proxy, dict))
          & HashMap.fromList
    atomicModifyIORef' taskRegistry \tr -> (HashMap.union tasks tr, ())
  ||]

lookupTask :: SomeTypeRep -> Maybe (SomeDict Task)
lookupTask t = unsafePerformIO do
  vr <- readIORef taskRegistry
  pure $ HashMap.lookup t vr
{-# NOINLINE lookupTask #-}

type CurryN :: [Type] -> Constraint
class CurryN args where
  type TupleArgs args = (r :: Type) | r -> args
  curryN :: (TupleArgs args -> result) -> (args :->: result)

instance CurryN '[] where
  type TupleArgs '[] = ()
  curryN f = f ()

instance CurryN '[a] where
  type TupleArgs '[a] = Identity a
  curryN f a = f (Identity a)

instance CurryN '[a, b] where
  type TupleArgs '[a, b] = (a, b)
  curryN f a b = f (a, b)

instance CurryN '[a, b, c] where
  type TupleArgs '[a, b, c] = (a, b, c)
  curryN f a b c = f (a, b, c)

instance CurryN '[a, b, c, d] where
  type TupleArgs '[a, b, c, d] = (a, b, c, d)
  curryN f a b c d = f (a, b, c, d)
