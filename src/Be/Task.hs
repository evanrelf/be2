{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Be.Task
  ( Task (..)
  , discoverTasks
  , TupleArgs
  , UncurryN (..)
  )
where

import Be.Build (TaskContext)
import Be.Value (SomeValue, Value)
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
  ) => Task a where
  type TaskArgs a :: [Type]
  type TaskResult a :: Type
  data TaskKey a :: Type
  data TaskValue a :: Type
  taskRealize :: proxy a -> TaskContext SomeValue SomeValue -> TaskArgs a :->: IO (TaskResult a)
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

type TupleArgs :: [Type] -> Type
type family TupleArgs args where
  TupleArgs '[] = ()
  TupleArgs '[a] = a
  TupleArgs '[a, b] = (a, b)
  TupleArgs '[a, b, c] = (a, b, c)
  TupleArgs '[a, b, c, d] = (a, b, c, d)

type UncurryN :: [Type] -> Constraint
class UncurryN args where
  type UncurryNF args result :: Type
  uncurryN :: (args :->: result) -> UncurryNF args result

instance UncurryN '[] where
  type UncurryNF '[] result = result
  uncurryN x = x

instance UncurryN '[a] where
  type UncurryNF '[a] result = a -> result
  uncurryN f = f

instance UncurryN '[a, b] where
  type UncurryNF '[a, b] result = (a, b) -> result
  uncurryN f (a, b) = f a b

instance UncurryN '[a, b, c] where
  type UncurryNF '[a, b, c] result = (a, b, c) -> result
  uncurryN f (a, b, c) = f a b c

instance UncurryN '[a, b, c, d] where
  type UncurryNF '[a, b, c, d] result = (a, b, c, d) -> result
  uncurryN f (a, b, c, d) = f a b c d
