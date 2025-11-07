{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Be.Task
  ( Task (..)
  , realize
  , discoverTasks
  , CurryN (..)
  , registerTask
  , registerTaskWith
  , TaskOptions (..)
  , defaultTaskOptions
  )
where

import Be.Build (TaskContext', taskContextRealize)
import Be.Value (Value, fromSomeValue', toSomeValue)
import Codec.Serialise (Serialise)
import Data.Char (toUpper)
import Data.HashMap.Strict qualified as HashMap
import DiscoverInstances (SomeDict, SomeDictOf (..), discoverInstances)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax qualified as TH
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

  taskOptions :: TaskOptions
  taskOptions = defaultTaskOptions

  taskSing :: a

  taskBuild :: proxy a -> TaskContext' -> TaskArgs a :->: IO (TaskResult a)

  taskRealize :: proxy a -> TaskContext' -> TaskArgs a :->: IO (TaskResult a)
  taskRealize _ taskContext = curryN \args -> do
    let argsToKey :: TupleArgs (TaskArgs a) -> TaskKey a
        argsToKey = coerce
    let valueToResult :: TaskValue a -> TaskResult a
        valueToResult = coerce
    someValue <- taskContextRealize taskContext (toSomeValue (argsToKey args))
    pure (valueToResult (fromSomeValue' someValue))

realize :: Task a => a -> TaskContext' -> TaskArgs a :->: IO (TaskResult a)
realize sing = taskRealize (Identity sing)

data TaskOptions = TaskOptions
  { volatile :: Bool
  }
  deriving stock (Lift)

defaultTaskOptions :: TaskOptions
defaultTaskOptions =
  TaskOptions
    { volatile = False
    }

registerTask :: TH.Name -> TH.Q [TH.Dec]
registerTask funName = registerTaskWith funName defaultTaskOptions

registerTaskWith :: TH.Name -> TaskOptions -> TH.Q [TH.Dec]
registerTaskWith funName options = do
  info <- TH.reify funName
  typ <- case info of
    TH.VarI _ typ _ -> pure typ
    _ -> fail "task: expected a function"
  (taskArgs, taskResult) <- case argsAndResult typ of
    ([], _) -> fail "task: function must have at least one argument (TaskContext)"
    (_taskContext : taskArgs, returnType) -> case unwrapIO returnType of
      Just taskResult -> pure (taskArgs, taskResult)
      Nothing -> fail "task: function must run in IO monad"
  sequence
    [ generateDataDec
    , generateInstDec taskArgs taskResult
    ]
  where
  argsAndResult :: TH.Type -> ([TH.Type], TH.Type)
  argsAndResult = go []
    where
    go args (TH.AppT (TH.AppT TH.ArrowT arg) rest) = go (arg : args) rest
    go args result = (reverse args, result)

  unwrapIO :: TH.Type -> Maybe TH.Type
  unwrapIO = \case
    (TH.AppT (TH.ConT f) a) | TH.nameBase f == "IO" -> Just a
    _ -> Nothing

  capitalizeFirst :: String -> String
  capitalizeFirst = \case
    [] -> []
    (c : cs) -> toUpper c : cs

  baseName = TH.nameBase funName
  dataName = TH.mkName (capitalizeFirst baseName)
  keyName = TH.mkName (capitalizeFirst baseName ++ "Key")
  valueName = TH.mkName (capitalizeFirst baseName ++ "Value")

  generateDataDec :: TH.Q TH.Dec
  generateDataDec =
    pure $ TH.DataD [] dataName [] Nothing [TH.NormalC dataName []] []

  generateInstDec :: [TH.Type] -> TH.Type -> TH.Q TH.Dec
  generateInstDec taskArgs taskResult = do
    let dataType = TH.ConT dataName

    -- `instance Task ...`
    instanceHead <- [t| Task $(pure dataType) |]

    -- `type TaskArgs _ = ...`
    taskArgsInst <- do
      let promote :: [TH.Type] -> TH.Type
          promote = foldr (\t ts -> TH.AppT (TH.AppT TH.PromotedConsT t) ts) TH.PromotedNilT
      lhs <- [t| TaskArgs $(pure dataType) |]
      pure $ TH.TySynInstD $ TH.TySynEqn Nothing lhs (promote taskArgs)

    -- `type TaskResult _ = ...`
    taskResultInst <- do
      lhs <- [t| TaskResult $(pure dataType) |]
      pure $ TH.TySynInstD $ TH.TySynEqn Nothing lhs taskResult

    let newtypeField typ = [(TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, typ)]

    -- `newtype TaskKey _ = ...`
    taskKeyInst <- TH.NewtypeInstD [] Nothing
      <$> [t| TaskKey $(pure dataType) |]
      <*> pure Nothing
      <*> (TH.NormalC keyName . newtypeField <$> [t| TupleArgs (TaskArgs $(pure dataType)) |])
      <*> pure
            [ TH.DerivClause (Just TH.StockStrategy) (map TH.ConT [''Generic, ''Show, ''Eq])
            , TH.DerivClause (Just TH.AnyclassStrategy) (map TH.ConT [''Serialise, ''Hashable, ''Value])
            ]

    -- `newtype TaskValue _ = ...`
    taskValueInst <- TH.NewtypeInstD [] Nothing
      <$> [t| TaskValue $(pure dataType) |]
      <*> pure Nothing
      <*> (TH.NormalC valueName . newtypeField <$> [t| TaskResult $(pure dataType) |])
      <*> pure
            [ TH.DerivClause (Just TH.StockStrategy) (map TH.ConT [''Generic, ''Show, ''Eq])
            , TH.DerivClause (Just TH.AnyclassStrategy) (map TH.ConT [''Serialise, ''Hashable, ''Value])
            ]

    -- `taskOptions = ...`
    taskOptionsFun <- TH.funD (TH.mkName "taskOptions")
      [TH.clause [] (TH.normalB (TH.lift options)) []]

    -- `taskSing = ...`
    taskSingFun <- TH.funD (TH.mkName "taskSing")
      [TH.clause [] (TH.normalB (TH.conE dataName)) []]

    -- `taskBuild = ...`
    taskBuildFun <- TH.funD (TH.mkName "taskBuild")
      [TH.clause [TH.wildP] (TH.normalB (TH.varE funName)) []]

    pure $ TH.InstanceD Nothing [] instanceHead
      [ taskArgsInst
      , taskResultInst
      , taskKeyInst
      , taskValueInst
      , taskOptionsFun
      , taskSingFun
      , taskBuildFun
      ]

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
