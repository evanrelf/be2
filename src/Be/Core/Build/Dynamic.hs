{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Be.Core.Build.Dynamic
  ( BuildState'
  , Static.BuildState (..)
  , TaskState'
  , Static.TaskState (..)
  , newBuildState'
  , newTaskState'

  , TaskM
  , runTaskM
  , realize
  , task
  , io

  , Task (..)
  , TaskOptions (..)
  , defaultTaskOptions

  , registerTask
  , registerTaskWith
  )
where

import Be.Core.Build.Static qualified as Static
import Be.Core.Registry (getInstancesIO)
import Be.Core.Value (SomeValue (..), Value, fromSomeValue, fromSomeValue', toSomeValue)
import Codec.Serialise (Serialise)
import Data.Char (toUpper)
import Data.HashMap.Strict qualified as HashMap
import Database.SQLite.Simple qualified as SQLite
import DiscoverInstances (Class (..), Dict (..), SomeDictOf (..), (:-) (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax qualified as TH
import VarArgs ((:->:))

type BuildState' = Static.BuildState SomeValue SomeValue

type TaskState' = Static.TaskState SomeValue SomeValue

newBuildState' :: SQLite.Connection -> IO BuildState'
newBuildState' connection = do
  tasks <- getTasks
  atomically $ Static.newBuildState connection tasks

newTaskState' :: BuildState' -> STM TaskState'
newTaskState' = Static.newTaskState

newtype TaskM a = TaskM (ReaderT TaskState' IO a)
  deriving newtype (Functor, Applicative, Monad)

runTaskM :: TaskState' -> TaskM a -> IO a
runTaskM taskState (TaskM readerT) = runReaderT readerT taskState

realize :: Task a => a -> TaskState' -> TaskArgs a :->: IO (TaskResult a)
realize sing = taskRealize (Identity sing)

task :: Task a => a -> TaskArgs a :->: TaskM (TaskResult a)
task sing = curryN \(args :: TupleArgs (TaskArgs a)) -> do
  taskState <- TaskM ask
  result :: TaskResult a <- io $ uncurryN (taskRealize @a (Identity sing) taskState) args
  pure result

io :: IO a -> TaskM a
io action = TaskM (ReaderT \_ -> action)

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

  taskBuild :: proxy a -> TaskState' -> TaskArgs a :->: IO (TaskResult a)

instance Class (Typeable a) (Task a) where
  cls = Sub Dict

taskRealize :: Task a => proxy a -> TaskState' -> TaskArgs a :->: IO (TaskResult a)
taskRealize proxy taskState = curryN \args -> do
  someValue <- Static.taskStateRealize taskState (toSomeValue (argsToKey proxy args))
  pure (valueToResult proxy (fromSomeValue' someValue))

taskHandler :: Task a => proxy a -> TaskState' -> TaskKey a -> IO (TaskValue a, Bool)
taskHandler @a proxy taskState key = do
  result <- uncurryN (taskBuild proxy taskState) (keyToArgs key)
  let options = taskOptions @a
  pure (resultToValue result, options.volatile)

argsToKey :: Task a => proxy a -> TupleArgs (TaskArgs a) -> TaskKey a
argsToKey _ = coerce

keyToArgs :: Task a => TaskKey a -> TupleArgs (TaskArgs a)
keyToArgs = coerce

resultToValue :: Task a => TaskResult a -> TaskValue a
resultToValue = coerce

valueToResult :: Task a => proxy a -> TaskValue a -> TaskResult a
valueToResult _ = coerce

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
    ([], _) -> fail "task: function must have at least one argument (TaskState)"
    (_taskState : taskArgs, returnType) -> case unwrapIO returnType of
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

data TaskHandler where
  TaskHandler :: Task a => (TaskState' -> TaskKey a -> IO (TaskValue a, Bool)) -> TaskHandler

getTasks :: IO (TaskState' -> SomeValue -> IO (SomeValue, Bool))
getTasks = do
  mInstances <- getInstancesIO @Task
  let instances = fromMaybe HashMap.empty mInstances
  let dicts = HashMap.elems instances
  let toTaskHandler (SomeDictOf @Task proxy) = TaskHandler (taskHandler proxy)
  let taskHandlers = map toTaskHandler dicts
  pure \taskState someKey@(SomeValue t _) -> do
    let tryHandler (TaskHandler handler) rest =
          case fromSomeValue someKey of
            Just key -> do
              (value, volatile) <- handler taskState key
              pure (toSomeValue value, volatile)
            Nothing -> rest
    let fallback = error $ "No task handler for `" <> show t <> "`"
    foldr tryHandler fallback taskHandlers

type CurryN :: [Type] -> Constraint
class CurryN args where
  type TupleArgs args = (r :: Type) | r -> args
  curryN :: (TupleArgs args -> result) -> (args :->: result)
  uncurryN :: (args :->: result) -> (TupleArgs args -> result)

instance CurryN '[] where
  type TupleArgs '[] = ()
  curryN f = f ()
  uncurryN f () = f

instance CurryN '[a] where
  type TupleArgs '[a] = Identity a
  curryN f a = f (Identity a)
  uncurryN f (Identity a) = f a

instance CurryN '[a, b] where
  type TupleArgs '[a, b] = (a, b)
  curryN f a b = f (a, b)
  uncurryN f (a, b) = f a b

instance CurryN '[a, b, c] where
  type TupleArgs '[a, b, c] = (a, b, c)
  curryN f a b c = f (a, b, c)
  uncurryN f (a, b, c) = f a b c

instance CurryN '[a, b, c, d] where
  type TupleArgs '[a, b, c, d] = (a, b, c, d)
  curryN f a b c d = f (a, b, c, d)
  uncurryN f (a, b, c, d) = f a b c d
