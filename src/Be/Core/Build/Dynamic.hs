{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | High-level task DSL built on top of "Be.Core.Build.Static".
--
-- This module provides an ergonomic, type-safe interface for defining build tasks
-- using Template Haskell. Key features:
--
-- * 'registerTask' TH macro: Generates Task instances from regular functions
-- * 'realize': Invoke tasks with natural function call syntax
-- * 'initBuild' TH macro: Discovers and registers all Value/Task instances at compile time
-- * Dynamic dispatch through runtime type registry
--
-- Example usage:
--
-- @
--   readFile :: FilePath -> Build ByteString
--   readFile path = ...
--
--   registerTask 'readFile
--
--   main = do
--     $$initBuild
--     runBuild connection $ realize ReadFile "/foo/bar.txt"
-- @
--
-- REVIEW: The Template Haskell approach is powerful but has tradeoffs:
-- * Pro: Eliminates boilerplate, natural API
-- * Con: Compile-time complexity, harder to debug type errors
-- * Con: Generates types at TH time (ReadFile, ReadFileKey, etc.) which pollute namespace
module Be.Core.Build.Dynamic
  ( Build (..)
  , unwrapBuild
  , runBuild
  , realize

  , Task (..)
  , TaskOptions (..)
  , defaultTaskOptions

  , initBuild
  , registerTask
  , registerTaskWith
  )
where

import Be.Core.Build.Static qualified as Static
import Be.Core.Registry (discoverInstances, getInstances, registerInstances)
import Be.Core.Value (SomeValue (..), Value, fromSomeValue, fromSomeValue', toSomeValue)
import Codec.Serialise (Serialise)
import Data.Char (toUpper)
import Data.HashMap.Strict qualified as HashMap
import Database.SQLite.Simple qualified as SQLite
import DiscoverInstances (Class (..), Dict (..), SomeDict, SomeDictOf (..), (:-) (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax qualified as TH
import VarArgs ((:->:))

-- | Type alias for the low-level task state, specialized to SomeValue.
-- This allows the high-level API to work with heterogeneous task types.
type TaskState = Static.TaskState SomeValue SomeValue

-- | The Build monad: context for executing build tasks.
--
-- This is a ReaderT over IO, carrying the TaskState. Tasks use this monad to:
-- 1. Realize dependencies via 'realize'
-- 2. Perform arbitrary IO
-- 3. Track their own dependencies automatically
newtype Build a = Build (ReaderT TaskState IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

unwrapBuild :: Build a -> (TaskState -> IO a)
unwrapBuild (Build (ReaderT f)) = \s -> f s

runBuild :: SQLite.Connection -> Build a -> IO a
runBuild connection taskM = do
  let tasks = getTasks
  buildState <- atomically $ Static.newBuildState connection (\taskState someKey -> unwrapBuild (tasks someKey) taskState)
  taskState <- atomically $ Static.newTaskState buildState
  unwrapBuild taskM taskState

realize :: Task a => a -> TaskArgs a :->: Build (TaskResult a)
realize sing = curryN \(args :: TupleArgs (TaskArgs a)) -> do
  uncurryN (taskRealize @a (Identity sing)) args :: Build (TaskResult a)

-- | Typeclass for registering build tasks.
--
-- This is a complex class with several associated types and coercibility constraints.
-- The Template Haskell machinery in 'registerTask' generates these instances
-- automatically, so users rarely need to understand the details.
--
-- Associated types:
-- * 'TaskArgs': Type-level list of argument types
-- * 'TaskResult': Return type (the 'a' in 'Build a')
-- * 'TaskKey': Newtype wrapper around tuple of arguments (for serialization)
-- * 'TaskValue': Newtype wrapper around result (for serialization)
--
-- The Coercible constraints ensure zero-cost conversion between the user-facing
-- types (arguments, result) and the internal wrapper types (TaskKey, TaskValue).
--
-- REVIEW: This is sophisticated type-level programming! The use of injective type
-- families (via '= (r :: Type) |') and functional dependencies in CurryN ensures
-- type inference works smoothly. However, the complexity is non-trivial. Consider
-- adding more examples in documentation.
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

  taskBuild :: proxy a -> TaskArgs a :->: Build (TaskResult a)

instance Class (Typeable a) (Task a) where
  cls = Sub Dict

taskRealize :: Task a => proxy a -> TaskArgs a :->: Build (TaskResult a)
taskRealize proxy = curryN \args -> do
  taskState <- Build ask
  someValue <- liftIO $ Static.taskStateRealize taskState (toSomeValue (argsToKey proxy args))
  pure (valueToResult proxy (fromSomeValue' someValue))

taskHandler :: Task a => proxy a -> TaskKey a -> Build (TaskValue a, Bool)
taskHandler @a proxy key = do
  result <- uncurryN (taskBuild proxy) (keyToArgs key)
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

initBuild :: TH.Code TH.Q (IO ())
initBuild =
  [|| do
    let values :: [SomeDict Value]
        values = $$discoverInstances
    registerInstances values
    let tasks :: [SomeDict Task]
        tasks = $$discoverInstances
    registerInstances tasks
  ||]

registerTask :: TH.Name -> TH.Q [TH.Dec]
registerTask funName = registerTaskWith funName defaultTaskOptions

registerTaskWith :: TH.Name -> TaskOptions -> TH.Q [TH.Dec]
registerTaskWith funName options = do
  info <- TH.reify funName
  typ <- case info of
    TH.VarI _ typ _ -> pure typ
    _ -> fail "Task registration expected a function"
  let (taskArgs, returnType) = argsAndResult typ
  taskResult <- case unwrapIO returnType of
    Just taskResult -> pure taskResult
    Nothing -> fail "Task function must run in 'Build' monad"
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
    (TH.AppT (TH.ConT f) a) | TH.nameBase f == "Build" -> Just a
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

-- | Existential wrapper for task handlers.
-- Each handler knows how to execute a specific task type.
data TaskHandler where
  TaskHandler :: Task a => (TaskKey a -> Build (TaskValue a, Bool)) -> TaskHandler

-- | Dynamic dispatch: route a SomeValue key to the appropriate task handler.
--
-- This function implements type-safe dynamic dispatch by:
-- 1. Retrieving all registered Task instances from the runtime registry
-- 2. Converting each to a TaskHandler (erasing the concrete type)
-- 3. Attempting to match the input key against each handler's expected type
-- 4. Executing the first matching handler
--
-- REVIEW: This is a linear search through all registered tasks! For a small number
-- of tasks this is fine, but could become a bottleneck with hundreds of task types.
-- Consider using a HashMap keyed by TypeRep for O(1) lookup instead of foldr.
--
-- REVIEW: The fallback error message is helpful, but occurs at runtime. With TH,
-- we could potentially detect missing Task instances at compile time.
getTasks :: SomeValue -> Build (SomeValue, Bool)
getTasks someKey@(SomeValue t _) = do
  let instances = getInstances @Task
  let dicts = HashMap.elems instances
  let toTaskHandler (SomeDictOf @Task proxy) = TaskHandler (taskHandler proxy)
  let taskHandlers = map toTaskHandler dicts
  let tryHandler (TaskHandler handler) rest =
        case fromSomeValue someKey of
          Just key -> do
            (value, volatile) <- handler key
            pure (toSomeValue value, volatile)
          Nothing -> rest
  let fallback = error $ "No handler for task `" <> show t <> "`; `Task` instance missing from registry"
  foldr tryHandler fallback taskHandlers

-- | Type-level currying/uncurrying for variadic task arguments.
--
-- This typeclass bridges between type-level lists of arguments and actual function
-- types. The injective type family 'TupleArgs' ensures bidirectional type inference.
--
-- Examples:
--   CurryN '[]          => TupleArgs = ()
--   CurryN '[a]         => TupleArgs = Identity a
--   CurryN '[a, b]      => TupleArgs = (a, b)
--   CurryN '[a, b, c]   => TupleArgs = (a, b, c)
--
-- The use of Identity for single-argument tasks avoids special-casing in the
-- serialization logic (everything is a "tuple", even singletons).
--
-- REVIEW: Limited to 0-4 arguments. This is probably sufficient for most use cases,
-- but if you need more, you'll need to add instances manually. Consider using a
-- library like 'varargs' more extensively, or generating instances with TH.
--
-- REVIEW: The injective type family is crucial here - without it, type inference
-- would fail in 'realize' and 'registerTask'. Well done!
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
