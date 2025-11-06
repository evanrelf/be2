module Be.BuildTest where

import Be.Build
import Be.Hash (Hash (..))
import Be.Trace (Trace (..), dbMigrate, fetchTraces)
import Be.Value (SomeValue (..), Value, fromSomeValue, toSomeValue)
import Codec.Serialise (Serialise)
import Data.Map.Strict qualified as Map
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (concat, readFile, state)
import Test.Tasty.HUnit

data TestKey
  = Key_ReadFile FilePath
  | Key_Concat FilePath
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (Serialise, Value)

data TestValue
  = Value_ReadFile ByteString
  | Value_Concat ByteString
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (Serialise, Value)

readFile :: TaskContext SomeValue SomeValue -> FilePath -> IO ByteString
readFile taskContext path = do
  let key = Key_ReadFile path
  value <- taskContextRealize taskContext (toSomeValue key)
  case fromSomeValue value of
    Just (Value_ReadFile bytes) -> pure bytes
    _ -> error $ "unexpected: " <> show value

taskReadFile :: TaskContext SomeValue SomeValue -> FilePath -> IO ByteString
taskReadFile _taskContext path = do
  let toBytes :: Text -> ByteString
      toBytes = encodeUtf8
  let bytes = case path of
        "/files" -> toBytes "/files/a\n/files/a\n/files/b\n"
        "/files/a" -> toBytes "AAAA\n"
        "/files/b" -> toBytes "BBBB\n"
        "/dev/null" -> toBytes ""
        _ -> error $ "Failed to read file at '" <> toText path <> "'"
  pure bytes

concat :: TaskContext SomeValue SomeValue -> FilePath -> IO ByteString
concat taskContext path = do
  let key = Key_Concat path
  value <- taskContextRealize taskContext (toSomeValue key)
  case fromSomeValue value of
    Just (Value_Concat bytes) -> pure bytes
    _ -> error $ "unexpected: " <> show value

taskConcat :: TaskContext SomeValue SomeValue -> FilePath -> IO ByteString
taskConcat taskContext path = do
  bytes <- readFile taskContext path
  let text = decodeUtf8 bytes
  let paths = map toString (lines text)
  output <- foldMapM (readFile taskContext) paths
  pure output

unit_build_system :: Assertion
unit_build_system = do
  let toBytes :: Text -> ByteString
      toBytes = encodeUtf8

  SQLite.withConnection ":memory:" \connection -> do
    dbMigrate connection

    let tasks :: TaskContext SomeValue SomeValue -> SomeValue -> IO (SomeValue, Bool)
        tasks taskContext someValue =
          case fromSomeValue someValue of
            Just (Key_ReadFile path) -> do
              bytes <- taskReadFile taskContext path
              let value = Value_ReadFile bytes
              let volatile = True
              pure (toSomeValue value, volatile)

            Just (Key_Concat path) -> do
              bytes <- taskConcat taskContext path
              let value = Value_Concat bytes
              let volatile = False
              pure (toSomeValue value, volatile)

            Nothing ->
              error $ "unexpected: " <> show someValue

    state <- atomically $ newState connection tasks

    let path = "/files"

    actualResult <- stateRealize state (toSomeValue (Key_Concat path))
    let expectedResult = toSomeValue (Value_Concat (toBytes "AAAA\nAAAA\nBBBB\n"))
    assertEqual "result" expectedResult actualResult

    let expectedStore = Map.fromList
          [ ( toSomeValue (Key_ReadFile "/files")
            , toSomeValue (Value_ReadFile (toBytes "/files/a\n/files/a\n/files/b\n"))
            )
          , ( toSomeValue (Key_ReadFile "/files/a")
            , toSomeValue (Value_ReadFile (toBytes "AAAA\n"))
            )
          , ( toSomeValue (Key_ReadFile "/files/b")
            , toSomeValue (Value_ReadFile (toBytes "BBBB\n"))
            )
          , ( toSomeValue (Key_Concat "/files")
            , toSomeValue (Value_Concat (toBytes "AAAA\nAAAA\nBBBB\n"))
            )
          ]
    actualStore <- readTVarIO state.store
    assertEqual "store" expectedStore actualStore

    let expectedDone = fmap (const True) expectedStore
    actualDone <- do
      done <- readTVarIO state.done
      forM done \tmvar -> do
        isEmpty <- atomically $ isEmptyTMVar tmvar
        pure (not isEmpty)
    assertEqual "done" expectedDone actualDone

    let expectedTraces =
          [ Trace
              { key = Key_Concat "/files"
              , deps = Map.fromList
                  [ ( Key_ReadFile "/files"
                    , Hash 217649648357837811
                    )
                  , ( Key_ReadFile "/files/a"
                    , Hash 7664945061632064206
                    )
                  , ( Key_ReadFile "/files/b"
                    , Hash 2092587128809980294
                    )
                  ]
              , value = Value_Concat (toBytes "AAAA\nAAAA\nBBBB\n")
              }
          ]
    actualTraces :: [Trace TestKey TestValue] <- fetchTraces connection Nothing
    assertEqual "traces" expectedTraces actualTraces

    -- 3 `readFile`s, 1 `concat`
    taskCount <- readTVarIO state.debugTaskCount
    assertEqual "task count" taskCount 4

    state' <- atomically $ newState connection tasks

    -- Second run should produce the same results...

    actualResult' <- stateRealize state' (toSomeValue (Key_Concat path))
    assertEqual "result 2" expectedResult actualResult'

    actualStore' <- readTVarIO state'.store
    assertEqual "store 2" expectedStore actualStore'

    actualDone' <- do
      done <- readTVarIO state'.done
      forM done \tmvar -> do
        isEmpty <- atomically $ isEmptyTMVar tmvar
        pure (not isEmpty)
    assertEqual "done 2" expectedDone actualDone'

    actualTraces' :: [Trace TestKey TestValue] <- fetchTraces connection Nothing
    assertEqual "traces 2" expectedTraces actualTraces'

    -- ...but not run any non-volatile tasks, because they're cached.
    -- 3 `readFile`s (volatile), 1 `concat` (non-volatile)
    taskCount' <- readTVarIO state'.debugTaskCount
    assertEqual "task count 2" taskCount' 3

    pure ()
