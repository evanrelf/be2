{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Be.BuildTest where

import Be.Build
import Be.Hash (Hash (..))
import Be.Task (Task (..), TaskOptions (..), defaultTaskOptions, discoverTasks, getTasks, realize, registerTask, registerTaskWith)
import Be.Trace (Trace (..), dbMigrate, fetchTraces)
import Be.Value (Value, discoverValues, toSomeValue)
import Codec.Serialise (Serialise)
import Data.HashMap.Strict qualified as HashMap
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (concat, readFile)
import Test.Tasty.HUnit

data TestKey
  = Key_ReadFile FilePath
  | Key_Concat FilePath
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise, Hashable, Value)

data TestValue
  = Value_ReadFile ByteString
  | Value_Concat ByteString
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise, Hashable, Value)

readFile :: TaskState TestKey TestValue -> FilePath -> IO ByteString
readFile taskState path = do
  let key = Key_ReadFile path
  value <- taskStateRealize taskState key
  case value of
    Value_ReadFile bytes -> pure bytes
    _ -> error $ "unexpected: " <> show value

taskReadFile :: TaskState TestKey TestValue -> FilePath -> IO ByteString
taskReadFile _taskState path = do
  let toBytes :: Text -> ByteString
      toBytes = encodeUtf8
  let bytes = case path of
        "/files" -> toBytes "/files/a\n/files/a\n/files/b\n"
        "/files/a" -> toBytes "AAAA\n"
        "/files/b" -> toBytes "BBBB\n"
        "/dev/null" -> toBytes ""
        _ -> error $ "Failed to read file at '" <> toText path <> "'"
  pure bytes

concat :: TaskState TestKey TestValue -> FilePath -> IO ByteString
concat taskState path = do
  let key = Key_Concat path
  value <- taskStateRealize taskState key
  case value of
    Value_Concat bytes -> pure bytes
    _ -> error $ "unexpected: " <> show value

taskConcat :: TaskState TestKey TestValue -> FilePath -> IO ByteString
taskConcat taskState path = do
  bytes <- readFile taskState path
  let text = decodeUtf8 bytes
  let paths = map toString (lines text)
  output <- foldMapM (readFile taskState) paths
  pure output

unit_build_system :: Assertion
unit_build_system = do
  let toBytes :: Text -> ByteString
      toBytes = encodeUtf8

  SQLite.withConnection ":memory:" \connection -> do
    dbMigrate connection

    let tasks :: TaskState TestKey TestValue -> TestKey -> IO (TestValue, Bool)
        tasks taskState = \case
          Key_ReadFile path -> do
            bytes <- taskReadFile taskState path
            let value = Value_ReadFile bytes
            let volatile = True
            pure (value, volatile)

          Key_Concat path -> do
            bytes <- taskConcat taskState path
            let value = Value_Concat bytes
            let volatile = False
            pure (value, volatile)

    buildState <- atomically $ newBuildState connection tasks

    let path = "/files"

    taskState <- atomically $ newTaskState buildState
    actualResult <- taskStateRealize taskState (Key_Concat path)
    let expectedResult = Value_Concat (toBytes "AAAA\nAAAA\nBBBB\n")
    assertEqual "result" expectedResult actualResult

    let expectedStore = HashMap.fromList
          [ ( Key_ReadFile "/files"
            , Value_ReadFile (toBytes "/files/a\n/files/a\n/files/b\n")
            )
          , ( Key_ReadFile "/files/a"
            , Value_ReadFile (toBytes "AAAA\n")
            )
          , ( Key_ReadFile "/files/b"
            , Value_ReadFile (toBytes "BBBB\n")
            )
          , ( Key_Concat "/files"
            , Value_Concat (toBytes "AAAA\nAAAA\nBBBB\n")
            )
          ]
    actualStore <- readTVarIO buildState.store
    assertEqual "store" expectedStore actualStore

    let expectedDone = fmap (const True) expectedStore
    actualDone <- do
      done <- readTVarIO buildState.done
      forM done \tmvar -> do
        isEmpty <- atomically $ isEmptyTMVar tmvar
        pure (not isEmpty)
    assertEqual "done" expectedDone actualDone

    let expectedTraces =
          [ Trace
              { key = Key_Concat "/files"
              , deps = HashMap.fromList
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
    taskCount <- readTVarIO buildState.debugTaskCount
    assertEqual "task count" taskCount 4

    buildState' <- atomically $ newBuildState connection tasks

    -- Second run should produce the same results...

    taskState' <- atomically $ newTaskState buildState'
    actualResult' <- taskStateRealize taskState' (Key_Concat path)
    assertEqual "result 2" expectedResult actualResult'

    actualStore' <- readTVarIO buildState'.store
    assertEqual "store 2" expectedStore actualStore'

    actualDone' <- do
      done <- readTVarIO buildState'.done
      forM done \tmvar -> do
        isEmpty <- atomically $ isEmptyTMVar tmvar
        pure (not isEmpty)
    assertEqual "done 2" expectedDone actualDone'

    actualTraces' :: [Trace TestKey TestValue] <- fetchTraces connection Nothing
    assertEqual "traces 2" expectedTraces actualTraces'

    -- ...but not run any non-volatile tasks, because they're cached.
    -- 3 `readFile`s (volatile), 1 `concat` (non-volatile)
    taskCount' <- readTVarIO buildState'.debugTaskCount
    assertEqual "task count 2" taskCount' 3

    pure ()

add1 :: TaskState' -> Int -> IO Int
add1 _taskState n = pure (n + 1)

registerTaskWith 'add1 defaultTaskOptions{ volatile = True }

yell :: TaskState' -> Text -> IO Text
yell _taskState message = do
  pure (message <> "!")

registerTask 'yell

greet :: TaskState' -> Text -> IO Text
greet taskState name = do
  let message = "Hello, " <> name
  message' <- realize Yell taskState message
  pure message'

registerTask 'greet

unit_existential_build_system :: Assertion
unit_existential_build_system = do
  $$discoverValues
  $$discoverTasks

  SQLite.withConnection ":memory:" \connection -> do
    dbMigrate connection

    tasks <- getTasks

    do
      buildState <- atomically $ newBuildState connection tasks
      do
        taskState <- atomically $ newTaskState buildState
        actualResult <- taskStateRealize taskState (toSomeValue (Add1Key 1))
        let expectedResult = toSomeValue (Add1Value 2)
        assertEqual "result 1" expectedResult actualResult
      do
        taskState <- atomically $ newTaskState buildState
        actualResult <- taskStateRealize taskState (toSomeValue (GreetKey "Evan"))
        let expectedResult = toSomeValue (GreetValue "Hello, Evan!")
        assertEqual "result 1" expectedResult actualResult
      taskCount <- readTVarIO buildState.debugTaskCount
      assertEqual "task count 1" taskCount 3

    do
      buildState <- atomically $ newBuildState connection tasks
      do
        taskState <- atomically $ newTaskState buildState
        actualResult <- taskStateRealize taskState (toSomeValue (Add1Key 1))
        let expectedResult = toSomeValue (Add1Value 2)
        assertEqual "result 1" expectedResult actualResult
      do
        taskState <- atomically $ newTaskState buildState
        actualResult <- taskStateRealize taskState (toSomeValue (GreetKey "Evan"))
        let expectedResult = toSomeValue (GreetValue "Hello, Evan!")
        assertEqual "result 1" expectedResult actualResult
      taskCount <- readTVarIO buildState.debugTaskCount
      assertEqual "task count 2" taskCount 1
