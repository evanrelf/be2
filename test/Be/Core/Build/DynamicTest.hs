{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Be.Core.Build.DynamicTest where

import Be.Core.Build.Dynamic
import Be.Core.Build.Static (BuildState (..), TaskState (..))
import Be.Core.Hash (hash)
import Be.Core.Registry (discoverInstances, registerInstances)
import Be.Core.Trace (Trace (..), dbCreate, dbDrop, fetchTraces)
import Be.Core.Value (SomeValue, Value, toSomeValue)
import Data.HashMap.Strict qualified as HashMap
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (concat, readFile)
import Test.Tasty.HUnit

readFile :: TaskState' -> FilePath -> IO ByteString
readFile _taskState path = do
  let toBytes :: Text -> ByteString
      toBytes = encodeUtf8
  let bytes = case path of
        "/files" -> toBytes "/files/a\n/files/a\n/files/b\n"
        "/files/a" -> toBytes "AAAA\n"
        "/files/b" -> toBytes "BBBB\n"
        "/dev/null" -> toBytes ""
        _ -> error $ "Failed to read file at '" <> toText path <> "'"
  pure bytes

registerTaskWith 'readFile defaultTaskOptions{ volatile = True }

concat :: TaskState' -> FilePath -> IO ByteString
concat taskState path = do
  bytes <- realize ReadFile taskState path
  let text = decodeUtf8 bytes
  let paths = map toString (lines text)
  output <- foldMapM (realize ReadFile taskState) paths
  pure output

registerTask 'concat

unit_build_system_dynamic :: Assertion
unit_build_system_dynamic = do
  registerInstances $$(discoverInstances @Value)
  registerInstances $$(discoverInstances @Task)

  let toBytes :: Text -> ByteString
      toBytes = encodeUtf8

  SQLite.withConnection ":memory:" \connection -> do
    dbDrop connection
    dbCreate connection

    taskState <- newTaskState' connection
    let buildState = taskState.buildState

    let path = "/files"

    actualResult <- realize Concat taskState path
    let expectedResult = toBytes "AAAA\nAAAA\nBBBB\n"
    assertEqual "result 1" expectedResult actualResult

    let expectedStore = HashMap.fromList
          [ ( toSomeValue (ReadFileKey "/files")
            , toSomeValue (ReadFileValue (toBytes "/files/a\n/files/a\n/files/b\n"))
            )
          , ( toSomeValue (ReadFileKey "/files/a")
            , toSomeValue (ReadFileValue (toBytes "AAAA\n"))
            )
          , ( toSomeValue (ReadFileKey "/files/b")
            , toSomeValue (ReadFileValue (toBytes "BBBB\n"))
            )
          , ( toSomeValue (ConcatKey "/files")
            , toSomeValue (ConcatValue (toBytes "AAAA\nAAAA\nBBBB\n"))
            )
          ]
    actualStore <- readTVarIO buildState.store
    assertEqual "store 1" expectedStore actualStore

    let expectedDone = fmap (const True) expectedStore
    actualDone <- do
      done <- readTVarIO buildState.done
      forM done \tmvar -> do
        isEmpty <- atomically $ isEmptyTMVar tmvar
        pure (not isEmpty)
    assertEqual "done 1" expectedDone actualDone

    let expectedTraces =
          [ Trace
              { key = toSomeValue (ConcatKey "/files")
              , deps = HashMap.fromList
                  [ ( toSomeValue (ReadFileKey "/files")
                    , hash (toSomeValue (ReadFileValue (toBytes "/files/a\n/files/a\n/files/b\n")))
                    )
                  , ( toSomeValue (ReadFileKey "/files/a")
                    , hash (toSomeValue (ReadFileValue (toBytes "AAAA\n")))
                    )
                  , ( toSomeValue (ReadFileKey "/files/b")
                    , hash (toSomeValue (ReadFileValue (toBytes "BBBB\n")))
                    )
                  ]
              , value = toSomeValue (ConcatValue (toBytes "AAAA\nAAAA\nBBBB\n"))
              }
          ]
    actualTraces :: [Trace SomeValue SomeValue] <- fetchTraces connection Nothing
    assertEqual "traces 1" expectedTraces actualTraces

    -- 3 `readFile`s, 1 `concat`
    taskCount <- readTVarIO buildState.debugTaskCount
    assertEqual "task count 1" taskCount 4

    taskState' <- newTaskState' connection
    let buildState' = taskState.buildState

    -- Second run should produce the same results...

    actualResult' <- realize Concat taskState' path
    assertEqual "result 2" expectedResult actualResult'

    actualStore' <- readTVarIO buildState'.store
    assertEqual "store 2" expectedStore actualStore'

    actualDone' <- do
      done <- readTVarIO buildState'.done
      forM done \tmvar -> do
        isEmpty <- atomically $ isEmptyTMVar tmvar
        pure (not isEmpty)
    assertEqual "done 2" expectedDone actualDone'

    actualTraces' :: [Trace SomeValue SomeValue] <- fetchTraces connection Nothing
    assertEqual "traces 2" expectedTraces actualTraces'

    -- ...but not run any non-volatile tasks, because they're cached.
    -- 3 `readFile`s (volatile), 1 `concat` (non-volatile)
    taskCount' <- readTVarIO buildState'.debugTaskCount
    assertEqual "task count 2" taskCount' 3

    pure ()
