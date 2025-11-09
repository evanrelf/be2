{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Be.Core.Build.DynamicTest where

import Be.Core.Build.Dynamic
import Be.Core.Build.Static (BuildState (..), TaskState (..))
import Be.Core.Hash (Hash (..))
import Be.Core.Trace (Trace (..), dbCreate, dbDrop, fetchTraces)
import Be.Core.Value (SomeValue, Value, toSomeValue)
import Control.Exception.Annotated.UnliftIO qualified as Exception
import Data.HashMap.Strict qualified as HashMap
import Data.List (isInfixOf)
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (concat, readFile)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.HUnit hiding (assertEqual)
import Test.Tasty.HUnit qualified as HUnit
import UnliftIO.Async (forConcurrently, mapConcurrently)

readFile :: FilePath -> Build ByteString
readFile path = do
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

concat :: FilePath -> Build ByteString
concat path = do
  bytes <- realize ReadFile path
  let text = decodeUtf8 bytes
  let paths = map toString (lines text)
  output <- foldMapM (realize ReadFile) paths
  pure output

registerTask 'concat

-- Tasks for exception handling test

failingTask :: Int -> Build Int
failingTask n = do
  when (n < 0) $ error "negative number not allowed"
  pure (n * 2)

registerTask 'failingTask

successfulTask :: Int -> Build Int
successfulTask n = do
  x <- realize FailingTask n
  pure (x + 1)

registerTask 'successfulTask

-- Initialize all instances once at module load time
_initBuild :: IO ()
_initBuild = $$initBuild
{-# NOINLINE _initBuild #-}

_initialized :: ()
_initialized = unsafePerformIO _initBuild
{-# NOINLINE _initialized #-}

unit_build_system_dynamic :: Assertion
unit_build_system_dynamic = do
  -- Force initialization
  _initialized `seq` pure ()

  let toBytes :: Text -> ByteString
      toBytes = encodeUtf8

  SQLite.withConnection ":memory:" \connection -> do
    dbDrop connection
    dbCreate connection

    let path = "/files"

    (expectedResult, expectedStore, expectedDone, expectedTraces) <- runBuild connection do
      taskState <- Build ask
      let buildState = taskState.buildState

      actualResult <- realize Concat path
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
                      , Hash 4667841580189462423
                      )
                    , ( toSomeValue (ReadFileKey "/files/a")
                      , Hash 16623056052819200894
                      )
                    , ( toSomeValue (ReadFileKey "/files/b")
                      , Hash 14687783424159735648
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

      pure (expectedResult, expectedStore, expectedDone, expectedTraces)

    runBuild connection do
      taskState <- Build ask
      let buildState = taskState.buildState

      -- Second run should produce the same results...

      actualResult' <- realize Concat path
      assertEqual "result 2" expectedResult actualResult'

      actualStore' <- readTVarIO buildState.store
      assertEqual "store 2" expectedStore actualStore'

      actualDone' <- do
        done <- readTVarIO buildState.done
        forM done \tmvar -> do
          isEmpty <- atomically $ isEmptyTMVar tmvar
          pure (not isEmpty)
      assertEqual "done 2" expectedDone actualDone'

      actualTraces' :: [Trace SomeValue SomeValue] <- fetchTraces connection Nothing
      assertEqual "traces 2" expectedTraces actualTraces'

      -- ...but not run any non-volatile tasks, because they're cached.
      -- 3 `readFile`s (volatile), 1 `concat` (non-volatile)
      taskCount' <- readTVarIO buildState.debugTaskCount
      assertEqual "task count 2" taskCount' 3

      pure ()

assertEqual :: (Eq a, Show a, MonadIO m) => String -> a -> a -> m ()
assertEqual x y z = liftIO $ HUnit.assertEqual x y z

unit_handle_exceptions :: Assertion
unit_handle_exceptions = do
  -- Force initialization
  _initialized `seq` pure ()

  SQLite.withConnection ":memory:" \connection -> do
    dbDrop connection
    dbCreate connection

    -- Test 1: Exception propagates correctly
    result1 <- Exception.try @SomeException $ runBuild connection do
      realize FailingTask (-5)

    case result1 of
      Left exception -> do
        let msg = displayException exception
        HUnit.assertBool "exception message contains 'negative'" $
          "negative" `isInfixOf` msg
      Right _ -> HUnit.assertFailure "Expected exception but got success"

    -- Test 2: Exception is not persisted to trace database
    traces1 :: [Trace SomeValue SomeValue] <- fetchTraces connection Nothing
    assertEqual "no traces after exception" [] traces1

    -- Test 3: Concurrent waiters all get the exception (CRITICAL TEST)
    -- This tests that if a task throws, the barrier is properly filled with the exception
    -- so all waiting threads get notified instead of hanging forever.
    -- Without proper exception handling, this test would HANG because the first thread
    -- throws an exception without filling the barrier, and the other 4 threads wait forever.
    result3 <- Exception.try @SomeException $ runBuild connection do
      taskState <- Build ask

      -- Launch 5 concurrent realize calls for the SAME failing key
      -- They all share the same BuildState/TaskState within this runBuild session
      results <- liftIO $ mapConcurrently
        (\_ -> Exception.try @SomeException $ unwrapBuild (realize FailingTask (-20)) taskState)
        [1..5 :: Int]

      -- All should have gotten exceptions (not hung waiting)
      let failureCount = length [() | Left _ <- results]
      liftIO $ assertEqual "all 5 threads got exceptions" 5 failureCount

      -- If we got here, the test passed (no hang)
      pure results

    -- The test itself shouldn't fail at the top level since we caught exceptions inside
    case result3 of
      Right _ -> pure ()  -- Expected - we caught all exceptions inside
      Left _ -> HUnit.assertFailure "Unexpected outer exception"

    -- Test 4: Failed keys are not in the store
    storeAfterFailure <- runBuild connection do
      taskState <- Build ask
      readTVarIO taskState.buildState.store

    let failingKey = toSomeValue (FailingTaskKey (-20))
    assertEqual "failing key not in store" Nothing (HashMap.lookup failingKey storeAfterFailure)

    -- Test 5: Exception is not cached across build sessions (retry works)
    -- First session: task fails
    result2 <- Exception.try @SomeException $ runBuild connection do
      _ <- realize FailingTask (-7)
      pure ()

    case result2 of
      Left _ -> pure ()
      Right _ -> HUnit.assertFailure "Expected first attempt to fail"

    -- Verify no trace was written
    traces1b :: [Trace SomeValue SomeValue] <- fetchTraces connection Nothing
    assertEqual "still no traces after another exception" [] traces1b

    -- Second session: same task should be attempted again (not cached)
    resultRetry <- Exception.try @SomeException $ runBuild connection do
      _ <- realize FailingTask (-7)
      pure ()

    case resultRetry of
      Left _ -> pure ()  -- Still fails, as expected
      Right _ -> HUnit.assertFailure "Expected second attempt to also fail"

    -- Test 6: Successful tasks are unaffected by other tasks failing
    result4 <- runBuild connection do
      taskState <- Build ask
      let buildState = taskState.buildState

      -- This should succeed
      success <- realize FailingTask 10

      -- Check it's in the store
      store <- readTVarIO buildState.store
      let successKey = toSomeValue (FailingTaskKey 10)
      let successValue = toSomeValue (FailingTaskValue 20)
      assertEqual "successful key in store" (Just successValue) (HashMap.lookup successKey store)

      pure success

    assertEqual "successful task result" 20 result4

    -- Trace should be written for successful task
    traces2 :: [Trace SomeValue SomeValue] <- fetchTraces connection Nothing
    HUnit.assertBool "at least one trace exists" (not (null traces2))
