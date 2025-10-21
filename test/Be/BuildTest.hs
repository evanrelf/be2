{-# LANGUAGE TypeFamilies #-}

module Be.BuildTest where

import Be.Build
import Be.Trace
import Codec.Serialise (Serialise)
import Data.Map.Strict qualified as Map
import Database.SQLite.Simple qualified as Sqlite
import Prelude hiding (concat, readFile)
import Prelude qualified
import Test.Tasty.HUnit

data TestKey
  = ReadFile FilePath
  | Concat FilePath
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (Serialise)

data TestValue
  = Bytes ByteString
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise)

data TestBuildSystem

instance BuildSystem TestBuildSystem where
  type Key _ = TestKey

  type Value _ = TestValue

  tasks taskContext = \case
    ReadFile path -> do
      bytes <- taskReadFile taskContext path
      let value = Bytes bytes
      let volatile = True
      pure (value, volatile)

    Concat path -> do
      bytes <- taskConcat taskContext path
      let value = Bytes bytes
      let volatile = False
      pure (value, volatile)

readFile :: TaskContext TestBuildSystem -> FilePath -> IO ByteString
readFile taskContext path = do
  let key = ReadFile path
  value <- taskContextRealize taskContext key
  let Bytes bytes = value
  pure bytes

taskReadFile :: TaskContext TestBuildSystem -> FilePath -> IO ByteString
taskReadFile taskContext path = do
  let toBytes :: Text -> ByteString
      toBytes = encodeUtf8
  let bytes = case path of
        "/files" -> toBytes "/files/a\n/files/a\n/files/b\n"
        "/files/a" -> toBytes "AAAA\n"
        "/files/b" -> toBytes "BBBB\n"
        "/dev/null" -> toBytes ""
        _ -> error $ "Failed to read file at '" <> toText path <> "'"
  pure bytes

concat :: TaskContext TestBuildSystem -> FilePath -> IO ByteString
concat taskContext path = do
  let key = Concat path
  value <- taskContextRealize taskContext key
  let Bytes bytes = value
  pure bytes

taskConcat :: TaskContext TestBuildSystem -> FilePath -> IO ByteString
taskConcat taskContext path = do
  bytes <- readFile taskContext path
  let text = decodeUtf8 bytes
  let paths = map toString (lines text)
  output <- foldMapM (readFile taskContext) paths
  pure output

unit_test :: Assertion
unit_test = do
  let toBytes :: Text -> ByteString
      toBytes = encodeUtf8

  Sqlite.withConnection ":memory:" \connection -> do
    dbMigrate connection

    state <- atomically $ newState @TestBuildSystem connection

    let path = "/files"

    actualResult <- stateRealize state (Concat path)
    let expectedResult = Bytes (toBytes "AAAA\nAAAA\nBBBB\n")
    expectedResult @=? actualResult

    let expectedStore = Map.fromList
          [ ( ReadFile "/files"
            , Bytes (toBytes "/files/a\n/files/a\n/files/b\n")
            )
          , ( ReadFile "/files/a"
            , Bytes (toBytes "AAAA\n")
            )
          , ( ReadFile "/files/b"
            , Bytes (toBytes "BBBB\n")
            )
          , ( Concat "/files"
            , Bytes (toBytes "AAAA\nAAAA\nBBBB\n")
            )
          ]
    actualStore <- readTVarIO state.store
    expectedStore @=? actualStore

    -- TODO: Convert `Map (Key b) (TMVar ())` to `Map (Key b) Bool` so the
    -- assert has a `Show` instance.

    -- let expectedDone = undefined
    -- actualDone <- readTVarIO state.done
    -- expectedDone @=? actualDone

    let expectedTraces =
          [ Trace
              { key = Concat "/files"
              , deps = undefined
              , value = Bytes (toBytes "AAAA\nAAAA\nBBBB\n")
              }
          ]
    actualTraces :: [Trace TestKey TestValue] <- fetchTraces connection Nothing
    expectedTraces @=? actualTraces

    -- TODO: Assert `debug_task_count == 4` (3 read files, 1 concat).

    -- TODO: Run it again, check that result + store + done + traces are same
    -- but `debug_task_count == 3` (3 read files, 0 concat).

    pure ()
