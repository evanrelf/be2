module Be.TraceTest where

import Be.Hash (Hash (..))
import Be.Trace
import Data.Map.Strict qualified as Map
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (trace)
import Test.Tasty.HUnit

unit_trace_roundtrip :: Assertion
unit_trace_roundtrip =
  SQLite.withConnection ":memory:" \connection -> do
    dbMigrate connection

    let key = "password"
    let deps = Map.singleton "answer" (Hash 42)
    let value = "hunter2"
    let trace :: Trace Text Text
        trace = Trace{ key, deps, value }

    -- Traces are de-duplicated
    traceId1 <- insertTrace connection trace
    traceId2 <- insertTrace connection trace
    traceId1 @=? traceId2

    let expectedTraces = [trace]
    actualTraces <- fetchTraces connection Nothing
    expectedTraces @=? actualTraces
