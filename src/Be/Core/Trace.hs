{-# LANGUAGE QuasiQuotes #-}

-- | SQLite-backed persistent trace storage for incremental builds.
--
-- A trace records the relationship between a key, its dependencies (with hashes),
-- and the resulting value. Traces are immutable and content-addressed by hash.
--
-- Database schema:
--   * traces: (id, key_blob, value_blob, trace_hash)
--   * trace_deps: (trace_id, dep_key_blob, dep_value_hash_blob)
--
-- Immutability is enforced via SQLite triggers - any attempt to UPDATE traces
-- or trace_deps will raise an error.
--
-- REVIEW: The immutability-via-triggers approach is clever! However, it means
-- errors are discovered at runtime rather than being prevented by the type system.
-- This is probably acceptable given the constraints, but worth noting.
module Be.Core.Trace
  ( Trace (..)
  , dbDrop
  , dbCreate
  , fetchTraces
  , insertTrace
  )
where

import Be.Core.Hash (Hash (..), hash)
import Be.Core.Value (Value)
import Codec.Serialise (Serialise, deserialise, serialise)
import Control.Exception (assert, onException)
import Data.HashMap.Strict qualified as HashMap
import Data.String.Interpolate (iii)
import Database.SQLite.Simple qualified as SQLite
import Prelude hiding (trace, traceId)

data Trace k v = Trace
  { key :: k
  , deps :: HashMap k Hash
  , value :: v
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Serialise)

dbDrop :: MonadIO m => SQLite.Connection -> m ()
dbDrop connection = liftIO $ SQLite.withTransaction connection do
  SQLite.execute_ connection "drop table if exists traces"
  SQLite.execute_ connection "drop table if exists trace_deps"

dbCreate :: MonadIO m => SQLite.Connection -> m ()
dbCreate connection = liftIO $ SQLite.withTransaction connection do
  SQLite.execute_ connection [iii|
    create table traces (
      id integer primary key,
      key blob not null,
      value blob not null,
      trace_hash blob not null unique
    ) strict
  |]
  SQLite.execute_ connection [iii|
    create table trace_deps (
      trace_id integer not null references traces on delete cascade,
      dep_key blob not null,
      dep_value_hash blob not null,
      unique (trace_id, dep_key)
    ) strict
  |]
  SQLite.execute_ connection [iii|
    create index idx_traces_key on traces(key)
  |]
  SQLite.execute_ connection [iii|
    create trigger forbid_trace_update
    before update on traces
    begin
      select raise(abort, 'traces are immutable');
    end
  |]
  SQLite.execute_ connection [iii|
    create trigger forbid_trace_deps_update
    before update on trace_deps
    begin
      select raise(abort, 'trace dependencies are immutable');
    end
  |]
  SQLite.execute_ connection [iii|
    create trigger forbid_trace_deps_delete
    before delete on trace_deps
    when (select count(*) from traces where id = old.trace_id) > 0
    begin
      select raise(abort, 'trace dependencies cannot be deleted directly');
    end
  |]

fetchTraces
  :: (Value k, Value v, MonadIO m)
  => SQLite.Connection -> Maybe k -> m [Trace k v]
fetchTraces connection mKey = liftIO do
  traceRows :: [(Int64, LByteString, LByteString, LByteString)] <-
    case mKey of
      Just key -> do
        let keyBytes = serialise key
        SQLite.query
          connection
          "select id, key, value, trace_hash from traces where key = ?"
          (SQLite.Only keyBytes)
      Nothing ->
        SQLite.query_
          connection
          "select id, key, value, trace_hash from traces"

  forM traceRows \traceRow -> do
    let (traceId, traceKeyBytes, traceValueBytes, traceHashBytes) = traceRow

    let traceKey = deserialise traceKeyBytes

    whenJust mKey \key ->
      assert (key == traceKey) $ pure ()

    let traceValue = deserialise traceValueBytes

    depsRows :: [(LByteString, LByteString)] <-
      SQLite.query
        connection
        "select dep_key, dep_value_hash from trace_deps where trace_id = ?"
        (SQLite.Only traceId)

    deps :: [(k, Hash)] <-
      forM depsRows \(depKeyBytes, depValueHashBytes) -> do
        let depKey = deserialise depKeyBytes
        let depValueHash = deserialise depValueHashBytes
        pure (depKey, depValueHash)

    let traceHash = deserialise traceHashBytes

    let trace = Trace
          { key = traceKey
          , deps = HashMap.fromList deps
          , value = traceValue
          }

    assert (hash trace == traceHash) $ pure ()

    pure trace

-- | Insert a trace into the database (with de-duplication).
--
-- Traces are identified by their content hash. If a trace with the same hash
-- already exists, this function:
-- 1. Prints a warning to stdout
-- 2. Rolls back the transaction
-- 3. Returns the existing trace ID
--
-- This de-duplication prevents storing identical traces multiple times, saving
-- disk space.
--
-- REVIEW: The manual transaction handling here is somewhat fragile. The
-- 'onException' handler ensures rollback on error, but consider using bracket-style
-- resource management or SQLite.withTransaction for more robust cleanup.
--
-- REVIEW: The warning goes to stdout via 'putStrLn'. This appears in test output
-- (see DynamicTest). Consider using a proper logging framework or making this
-- configurable (debug mode?).
--
-- REVIEW: The de-duplication check uses 'changes() == 0', which is a clever way
-- to detect "insert or ignore" conflicts. However, the subsequent query could
-- theoretically fail if the trace was deleted between operations (though triggers
-- prevent this in practice).
insertTrace :: (Value k, Value v, MonadIO m) => SQLite.Connection -> Trace k v -> m Int64
insertTrace connection trace = liftIO do
  flip onException (SQLite.execute_ connection "rollback") do
    SQLite.execute_ connection "begin"

    let keyBytes = serialise trace.key
    let valueBytes = serialise trace.value
    let traceHashBytes = serialise (hash trace)

    SQLite.execute
      connection
      "insert or ignore into traces (key, value, trace_hash) values (?, ?, ?)"
      (keyBytes, valueBytes, traceHashBytes)

    rows :: [(Int64, Bool)] <-
      SQLite.query
        connection
        "select id, changes() == 0 as is_dupe from traces where trace_hash = ?"
        (SQLite.Only traceHashBytes)

    (traceId, isDupe) <-
      case rows of
        [row] -> pure row
        [] -> error "No rows"
        _ : _ : _ -> error "More than one row"

    if isDupe then do
      putStrLn $ "warn: Trace " <> show traceId <> " already exists in database"
      SQLite.execute_ connection "rollback"
      pure traceId

    else do
      for_ (HashMap.toList trace.deps) \(depKey, depValueHash) -> do
        let depKeyBytes = serialise depKey
        let depValueHashBytes = serialise depValueHash

        SQLite.execute
          connection
          "insert into trace_deps values (?, ?, ?)"
          (traceId, depKeyBytes, depValueHashBytes)

      SQLite.execute_ connection "commit"

      pure traceId
