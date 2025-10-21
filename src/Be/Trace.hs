{-# LANGUAGE QuasiQuotes #-}

module Be.Trace
  ( Key
  , Value
  , Trace (..)
  , dbMigrate
  , fetchTraces
  , insertTrace
  )
where

import Be.Hash (Hash (..), hash)
import Codec.Serialise (Serialise, deserialise, serialise)
import Control.Exception (assert, onException)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (iii)
import Database.SQLite.Simple qualified as Sqlite
import Prelude hiding (trace, traceId)

type Key a = (Ord a, Serialise a)

type Value a = Serialise a

data Trace k v = Trace
  { key :: k
  , deps :: Map k Hash
  , value :: v
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Serialise)

dbMigrate :: Sqlite.Connection -> IO ()
dbMigrate connection = Sqlite.withTransaction connection do
  Sqlite.execute_ connection [iii|
    create table if not exists traces (
      id integer primary key,
      key blob not null,
      value blob not null,
      trace_hash blob not null unique
    ) strict
  |]
  Sqlite.execute_ connection [iii|
    create table if not exists trace_deps (
      trace_id integer not null references traces on delete cascade,
      dep_key blob not null,
      dep_value_hash blob not null,
      unique (trace_id, dep_key)
    ) strict
  |]
  Sqlite.execute_ connection [iii|
    create index if not exists idx_traces_key on traces(key)
  |]
  Sqlite.execute_ connection [iii|
    create trigger if not exists forbid_trace_update
    before update on traces
    begin
      select raise(abort, 'traces are immutable');
    end
  |]
  Sqlite.execute_ connection [iii|
    create trigger if not exists forbid_trace_deps_update
    before update on trace_deps
    begin
      select raise(abort, 'trace dependencies are immutable');
    end
  |]
  Sqlite.execute_ connection [iii|
    create trigger if not exists forbid_trace_deps_delete
    before delete on trace_deps
    when (select count(*) from traces where id = old.trace_id) > 0
    begin
      select raise(abort, 'trace dependencies cannot be deleted directly');
    end
  |]

fetchTraces
  :: (Key k, Value v)
  => Sqlite.Connection -> Maybe k -> IO [Trace k v]
fetchTraces connection mKey = Sqlite.withTransaction connection do
  traceRows :: [(Int64, LByteString, LByteString, LByteString)] <-
    case mKey of
      Just key -> do
        let keyBytes = serialise key
        Sqlite.query
          connection
          "select id, key, value, trace_hash from traces where key = ?"
          (Sqlite.Only keyBytes)
      Nothing ->
        Sqlite.query_
          connection
          "select id, key, value, trace_hash from traces"

  forM traceRows \traceRow -> do
    let (traceId, traceKeyBytes, traceValueBytes, traceHashBytes) = traceRow

    let traceKey = deserialise traceKeyBytes

    whenJust mKey \key ->
      assert (key == traceKey) $ pure ()

    let traceValue = deserialise traceValueBytes

    depsRows :: [(LByteString, LByteString)] <-
      Sqlite.query
        connection
        "select dep_key, dep_value_hash from trace_deps where trace_id = ?"
        (Sqlite.Only traceId)

    deps :: [(k, Hash)] <-
      forM depsRows \(depKeyBytes, depValueHashBytes) -> do
        let depKey = deserialise depKeyBytes
        let depValueHash = deserialise depValueHashBytes
        pure (depKey, depValueHash)

    let traceHash = deserialise traceHashBytes

    let trace = Trace
          { key = traceKey
          , deps = Map.fromList deps
          , value = traceValue
          }

    assert (hash trace == traceHash) $ pure ()

    pure trace

insertTrace :: (Key k, Value v) => Sqlite.Connection -> Trace k v -> IO Int64
insertTrace connection trace =
  flip onException (Sqlite.execute_ connection "rollback") do
    Sqlite.execute_ connection "begin"

    let keyBytes = serialise trace.key
    let valueBytes = serialise trace.value
    let traceHashBytes = serialise (hash trace)

    Sqlite.execute
      connection
      "insert or ignore into traces (key, value, trace_hash) values (?, ?, ?)"
      (keyBytes, valueBytes, traceHashBytes)

    rows :: [(Int64, Bool)] <-
      Sqlite.query
        connection
        "select id, changes() == 0 as is_dupe from traces where trace_hash = ?"
        (Sqlite.Only traceHashBytes)

    (traceId, isDupe) <-
      case rows of
        [row] -> pure row
        [] -> error "No rows"
        _ : _ : _ -> error "More than one row"

    if isDupe then do
      putStrLn $ "warn: Trace " <> show traceId <> " already exists in database"
      Sqlite.execute_ connection "rollback"
      pure traceId

    else do
      for_ (Map.assocs trace.deps) \(depKey, depValueHash) -> do
        let depKeyBytes = serialise depKey
        let depValueHashBytes = serialise depValueHash

        Sqlite.execute
          connection
          "insert into trace_deps values (?, ?, ?)"
          (traceId, depKeyBytes, depValueHashBytes)

      Sqlite.execute_ connection "commit"

      pure traceId
