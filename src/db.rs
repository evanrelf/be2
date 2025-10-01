use camino::Utf8Path;
use sqlx::{
    SqlitePool,
    sqlite::{SqliteConnectOptions, SqliteJournalMode, SqliteSynchronous},
};
use std::str::FromStr as _;

pub async fn connect(path: &Utf8Path) -> sqlx::Result<SqlitePool> {
    let sqlite = SqlitePool::connect_with(
        SqliteConnectOptions::from_str(&format!("sqlite://{path}"))?
            .create_if_missing(true)
            .journal_mode(SqliteJournalMode::Wal)
            // Higher performance, at the cost of durability. Does not call `fsync` after every
            // transaction, so data loss is possible if the machine loses power and the OS hasn't
            // flushed modifications to disk. This is an acceptable tradeoff for a cache like this.
            .synchronous(SqliteSynchronous::Normal),
    )
    .await?;

    Ok(sqlite)
}

pub async fn migrate(db: &SqlitePool) -> sqlx::Result<()> {
    sqlx::query(
        "
        create table if not exists traces (
            id integer primary key,
            key blob not null,
            value blob not null,
            trace_hash blob not null unique
        ) strict;

        create table if not exists trace_deps (
            trace_id integer not null references traces on delete cascade,
            dep_key blob not null,
            dep_value_hash blob not null,
            unique (trace_id, dep_key)
        ) strict;

        create index if not exists idx_traces_key on traces(key);

        create trigger if not exists forbid_trace_update
        before update on traces
        begin
            select raise(abort, 'traces are immutable');
        end;

        create trigger if not exists forbid_trace_deps_update
        before update on trace_deps
        begin
            select raise(abort, 'trace dependencies are immutable');
        end;

        create trigger if not exists forbid_trace_deps_delete
        before delete on trace_deps
        when (select count(*) from traces where id = old.trace_id) > 0
        begin
            select raise(abort, 'trace dependencies cannot be deleted directly');
        end;
        ",
    )
    .execute(db)
    .await?;

    Ok(())
}
