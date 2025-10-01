use std::hash::{Hash, Hasher as _};
use twox_hash::XxHash3_64;

use camino::Utf8Path;
use sqlx::{
    SqlitePool,
    sqlite::{SqliteConnectOptions, SqliteJournalMode, SqliteSynchronous},
};
use std::str::FromStr as _;

pub trait Xxhash {
    fn xxhash(&self) -> u64;
}

impl<T> Xxhash for T
where
    T: Hash,
{
    fn xxhash(&self) -> u64 {
        let mut hasher = XxHash3_64::default();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

pub async fn sqlite_connect(path: &Utf8Path) -> sqlx::Result<SqlitePool> {
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

pub async fn flatten<T, E1, E2>(
    future: impl Future<Output = Result<Result<T, E2>, E1>>,
) -> Result<T, E2>
where
    E1: std::error::Error,
    E2: From<E1>,
{
    future.await?
}
