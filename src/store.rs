use std::{convert::Infallible, hash::Hash};

pub trait Store {
    type Key;

    type Value;

    type Error;

    async fn get(&self, key: &Self::Key) -> Result<Option<Self::Value>, Self::Error>;

    async fn insert(&self, key: Self::Key, value: Self::Value) -> Result<(), Self::Error>;
}

pub struct HashMapStore<K, V>(papaya::HashMap<K, V>);

impl<K, V> Store for HashMapStore<K, V>
where
    K: Eq + Hash,
    V: Clone,
{
    type Key = K;

    type Value = V;

    type Error = Infallible;

    async fn get(&self, key: &Self::Key) -> Result<Option<Self::Value>, Self::Error> {
        match self.0.pin().get(key) {
            Some(value) => Ok(Some(value.clone())),
            None => Ok(None),
        }
    }

    async fn insert(&self, key: Self::Key, value: Self::Value) -> Result<(), Self::Error> {
        self.0.pin().insert(key, value);
        Ok(())
    }
}

pub struct SqliteStore<'a> {
    sqlite: &'a sqlx::SqlitePool,
}

impl<'a> SqliteStore<'a> {
    pub fn new(sqlite: &'a sqlx::SqlitePool) -> Self {
        Self { sqlite }
    }

    pub async fn migrate(&self) -> sqlx::Result<()> {
        sqlx::query(
            "
            create table if not exists store (
                key blob primary key,
                value blob not null
            ) strict;
            ",
        )
        .execute(self.sqlite)
        .await?;
        Ok(())
    }
}

impl Store for SqliteStore<'_> {
    type Key = Vec<u8>;

    type Value = Vec<u8>;

    type Error = anyhow::Error;

    async fn get(&self, key: &Self::Key) -> Result<Option<Self::Value>, Self::Error> {
        let bytes = sqlx::query_scalar("select value from store where key = $1")
            .bind(key)
            .fetch_optional(self.sqlite)
            .await?;
        Ok(bytes)
    }

    async fn insert(&self, key: Self::Key, value: Self::Value) -> Result<(), Self::Error> {
        sqlx::query("insert into store (key, value) values ($1, $2)")
            .bind(key)
            .bind(value)
            .execute(self.sqlite)
            .await?;
        Ok(())
    }
}
