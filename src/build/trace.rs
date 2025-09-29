use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    hash::{BuildHasherDefault, Hash, Hasher},
};
use twox_hash::XxHash3_64;

pub trait Key: for<'de> Deserialize<'de> + Clone + Hash + Ord + Serialize {}

impl<T> Key for T where T: for<'de> Deserialize<'de> + Clone + Hash + Ord + Serialize {}

pub trait Value: for<'de> Deserialize<'de> + Clone + Eq + Hash + Serialize {}

impl<T> Value for T where T: for<'de> Deserialize<'de> + Clone + Eq + Hash + Serialize {}

#[derive(Debug, PartialEq)]
pub struct Trace<K, V>
where
    K: Key,
    V: Value,
{
    pub key: K,
    pub deps: HashMap<K, u64, BuildHasherDefault<XxHash3_64>>,
    pub value: V,
}

impl<K, V> Hash for Trace<K, V>
where
    K: Key,
    V: Value,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.hash(state);
        let mut deps = Vec::from_iter(&self.deps);
        deps.sort_unstable();
        for dep in &deps {
            dep.hash(state);
        }
        self.value.hash(state);
    }
}
