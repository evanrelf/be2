use std::hash::{Hash, Hasher as _};
use twox_hash::XxHash3_64;

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
