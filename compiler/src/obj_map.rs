//! Open-addressed map from interned object-id keys to values.
//!
//! Layout mirrors clox `Table`: power-of-two capacity, linear probing, tombstones.
//! Chosen over `HashMap` because Lox field/method tables stay small and the
//! hash of an interned id is essentially free.

use crate::value::LoxValue;

/// Same representation as [`crate::object::ObjId`].
type ObjId = u32;

/// Reserved key meaning an unused / never-occupied slot.
const EMPTY_KEY: ObjId = u32::MAX;
/// Reserved key meaning a deleted slot (keeps linear probe chains intact).
const TOMBSTONE_KEY: ObjId = u32::MAX - 1;

const MIN_CAPACITY: usize = 8;

#[derive(Clone, Copy)]
struct Entry {
    key: ObjId,
    value: LoxValue,
}

impl Entry {
    #[inline(always)]
    const fn empty() -> Self {
        Self {
            key: EMPTY_KEY,
            value: LoxValue::NIL,
        }
    }

    #[inline(always)]
    fn is_empty(self) -> bool {
        self.key == EMPTY_KEY
    }

    #[inline(always)]
    fn is_tombstone(self) -> bool {
        self.key == TOMBSTONE_KEY
    }

    #[inline(always)]
    fn is_live(self) -> bool {
        !self.is_empty() && !self.is_tombstone()
    }
}

/// Compact identity map used for instance fields and class methods.
#[derive(Clone)]
pub struct ObjMap {
    /// Number of live (non-tombstone) entries.
    count: usize,
    entries: Box<[Entry]>,
}

impl Default for ObjMap {
    fn default() -> Self {
        Self::new()
    }
}

impl ObjMap {
    #[must_use]
    pub fn new() -> Self {
        Self {
            count: 0,
            entries: Box::new([]),
        }
    }

    #[inline(always)]
    fn hash(key: ObjId) -> usize {
        // Multiplicative hash; ObjId is already an identity.
        (u64::from(key).wrapping_mul(0x9E37_79B9_7F4A_7C15)) as usize
    }

    #[inline(always)]
    fn find_entry(entries: &[Entry], key: ObjId) -> usize {
        debug_assert!(!entries.is_empty());
        let mask = entries.len() - 1;
        let mut index = Self::hash(key) & mask;
        let mut tombstone: Option<usize> = None;
        loop {
            // SAFETY: `index` stays in range via `& mask`.
            let entry = unsafe { *entries.get_unchecked(index) };
            if entry.is_empty() {
                return tombstone.unwrap_or(index);
            }
            if entry.is_tombstone() {
                if tombstone.is_none() {
                    tombstone = Some(index);
                }
            } else if entry.key == key {
                return index;
            }
            index = (index + 1) & mask;
        }
    }

    #[inline(always)]
    #[must_use]
    pub fn get(&self, key: ObjId) -> Option<&LoxValue> {
        if self.entries.is_empty() {
            return None;
        }
        let index = Self::find_entry(&self.entries, key);
        // SAFETY: find_entry returns an in-range index.
        let entry = unsafe { self.entries.get_unchecked(index) };
        if entry.is_live() && entry.key == key {
            Some(&entry.value)
        } else {
            None
        }
    }

    #[inline(always)]
    #[must_use]
    pub fn contains_key(&self, key: ObjId) -> bool {
        self.get(key).is_some()
    }

    pub fn insert(&mut self, key: ObjId, value: LoxValue) -> Option<LoxValue> {
        debug_assert!(key != EMPTY_KEY && key != TOMBSTONE_KEY);
        if self.count + 1 > self.entries.len().saturating_mul(3) / 4 {
            let capacity = if self.entries.is_empty() {
                MIN_CAPACITY
            } else {
                self.entries.len() * 2
            };
            self.adjust(capacity);
        }

        let index = Self::find_entry(&self.entries, key);
        // SAFETY: find_entry returns an in-range index into `entries`.
        let entry = unsafe { self.entries.get_unchecked_mut(index) };
        if entry.is_live() && entry.key == key {
            let old = entry.value;
            entry.value = value;
            return Some(old);
        }

        // New live entry (empty or tombstone slot).
        if !entry.is_live() {
            self.count += 1;
        }
        entry.key = key;
        entry.value = value;
        None
    }

    fn adjust(&mut self, capacity: usize) {
        debug_assert!(capacity.is_power_of_two());
        let mut new_entries = vec![Entry::empty(); capacity].into_boxed_slice();
        let mut count = 0;
        for entry in self.entries.iter().copied() {
            if !entry.is_live() {
                continue;
            }
            let index = Self::find_entry(&new_entries, entry.key);
            // SAFETY: find_entry returns an in-range index.
            *unsafe { new_entries.get_unchecked_mut(index) } = entry;
            count += 1;
        }
        self.entries = new_entries;
        self.count = count;
    }

    pub fn values(&self) -> impl Iterator<Item = &LoxValue> {
        self.entries
            .iter()
            .filter(|entry| entry.is_live())
            .map(|entry| &entry.value)
    }

    pub fn iter(&self) -> impl Iterator<Item = (ObjId, LoxValue)> + '_ {
        self.entries
            .iter()
            .filter(|entry| entry.is_live())
            .map(|entry| (entry.key, entry.value))
    }
}

impl IntoIterator for ObjMap {
    type Item = (ObjId, LoxValue);
    type IntoIter = std::vec::IntoIter<(ObjId, LoxValue)>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries
            .iter()
            .filter(|entry| entry.is_live())
            .map(|entry| (entry.key, entry.value))
            .collect::<Vec<_>>()
            .into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test]
    fn insert_get_overwrite() {
        // Arrange
        let mut map = ObjMap::new();

        // Act
        assert!(map.insert(1, LoxValue::number(10.0)).is_none());
        assert!(map.insert(2, LoxValue::number(20.0)).is_none());
        let old = map.insert(1, LoxValue::number(11.0));

        // Assert
        assert_eq!(old, Some(LoxValue::number(10.0)));
        assert_eq!(map.get(1).copied(), Some(LoxValue::number(11.0)));
        assert_eq!(map.get(2).copied(), Some(LoxValue::number(20.0)));
        assert!(map.get(3).is_none());
    }

    #[test_case(0usize; "empty")]
    #[test_case(7usize; "below_min_capacity")]
    #[test_case(8usize; "exact_min_capacity")]
    #[test_case(64usize; "grows_past_load_factor")]
    fn insert_many_round_trip(n: usize) {
        // Arrange
        let mut map = ObjMap::new();

        // Act
        for i in 0..n {
            let key = u32::try_from(i).expect("test size fits u32");
            assert!(map.insert(key, LoxValue::number(f64::from(key))).is_none());
        }

        // Assert
        for i in 0..n {
            let key = u32::try_from(i).expect("test size fits u32");
            assert_eq!(
                map.get(key).copied(),
                Some(LoxValue::number(f64::from(key)))
            );
        }
        assert_eq!(map.iter().count(), n);
    }
}
