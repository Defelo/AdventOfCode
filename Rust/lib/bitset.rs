use std::{
    iter,
    ops::{BitAnd, BitOr, Sub},
};

use itertools::Itertools;

#[derive(Debug, Clone, Default)]
pub struct BitSet {
    bits: Vec<u64>,
    len: usize,
}

impl BitSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn contains(&self, x: usize) -> bool {
        let (i, j) = key(x);
        i < self.bits.len() && self.bits[i] & j != 0
    }

    pub fn insert(&mut self, x: usize) {
        let (i, j) = key(x);
        self.extend_to_len(i + 1);
        if self.bits[i] & j == 0 {
            self.len += 1;
        }
        self.bits[i] |= j;
    }

    pub fn remove(&mut self, x: usize) {
        let (i, j) = key(x);
        if i < self.bits.len() {
            if self.bits[i] & j != 0 {
                self.len -= 1;
            }
            self.bits[i] &= !j;
        }
    }

    pub fn shrink(&mut self) {
        while self.bits.last() == Some(&0) {
            self.bits.pop();
        }
        self.bits.shrink_to_fit();
    }

    fn extend_to_len(&mut self, len: usize) {
        if len > self.bits.len() {
            self.bits
                .extend(iter::repeat(0).take(len - self.bits.len()));
        }
    }

    pub fn iter(&self) -> BitSetIter<'_> {
        self.into_iter()
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl BitAnd<&Self> for BitSet {
    type Output = Self;

    fn bitand(mut self, rhs: &Self) -> Self::Output {
        let mut removed = self
            .bits
            .iter()
            .skip(rhs.bits.len())
            .map(|x| x.count_ones())
            .sum::<u32>();
        self.bits.truncate(rhs.bits.len());
        for (a, &b) in self.bits.iter_mut().zip(&rhs.bits) {
            removed += (*a & !b).count_ones();
            *a &= b;
        }
        self.len -= removed as usize;
        self
    }
}

impl BitOr<&Self> for BitSet {
    type Output = Self;

    fn bitor(mut self, rhs: &Self) -> Self::Output {
        self.extend_to_len(rhs.bits.len());
        let mut added = 0;
        for (a, &b) in self.bits.iter_mut().zip(&rhs.bits) {
            added += (!*a & b).count_ones();
            *a |= b;
        }
        self.len += added as usize;
        self
    }
}

impl Sub<&Self> for BitSet {
    type Output = Self;

    fn sub(mut self, rhs: &Self) -> Self::Output {
        let mut removed = 0;
        for (a, &b) in self.bits.iter_mut().zip(&rhs.bits) {
            removed += (*a & b).count_ones();
            *a &= !b;
        }
        self.len -= removed as usize;
        self
    }
}

impl PartialEq for BitSet {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            return false;
        }
        self.bits.iter().zip_longest(&other.bits).all(|x| {
            let (a, b) = x.left_and_right();
            a.unwrap_or(&0) == b.unwrap_or(&0)
        })
    }
}

impl Eq for BitSet {}

impl FromIterator<usize> for BitSet {
    fn from_iter<T: IntoIterator<Item = usize>>(iter: T) -> Self {
        let mut set = Self::new();
        for x in iter {
            set.insert(x);
        }
        set
    }
}

pub struct BitSetIter<'a> {
    i: usize,
    j: usize,
    set: &'a BitSet,
}

impl<'a> IntoIterator for &'a BitSet {
    type Item = <Self::IntoIter as Iterator>::Item;

    type IntoIter = BitSetIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        BitSetIter {
            i: 0,
            j: 0,
            set: self,
        }
    }
}

impl Iterator for BitSetIter<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        while *self.set.bits.get(self.i)? >> self.j == 0 {
            self.i += 1;
            self.j = 0;
        }

        while self.set.bits[self.i] & (1 << self.j) == 0 {
            self.j += 1;
        }

        let x = (self.i << 6) | self.j;
        self.j += 1;
        if self.j == 64 {
            self.j = 0;
            self.i += 1;
        }
        Some(x)
    }
}

fn key(x: usize) -> (usize, u64) {
    (x >> 6, 1 << (x & 0x3f))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set() {
        let mut set = BitSet::new();
        assert_eq!(set.iter().collect::<Vec<_>>(), []);
        assert!(!set.contains(1));
        assert!(!set.contains(2));
        assert!(!set.contains(3));
        assert!(!set.contains(1337));
        assert_eq!(set.len(), 0);
        set.insert(1);
        set.insert(2);
        set.insert(1337);
        set.insert(3);
        assert!(set.contains(1));
        assert!(set.contains(2));
        assert!(set.contains(3));
        assert!(set.contains(1337));
        assert_eq!(set.len(), 4);
        assert_eq!(set.iter().collect::<Vec<_>>(), [1, 2, 3, 1337]);
        set.remove(2);
        assert_eq!(set.len(), 3);
        assert_eq!(set.iter().collect::<Vec<_>>(), [1, 3, 1337]);
        set.remove(1337);
        assert_eq!(set.len(), 2);
        assert_eq!(set, BitSet::from_iter([1, 3]));
        assert_ne!(set.bits, BitSet::from_iter([1, 3]).bits);
        assert_eq!(set.bits.len(), 21);
        set.shrink();
        assert_eq!(set.bits.len(), 1);
        assert_eq!(set.iter().collect::<Vec<_>>(), [1, 3]);
    }

    #[test]
    fn ops() {
        let a = BitSet::from_iter([1, 2, 3]);
        let b = BitSet::from_iter([3, 4]);

        assert_eq!(a.clone() & &b, BitSet::from_iter([3]));
        assert_eq!((a.clone() & &b).len(), 1);
        assert_eq!(a.clone() | &b, BitSet::from_iter([1, 2, 3, 4]));
        assert_eq!((a.clone() | &b).len(), 4);
        assert_eq!(a.clone() - &b, BitSet::from_iter([1, 2]));
        assert_eq!((a.clone() - &b).len(), 2);
    }
}
