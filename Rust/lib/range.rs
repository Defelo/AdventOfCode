use std::ops::{Add, Sub};

use num::One;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Range<T> {
    pub start: T,
    pub end: T,
}

pub enum RangeRel {
    LeftOf,
    RightOf,
    ContainedIn,
    Contains,
    ContainsStartOf,
    ContainsEndOf,
}

impl<T> From<Range<T>> for std::ops::Range<T> {
    fn from(value: Range<T>) -> Self {
        value.start..value.end
    }
}

impl<T> From<std::ops::Range<T>> for Range<T> {
    fn from(value: std::ops::Range<T>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl<T: Sub<Output = T> + One> From<Range<T>> for std::ops::RangeInclusive<T> {
    fn from(value: Range<T>) -> Self {
        value.start..=value.end - T::one()
    }
}

impl<T: Add<Output = T> + One> From<std::ops::RangeInclusive<T>> for Range<T> {
    fn from(value: std::ops::RangeInclusive<T>) -> Self {
        let (start, end) = value.into_inner();
        Self {
            start,
            end: end + T::one(),
        }
    }
}

impl<T> Range<T> {
    pub fn new(start: T, end: T) -> Self {
        Self { start, end }
    }

    pub fn new_inclusive(start: T, end: T) -> Self
    where
        T: Add<Output = T> + One,
    {
        Self::new(start, end + T::one())
    }

    pub fn into_std(self) -> std::ops::Range<T> {
        self.into()
    }

    pub fn contains<U>(&self, item: &U) -> bool
    where
        T: PartialOrd<U>,
        U: PartialOrd<T>,
    {
        &self.start <= item && item < &self.end
    }

    pub fn rel(&self, other: &Range<T>) -> RangeRel
    where
        T: PartialOrd,
    {
        debug_assert!(self.start <= self.end);
        debug_assert!(other.start <= other.end);
        match () {
            _ if self.end <= other.start => RangeRel::LeftOf,
            _ if self.start >= other.end => RangeRel::RightOf,
            _ if self.start >= other.start && self.end <= other.end => RangeRel::ContainedIn,
            _ if self.start <= other.start && self.end >= other.end => RangeRel::Contains,
            _ if self.contains(&other.start) => RangeRel::ContainsStartOf,
            _ if self.contains(&other.end) => RangeRel::ContainsEndOf,
            _ => unreachable!(),
        }
    }

    pub fn overlaps(&self, other: &Range<T>) -> bool
    where
        T: PartialOrd,
    {
        !matches!(self.rel(other), RangeRel::LeftOf | RangeRel::RightOf)
    }

    pub fn add<N>(&self, n: N) -> Range<<T as Add<N>>::Output>
    where
        T: Copy + Add<N>,
        N: Copy,
    {
        Range {
            start: self.start + n,
            end: self.end + n,
        }
    }

    pub fn split_at(&self, at: T) -> (Option<Range<T>>, Option<Range<T>>)
    where
        T: Copy + Ord,
    {
        (
            (self.start < at).then_some((self.start..at.min(self.end)).into()),
            (at < self.end).then_some((at.max(self.start)..self.end).into()),
        )
    }

    pub fn intersect(&self, other: &Range<T>) -> Option<Range<T>>
    where
        T: Copy + Ord,
    {
        debug_assert!(self.start <= self.end);
        debug_assert!(other.start <= other.end);
        let start = self.start.max(other.start);
        let end = self.end.min(other.end);
        (start < end).then_some((start..end).into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(Range::from(10..20).add(5), Range::from(15..25));
    }

    #[test]
    fn test_split_at() {
        assert_eq!(
            Range::from(5..15).split_at(10),
            (Some(Range::from(5..10)), Some(Range::from(10..15)))
        );
        assert_eq!(
            Range::from(5..15).split_at(20),
            (Some(Range::from(5..15)), None)
        );
        assert_eq!(
            Range::from(5..15).split_at(0),
            (None, Some(Range::from(5..15)))
        );
        assert_eq!(
            Range::from(5..15).split_at(15),
            (Some(Range::from(5..15)), None)
        );
        assert_eq!(
            Range::from(5..15).split_at(5),
            (None, Some(Range::from(5..15)))
        );
    }

    #[test]
    fn test_intersect() {
        assert_eq!(
            Range::from(5..15).intersect(&(Range::from(10..20))),
            Some(Range::from(10..15))
        );
        assert_eq!(
            Range::from(10..20).intersect(&(Range::from(5..15))),
            Some(Range::from(10..15))
        );
        assert_eq!(
            Range::from(10..20).intersect(&(Range::from(5..25))),
            Some(Range::from(10..20))
        );
        assert_eq!(
            Range::from(5..25).intersect(&(Range::from(10..20))),
            Some(Range::from(10..20))
        );
        assert_eq!(Range::from(10..15).intersect(&(Range::from(20..25))), None);
        assert_eq!(Range::from(20..25).intersect(&(Range::from(10..15))), None);
    }
}
