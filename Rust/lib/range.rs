use std::ops::{Add, Range};

pub enum RangeRel {
    LeftOf,
    RightOf,
    ContainedIn,
    Contains,
    ContainsStartOf,
    ContainsEndOf,
}

pub trait RangeExt<T> {
    fn rel(&self, other: &Range<T>) -> RangeRel
    where
        T: PartialOrd;

    fn add<N>(&self, n: N) -> Range<<T as Add<N>>::Output>
    where
        T: Copy + Add<N>,
        N: Copy;

    fn split_at(&self, at: T) -> (Option<Range<T>>, Option<Range<T>>)
    where
        T: Copy + Ord;

    fn intersect(&self, other: &Range<T>) -> Option<Range<T>>
    where
        T: Copy + Ord;
}

impl<T> RangeExt<T> for Range<T> {
    fn rel(&self, other: &Range<T>) -> RangeRel
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

    fn add<N>(&self, n: N) -> Range<<T as Add<N>>::Output>
    where
        T: Copy + Add<N>,
        N: Copy,
    {
        Range {
            start: self.start + n,
            end: self.end + n,
        }
    }

    fn split_at(&self, at: T) -> (Option<Range<T>>, Option<Range<T>>)
    where
        T: Copy + Ord,
    {
        (
            (self.start < at).then_some(self.start..at.min(self.end)),
            (at < self.end).then_some(at.max(self.start)..self.end),
        )
    }

    fn intersect(&self, other: &Range<T>) -> Option<Range<T>>
    where
        T: Copy + Ord,
    {
        debug_assert!(self.start <= self.end);
        debug_assert!(other.start <= other.end);
        let start = self.start.max(other.start);
        let end = self.end.min(other.end);
        (start < end).then_some(start..end)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!((10..20).add(5), 15..25);
    }

    #[test]
    fn test_split_at() {
        assert_eq!((5..15).split_at(10), (Some(5..10), Some(10..15)));
        assert_eq!((5..15).split_at(20), (Some(5..15), None));
        assert_eq!((5..15).split_at(0), (None, Some(5..15)));
        assert_eq!((5..15).split_at(15), (Some(5..15), None));
        assert_eq!((5..15).split_at(5), (None, Some(5..15)));
    }

    #[test]
    fn test_intersect() {
        assert_eq!((5..15).intersect(&(10..20)), Some(10..15));
        assert_eq!((10..20).intersect(&(5..15)), Some(10..15));
        assert_eq!((10..20).intersect(&(5..25)), Some(10..20));
        assert_eq!((5..25).intersect(&(10..20)), Some(10..20));
        assert_eq!((10..15).intersect(&(20..25)), None);
        assert_eq!((20..25).intersect(&(10..15)), None);
    }
}
