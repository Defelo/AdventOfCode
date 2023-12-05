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
}
