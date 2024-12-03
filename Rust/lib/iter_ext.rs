use std::{hash::Hash, iter::Iterator};

use rustc_hash::FxHashMap;

pub trait IterExt: Iterator {
    fn take_while_inclusive<P>(self, predicate: P) -> TakeWhileInclusive<Self, P>
    where
        Self: Sized,
        P: FnMut(&Self::Item) -> bool;

    fn chain_if_empty<U>(self, other: U) -> ChainIfEmpty<Self, U::IntoIter>
    where
        Self: Sized,
        U: IntoIterator<Item = Self::Item>;

    fn transpose(self) -> Transpose<<Self::Item as IntoIterator>::IntoIter>
    where
        Self: Sized,
        Self::Item: IntoIterator;

    fn all_consume<P>(self, predicate: P) -> bool
    where
        Self: Sized,
        P: FnMut(Self::Item) -> bool;

    fn any_consume<P>(self, predicate: P) -> bool
    where
        Self: Sized,
        P: FnMut(Self::Item) -> bool;

    fn index(mut self, elem: impl PartialEq<Self::Item>) -> Option<usize>
    where
        Self: Sized,
    {
        self.position(|x| elem == x)
    }

    fn counts_fx(self) -> FxHashMap<Self::Item, usize>
    where
        Self: Sized,
        Self::Item: Hash + Eq,
    {
        self.fold(FxHashMap::default(), |mut counts, x| {
            counts.entry(x).and_modify(|x| *x += 1).or_insert(1);
            counts
        })
    }
}

impl<I> IterExt for I
where
    I: Iterator,
{
    fn take_while_inclusive<P>(self, predicate: P) -> TakeWhileInclusive<I, P>
    where
        P: FnMut(&I::Item) -> bool,
    {
        TakeWhileInclusive::new(self, predicate)
    }

    fn chain_if_empty<U>(self, other: U) -> ChainIfEmpty<I, U::IntoIter>
    where
        U: IntoIterator<Item = Self::Item>,
    {
        ChainIfEmpty {
            iter: self,
            other: other.into_iter(),
            state: State::Unknown,
        }
    }

    fn transpose(self) -> Transpose<<Self::Item as std::iter::IntoIterator>::IntoIter>
    where
        Self::Item: IntoIterator,
    {
        Transpose {
            iterators: self.map(|it| it.into_iter()).collect(),
        }
    }

    fn all_consume<P>(self, mut predicate: P) -> bool
    where
        Self: Sized,
        P: FnMut(Self::Item) -> bool,
    {
        #[allow(clippy::unnecessary_fold)]
        self.fold(true, |acc, x| acc && predicate(x))
    }

    fn any_consume<P>(self, mut predicate: P) -> bool
    where
        Self: Sized,
        P: FnMut(Self::Item) -> bool,
    {
        #[allow(clippy::unnecessary_fold)]
        self.fold(false, |acc, x| acc || predicate(x))
    }
}

pub struct TakeWhileInclusive<I, P> {
    iter: I,
    predicate: P,
    done: bool,
}

impl<I, P> TakeWhileInclusive<I, P> {
    fn new(iter: I, predicate: P) -> Self {
        TakeWhileInclusive {
            iter,
            predicate,
            done: false,
        }
    }
}

impl<I, P> Iterator for TakeWhileInclusive<I, P>
where
    I: Iterator,
    P: FnMut(&I::Item) -> bool,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        self.iter.next().map(|item| {
            if !(self.predicate)(&item) {
                self.done = true;
            }
            item
        })
    }
}

enum State {
    Empty,
    NotEmpty,
    Unknown,
}

pub struct ChainIfEmpty<I, U> {
    iter: I,
    other: U,
    state: State,
}

impl<I, U> Iterator for ChainIfEmpty<I, U>
where
    I: Iterator,
    U: Iterator<Item = I::Item>,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            State::Empty => self.other.next(),
            State::NotEmpty => self.iter.next(),
            State::Unknown => match self.iter.next() {
                Some(x) => {
                    self.state = State::NotEmpty;
                    Some(x)
                }
                None => {
                    self.state = State::Empty;
                    self.other.next()
                }
            },
        }
    }
}

pub struct Transpose<I: Iterator> {
    iterators: Vec<I>,
}

impl<I: Iterator> Iterator for Transpose<I> {
    type Item = Vec<I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iterators
            .iter_mut()
            .map(|it| it.next())
            .collect::<Option<Vec<_>>>()
    }
}

#[cfg(test)]
mod tests_take_while_inclusive {
    use super::*;

    macro_rules! test {
        ($name:ident, $inp:expr, $pred:expr, $exp: expr) => {
            #[test]
            fn $name() {
                let pred = $pred;
                assert_eq!(
                    $inp.iter()
                        .take_while_inclusive(|&&x| (pred)(x))
                        .copied()
                        .collect::<Vec<_>>(),
                    $exp
                )
            }
        };
    }

    test!(split, [1, 2, 3, 4], |x| x < 3, vec![1, 2, 3]);
    test!(always_true, [1, 2, 3, 4], |_| true, vec![1, 2, 3, 4]);
    test!(always_false, [1, 2, 3, 4], |_| false, vec![1]);
}

#[cfg(test)]
mod tests_chain_if_empty {
    use super::*;

    macro_rules! test {
        ($name:ident, $inp1:expr, $inp2:expr, $exp: expr) => {
            #[test]
            fn $name() {
                assert_eq!(
                    $inp1.into_iter().chain_if_empty($inp2).collect::<Vec<_>>(),
                    $exp
                );
            }
        };
    }

    test!(both_empty, Vec::<i32>::new(), [], vec![]);
    test!(first_empty, [], [4, 5, 6], vec![4, 5, 6]);
    test!(second_empty, [1, 2, 3], [], vec![1, 2, 3]);
    test!(both_nonempty, [1, 2, 3], [4, 5, 6], vec![1, 2, 3]);
}
