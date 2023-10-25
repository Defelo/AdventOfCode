use std::iter::Iterator;

pub trait IterExt: Iterator {
    fn take_while_inclusive<P>(self, predicate: P) -> TakeWhileInclusive<Self, P>
    where
        Self: Sized,
        P: FnMut(&Self::Item) -> bool;

    fn chain_if_empty<U>(self, other: U) -> ChainIfEmpty<Self, U::IntoIter>
    where
        Self: Sized,
        U: IntoIterator<Item = Self::Item>;

    fn transpose(self) -> Transpose<Self>
    where
        Self: Sized,
        Self::Item: Iterator;
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

    fn transpose(self) -> Transpose<Self>
    where
        Self::Item: Iterator,
    {
        Transpose {
            iter: self,
            iterators: Vec::new(),
        }
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

pub struct Transpose<I>
where
    I: Iterator,
    I::Item: Iterator,
{
    iter: I,
    iterators: Vec<I::Item>,
}

impl<I> Iterator for Transpose<I>
where
    I: Iterator,
    I::Item: Iterator,
{
    type Item = Vec<<<I as Iterator>::Item as IntoIterator>::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.iterators.is_empty() {
            self.iterators = self.iter.by_ref().collect();
        }
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
