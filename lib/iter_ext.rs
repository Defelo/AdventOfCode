pub trait IterExt<I>
where
    I: Iterator,
{
    fn take_while_inclusive<P>(self, predicate: P) -> TakeWhileInclusive<I, P>
    where
        P: FnMut(&I::Item) -> bool;
}

impl<I> IterExt<I> for I
where
    I: Iterator,
{
    fn take_while_inclusive<P>(self, predicate: P) -> TakeWhileInclusive<I, P>
    where
        P: FnMut(&I::Item) -> bool,
    {
        TakeWhileInclusive::new(self, predicate)
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

    test!(split, vec![1, 2, 3, 4], |x| x < 3, vec![1, 2, 3]);
    test!(always_true, vec![1, 2, 3, 4], |_| true, vec![1, 2, 3, 4]);
    test!(always_false, vec![1, 2, 3, 4], |_| false, vec![1]);
}
