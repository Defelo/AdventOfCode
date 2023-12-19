pub trait TupleExt<T1, T2> {
    fn swap(self) -> (T2, T1);
}

impl<T1, T2> TupleExt<T1, T2> for (T1, T2) {
    fn swap(self) -> (T2, T1) {
        (self.1, self.0)
    }
}

pub trait HomogeneousTupleExt<T> {
    fn bimap<U>(self, f: impl Fn(T) -> U) -> (U, U);
}

impl<T> HomogeneousTupleExt<T> for (T, T) {
    fn bimap<U>(self, f: impl Fn(T) -> U) -> (U, U) {
        (f(self.0), f(self.1))
    }
}
