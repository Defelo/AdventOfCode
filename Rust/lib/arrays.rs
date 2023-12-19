pub trait ArrayExt<T> {
    fn update(self, index: usize, value: T) -> Self;
}

impl<T, const N: usize> ArrayExt<T> for [T; N] {
    fn update(mut self, index: usize, value: T) -> Self {
        self[index] = value;
        self
    }
}
