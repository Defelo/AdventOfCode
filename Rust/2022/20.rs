#![feature(test)]

use itertools::Itertools;

type Input = Vec<i64>;

fn setup(input: &str) -> Input {
    input.lines().map(|line| line.parse().unwrap()).collect()
}

#[derive(Debug)]
struct CircularList {
    next: Vec<usize>,
    prev: Vec<usize>,
    next_n: Vec<usize>,
    prev_n: Vec<usize>,
    n: usize,
    removed: usize,
}

impl CircularList {
    fn new(len: usize) -> Self {
        let n = (len.isqrt() / 2).max(1);
        let next = (1..len).chain(0..1).collect_vec();
        let prev = (len - 1..len).chain(0..len - 1).collect_vec();
        let next_n = (n..len).chain(0..n).collect_vec();
        let prev_n = (len - n..len).chain(0..len - n).collect_vec();
        Self {
            next,
            prev,
            next_n,
            prev_n,
            n,
            removed: 0,
        }
    }

    fn len(&self) -> usize {
        self.next.len() - self.removed
    }

    fn step_forward(&self, mut element: usize, steps: usize) -> usize {
        let steps = steps % self.len();
        if steps * 2 > self.len() {
            let steps = self.len() - steps;
            for _ in 0..steps / self.n {
                element = self.prev_n[element];
            }
            for _ in 0..steps % self.n {
                element = self.prev[element];
            }
        } else {
            for _ in 0..steps / self.n {
                element = self.next_n[element];
            }
            for _ in 0..steps % self.n {
                element = self.next[element];
            }
        }
        element
    }

    fn remove(&mut self, element: usize) -> usize {
        let before = self.prev[element];
        let after = self.next[element];

        let mut i = self.prev_n[element];
        let mut j = after;
        while i != element {
            self.next_n[i] = j;
            self.prev_n[j] = i;
            i = self.next[i];
            j = self.next[j];
        }
        self.prev_n[element] = usize::MAX;
        self.next_n[element] = usize::MAX;

        self.next[before] = after;
        self.next[element] = usize::MAX;
        self.prev[element] = usize::MAX;
        self.prev[after] = before;

        self.removed += 1;

        before
    }

    fn insert(&mut self, element: usize, prev: usize) {
        let before = prev;
        let after = self.next[before];

        self.next[before] = element;
        self.prev[element] = before;
        self.next[element] = after;
        self.prev[after] = element;

        let mut i = self.prev_n[after];
        let mut j = element;
        while i != after {
            self.next_n[i] = j;
            self.prev_n[j] = i;
            i = self.next[i];
            j = self.next[j];
        }

        self.removed -= 1;
    }

    fn move_forward(&mut self, element: usize, steps: usize) {
        let steps = steps % (self.len() - 1);
        if steps != 0 {
            let prev = self.remove(element);
            let new_prev = self.step_forward(prev, steps);
            self.insert(element, new_prev);
        }
    }
}

fn solve<const N: usize, const K: i64>(input: &Input) -> i64 {
    let zero = input.iter().position(|&x| x == 0).unwrap();
    let mut list = CircularList::new(input.len());
    for _ in 0..N {
        for (i, &x) in input.iter().enumerate() {
            let x = x * K;
            list.move_forward(i, x.rem_euclid(list.len() as i64 - 1) as _);
        }
    }
    [1000, 2000, 3000]
        .into_iter()
        .map(|i| K * input[list.step_forward(zero, i)])
        .sum()
}

fn part1(input: &Input) -> i64 {
    solve::<1, 1>(input)
}

fn part2(input: &Input) -> i64 {
    solve::<10, 811589153>(input)
}

#[cfg(test)]
mod tests {
    use rustc_hash::FxHashSet;

    use super::*;

    #[test]
    fn test_move_forward() {
        let mut list = CircularList::new(7);
        check_list_consistency(&list);
        assert_eq!(to_vec(&list), [0, 1, 2, 3, 4, 5, 6]);
        list.move_forward(1, 3);
        check_list_consistency(&list);
        assert_eq!(to_vec(&list), [0, 2, 3, 4, 1, 5, 6]);
    }

    #[test]
    fn test_remove() {
        let mut list = CircularList::new(7);
        check_list_consistency(&list);
        assert_eq!(to_vec(&list), [0, 1, 2, 3, 4, 5, 6]);

        let prev = list.remove(3);

        assert_eq!(prev, 2);
        check_list_consistency(&list);
        assert_eq!(to_vec(&list), [0, 1, 2, 4, 5, 6]);
    }

    #[test]
    fn test_insert() {
        let mut list = CircularList::new(7);
        let prev = list.remove(3);
        check_list_consistency(&list);
        assert_eq!(to_vec(&list), [0, 1, 2, 4, 5, 6]);

        list.insert(3, prev);

        check_list_consistency(&list);
        assert_eq!(to_vec(&list), [0, 1, 2, 3, 4, 5, 6]);
    }

    fn check_list_consistency(list: &CircularList) {
        dbg!(&list);
        for i in 0..list.len() {
            if list.next[i]
                .max(list.prev[i])
                .max(list.next_n[i])
                .max(list.prev_n[i])
                == usize::MAX
            {
                assert_eq!(list.prev_n[i], usize::MAX);
                assert_eq!(list.prev[i], usize::MAX);
                assert_eq!(list.next[i], usize::MAX);
                assert_eq!(list.next_n[i], usize::MAX);
                continue;
            }

            assert_eq!(list.next[list.prev[i]], i);
            assert_eq!(list.prev[list.next[i]], i);
            assert_eq!(list.next_n[list.prev_n[i]], i);
            assert_eq!(list.prev_n[list.next_n[i]], i);

            let mut j = i;
            let mut k = i;
            for _ in 0..list.n {
                j = list.next[j];
                k = list.prev[k];
            }
            assert_eq!(list.next_n[i], j);
            assert_eq!(list.prev_n[i], k);
        }

        let mut seen = FxHashSet::default();
        let mut i = 0;
        for _ in 0..list.len() {
            seen.insert(i);
            i = list.next[i];
        }
        assert_eq!(i, 0);
        assert_eq!(seen.len(), list.len());
    }

    fn to_vec(list: &CircularList) -> Vec<usize> {
        let mut out = Vec::new();
        let mut i = 0;
        for _ in 0..list.len() {
            out.push(i);
            i = list.next[i];
        }
        out
    }
}

aoc::main!(2022, 20, ex: 1);
