#![feature(test)]

use aoc::iter_ext::IterExt;
use itertools::Itertools;

type Input = Vec<Vec<bool>>;

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| line.bytes().map(|b| b == b'#').collect())
        .collect()
}

fn solve<const N: usize>(input: &Input) -> usize {
    let empty_rows = input.iter().map(|row| row.iter().all(|x| !x)).collect_vec();
    let empty_cols = input
        .iter()
        .transpose()
        .map(|row| row.iter().all(|&x| !x))
        .collect_vec();

    input
        .iter()
        .zip(empty_rows)
        .scan(0, |i, (row, empty)| {
            *i += if empty { N } else { 1 };
            let i = *i;
            Some(row.iter().zip(&empty_cols).scan(0, move |j, (&x, &empty)| {
                *j += if empty { N } else { 1 };
                Some((i, *j, x))
            }))
        })
        .flatten()
        .filter_map(|(i, j, x)| x.then_some((i, j)))
        .collect_vec()
        .into_iter()
        .tuple_combinations()
        .map(|(a, b)| a.0.abs_diff(b.0) + a.1.abs_diff(b.1))
        .sum()
}

fn part1(input: &Input) -> usize {
    solve::<2>(input)
}

fn part2(input: &Input) -> usize {
    solve::<1000000>(input)
}

aoc::main!(2023, 11, ex: 1);
