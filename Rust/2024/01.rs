#![feature(test)]

use itertools::Itertools;

type Input = Vec<(i32, i32)>;

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|x| x.parse().unwrap())
                .collect_tuple()
                .unwrap()
        })
        .collect()
}

fn part1(input: &Input) -> u32 {
    let (mut left, mut right): (Vec<_>, Vec<_>) = input.iter().copied().unzip();
    left.sort_unstable();
    right.sort_unstable();

    left.into_iter()
        .zip(right)
        .map(|(l, r)| l.abs_diff(r))
        .sum()
}

fn part2(input: &Input) -> i32 {
    input
        .iter()
        .map(|&(l, _)| l * input.iter().filter(|&&(_, r)| l == r).count() as i32)
        .sum()
}

aoc::main!(2024, 1, ex: 1);
