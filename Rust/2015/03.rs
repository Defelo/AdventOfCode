#![feature(test)]

use aoc::grid::Direction;
use rustc_hash::FxHashSet;

type Input = Vec<Direction>;

fn setup(input: &str) -> Input {
    input.trim().chars().map(|b| b.into()).collect()
}

fn part1(input: &Input) -> usize {
    input
        .iter()
        .scan((0, 0), |acc, &x| {
            *acc = x.step_signed(*acc);
            Some(*acc)
        })
        .chain([(0, 0)])
        .collect::<FxHashSet<_>>()
        .len()
}

fn part2(input: &Input) -> usize {
    input
        .iter()
        .scan(((0, 0), (0, 0)), |(a, b), &x| {
            std::mem::swap(a, b);
            *a = x.step_signed(*a);
            Some(*a)
        })
        .chain([(0, 0)])
        .collect::<FxHashSet<_>>()
        .len()
}

aoc::main!(2015, 3, ex: 1);
