#![feature(test)]

use aoc::intcode::{Int, IntcodeVm};

type Input = Vec<Int>;

fn setup(input: &str) -> Input {
    input
        .split(',')
        .map(|x| x.trim().parse().unwrap())
        .collect()
}

fn part1(input: &Input) -> Int {
    IntcodeVm::with_input(input.iter().copied(), [1])
        .last()
        .unwrap()
        .unwrap()
}

fn part2(input: &Input) -> Int {
    IntcodeVm::with_input(input.iter().copied(), [5])
        .next()
        .unwrap()
        .unwrap()
}

aoc::main!(2019, 5);
