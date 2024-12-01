#![feature(test)]

use aoc::intcode::{get_output, Int};

type Input = Vec<Int>;

fn setup(input: &str) -> Input {
    input
        .split(',')
        .map(|x| x.trim().parse().unwrap())
        .collect()
}

fn part1(input: &Input) -> Int {
    let output = get_output(input.iter().copied(), [1]).unwrap();
    *output.last().unwrap()
}

fn part2(input: &Input) -> Int {
    let output = get_output(input.iter().copied(), [5]).unwrap();
    output[0]
}

aoc::main!(2019, 5);
