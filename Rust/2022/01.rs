#![feature(test)]

use itertools::Itertools;

type Input = Vec<Vec<u32>>;

fn setup(input: &str) -> Input {
    input
        .split("\n\n")
        .map(|block| {
            block
                .split_whitespace()
                .map(|num| num.parse().unwrap())
                .collect()
        })
        .collect()
}

fn part1(input: &Input) -> u32 {
    input.iter().map(|block| block.iter().sum()).max().unwrap()
}

fn part2(input: &Input) -> u32 {
    -input
        .iter()
        .map(|block| -(block.iter().sum::<u32>() as i32))
        .k_smallest(3)
        .sum::<i32>() as u32
}

aoc::main!(2022, 1, ex: 1);
