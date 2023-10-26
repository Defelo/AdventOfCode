#![feature(test)]

use itertools::Itertools;

type Input = Vec<u32>;

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            line.bytes().fold(0, |acc, c| {
                (acc << 1)
                    | match c {
                        b'F' | b'L' => 0,
                        b'B' | b'R' => 1,
                        _ => panic!(),
                    }
            })
        })
        .collect()
}

fn part1(input: &Input) -> u32 {
    input.iter().copied().max().unwrap()
}

fn part2(input: &Input) -> u32 {
    input
        .iter()
        .sorted_unstable()
        .tuple_windows()
        .find(|(&a, &b)| a + 1 != b)
        .unwrap()
        .0
        + 1
}

aoc::main!(2020, 5);
