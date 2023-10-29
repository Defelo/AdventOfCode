#![feature(test)]

use std::collections::VecDeque;

use itertools::Itertools;

#[derive(Debug)]
struct Input {
    numbers: Vec<u64>,
    n: usize,
}

fn setup(input: &str) -> Input {
    let mut lines = input.lines().peekable();
    let n = if lines.peek().unwrap().starts_with("# n=") {
        lines
            .next()
            .unwrap()
            .split('=')
            .nth(1)
            .unwrap()
            .parse()
            .unwrap()
    } else {
        25
    };
    let numbers = lines.map(|line| line.parse().unwrap()).collect();
    Input { numbers, n }
}

fn part1(input: &Input) -> u64 {
    input
        .numbers
        .iter()
        .copied()
        .enumerate()
        .skip(input.n)
        .find(|&(i, x)| {
            !(i - input.n..i).any(|j| {
                (i - input.n..i).any(|k| {
                    input.numbers[j] != input.numbers[k] && input.numbers[j] + input.numbers[k] == x
                })
            })
        })
        .unwrap()
        .1
}

fn part2(input: &Input) -> u64 {
    let target = part1(input);
    let mut nums = VecDeque::new();
    let mut inp = input.numbers.iter().copied();
    let mut sum = 0;
    while sum != target {
        if sum < target {
            let x = inp.next().unwrap();
            nums.push_back(x);
            sum += x;
        } else {
            let x = nums.pop_front().unwrap();
            sum -= x;
        }
    }
    let (min, max) = nums.into_iter().minmax().into_option().unwrap();
    min + max
}

aoc::main!(2020, 9, ex: 1);
