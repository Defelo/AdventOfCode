#![feature(test)]

use std::cmp::Ordering;

use itertools::Itertools;

type Input = Vec<Vec<i32>>;

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|x| x.parse().unwrap())
                .collect()
        })
        .collect()
}

fn check(nums: impl Iterator<Item = i32>) -> bool {
    let mut increase = false;
    let mut decrease = false;
    for (a, b) in nums.tuple_windows() {
        if !(1..=3).contains(&a.abs_diff(b)) {
            return false;
        }
        match a.cmp(&b) {
            Ordering::Less => increase = true,
            Ordering::Greater => decrease = true,
            Ordering::Equal => return false,
        }
    }

    increase != decrease
}

fn part1(input: &Input) -> usize {
    input
        .iter()
        .filter(|nums| check(nums.iter().copied()))
        .count()
}

fn part2(input: &Input) -> usize {
    input
        .iter()
        .filter(|nums| {
            nums.iter()
                .enumerate()
                .any(|(i, _)| check(nums[..i].iter().chain(&nums[i + 1..]).copied()))
        })
        .count()
}

aoc::main!(2024, 2, ex: 1);
