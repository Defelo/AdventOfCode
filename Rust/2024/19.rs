#![feature(test)]

use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

#[derive(Debug)]
struct Input {
    patterns: Vec<String>,
    designs: Vec<String>,
}

fn setup(input: &str) -> Input {
    let mut lines = input.trim().lines();
    let patterns = lines.next().unwrap().split(", ").map(Into::into).collect();
    let designs = lines.skip(1).map(Into::into).collect();
    Input { patterns, designs }
}

fn count(design: &str, patterns: &[String]) -> usize {
    let mut dp = vec![0; design.len() + 1];
    dp[design.len()] = 1;
    for i in (0..design.len()).rev() {
        for p in patterns {
            if i + p.len() > design.len() || !design[i..].starts_with(p) {
                continue;
            }
            dp[i] += dp[i + p.len()];
        }
    }
    dp[0]
}

fn part1(input: &Input) -> usize {
    input
        .designs
        .par_iter()
        .map(|d| count(d, &input.patterns).min(1))
        .sum()
}

fn part2(input: &Input) -> usize {
    input
        .designs
        .par_iter()
        .map(|d| count(d, &input.patterns))
        .sum()
}

aoc::main!(2024, 19, ex: 1);
