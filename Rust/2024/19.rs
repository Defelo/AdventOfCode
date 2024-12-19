#![feature(test)]

use aoc::trie::Trie;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

#[derive(Debug)]
struct Input {
    patterns: Trie<u8>,
    designs: Vec<Vec<u8>>,
}

fn setup(input: &str) -> Input {
    let mut lines = input.trim().lines();
    let patterns = lines
        .next()
        .unwrap()
        .split(", ")
        .map(|p| p.bytes())
        .collect();
    let designs = lines.skip(1).map(Into::into).collect();
    Input { patterns, designs }
}

fn count(design: &[u8], patterns: &Trie<u8>) -> usize {
    let mut dp = vec![0; design.len() + 1];
    dp[design.len()] = 1;
    for i in (0..design.len()).rev() {
        dp[i] = patterns
            .prefix_matches(design[i..].iter().copied())
            .map(|len| dp[i + len])
            .sum();
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
