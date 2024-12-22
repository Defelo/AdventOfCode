#![feature(test)]

use std::sync::atomic::{AtomicU64, Ordering};

use itertools::Itertools;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

type Input = Vec<u64>;

fn setup(input: &str) -> Input {
    input.trim().lines().map(|l| l.parse().unwrap()).collect()
}

fn evolve(secret: u64) -> impl Iterator<Item = u64> {
    std::iter::successors(Some(secret), |&num| {
        let num = ((num << 6) ^ num) & 0xFFFFFF;
        let num = ((num >> 5) ^ num) & 0xFFFFFF;
        let num = ((num << 11) ^ num) & 0xFFFFFF;
        Some(num)
    })
}

fn diffs_to_idx(diffs: [i64; 4]) -> usize {
    diffs.into_iter().fold(0, |idx, x| {
        debug_assert!((-9..=9).contains(&x));
        idx * 19 + (x + 9) as usize
    })
}

fn part1(input: &Input) -> u64 {
    input
        .par_iter()
        .map(|&secret| evolve(secret).nth(2000).unwrap())
        .sum()
}

fn part2(input: &Input) -> u64 {
    let sequences = (0..19usize.pow(4))
        .map(|_| AtomicU64::new(0))
        .collect::<Vec<_>>();

    input.par_iter().for_each(|&secret| {
        let mut seen = [0u64; 19usize.pow(4).div_ceil(64)];
        evolve(secret)
            .map(|num| num % 10)
            .tuple_windows()
            .map(|(a, b)| (b as i64 - a as i64, b))
            .take(2000)
            .tuple_windows()
            .map(|((a, _), (b, _), (c, _), (d, price))| ((a, b, c, d), price))
            .for_each(|(seq, price)| {
                let idx = diffs_to_idx(seq.into());
                if seen[idx / 64] & (1 << (idx % 64)) != 0 {
                    return;
                }
                seen[idx / 64] |= 1 << (idx % 64);
                sequences[idx].fetch_add(price, Ordering::Relaxed);
            });
    });

    sequences.into_iter().map(|x| x.into_inner()).max().unwrap()
}

aoc::main!(2024, 22, ex: 1, 2);
