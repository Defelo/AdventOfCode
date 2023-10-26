#![feature(test)]

use rustc_hash::FxHashSet;

type Input = Vec<Vec<Vec<u8>>>;

fn setup(input: &str) -> Input {
    input
        .split("\n\n")
        .map(|group| group.lines().map(|line| line.bytes().collect()).collect())
        .collect()
}

fn part1(input: &Input) -> usize {
    input
        .iter()
        .map(|group| group.iter().flatten().collect::<FxHashSet<_>>().len())
        .sum()
}

fn part2(input: &Input) -> usize {
    input
        .iter()
        .map(|group| {
            let l = group.len();
            group
                .iter()
                .fold([0; 256], |mut acc, xs| {
                    for &x in xs {
                        acc[x as usize] += 1;
                    }
                    acc
                })
                .into_iter()
                .filter(|&x| x == l)
                .count()
        })
        .sum()
}

aoc::main!(2020, 6, ex: 1);
