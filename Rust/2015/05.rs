#![feature(test)]

use itertools::Itertools;

type Input = Vec<String>;

fn setup(input: &str) -> Input {
    input.trim().lines().map(Into::into).collect()
}

fn part1(input: &Input) -> usize {
    input
        .iter()
        .filter(|s| {
            let r1 = s
                .chars()
                .filter(|c| "aeiou".contains(c.to_ascii_lowercase()))
                .count()
                >= 3;

            let r2 = s.chars().tuple_windows().any(|(a, b)| a == b);

            let r3 = ["ab", "cd", "pq", "xy"].into_iter().all(|x| !s.contains(x));

            r1 && r2 && r3
        })
        .count()
}

fn part2(input: &Input) -> usize {
    input
        .iter()
        .filter(|s| {
            let r1 =
                (0..s.len() - 1).any(|i| (i + 2..s.len() - 1).any(|j| s[i..i + 2] == s[j..j + 2]));

            let r2 = s.chars().tuple_windows().any(|(a, _, b)| a == b);

            r1 && r2
        })
        .count()
}

aoc::main!(2015, 5);
