#![feature(test)]

use rustc_hash::FxHashMap;

type Input = Vec<u64>;

fn setup(input: &str) -> Input {
    input
        .split_whitespace()
        .map(|n| n.parse().unwrap())
        .collect()
}

fn solve(input: &Input, steps: usize) -> usize {
    let mut nums = FxHashMap::default();
    let mut new_nums = FxHashMap::default();
    for &n in input {
        *nums.entry(n).or_default() += 1;
    }

    for _ in 0..steps {
        new_nums.clear();
        for (&n, &cnt) in &nums {
            let mut insert = |n| *new_nums.entry(n).or_default() += cnt;
            if n == 0 {
                insert(1);
                continue;
            }
            let d = n.ilog10() + 1;
            if d & 1 == 0 {
                let m = 10u64.pow(d >> 1);
                insert(n / m);
                insert(n % m);
            } else {
                insert(n * 2024);
            }
        }
        std::mem::swap(&mut nums, &mut new_nums);
    }

    nums.values().sum()
}

fn part1(input: &Input) -> usize {
    solve(input, 25)
}

fn part2(input: &Input) -> usize {
    solve(input, 75)
}

aoc::main!(2024, 11, ex: 1);
