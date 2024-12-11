#![feature(test)]

use rustc_hash::FxHashMap;

type Input = Vec<u64>;

fn setup(input: &str) -> Input {
    input
        .split_whitespace()
        .map(|n| n.parse().unwrap())
        .collect()
}

fn count(n: u64, steps: usize, dp: &mut FxHashMap<(u64, usize), usize>) -> usize {
    let k = (n, steps);
    if let Some(&result) = dp.get(&k) {
        return result;
    }

    let result = if steps == 0 {
        1
    } else if n == 0 {
        count(1, steps - 1, dp)
    } else {
        let d = n.ilog10() + 1;
        if d & 1 == 0 {
            let m = 10u64.pow(d / 2);
            count(n / m, steps - 1, dp) + count(n % m, steps - 1, dp)
        } else {
            count(n * 2024, steps - 1, dp)
        }
    };

    dp.insert(k, result);

    result
}

fn solve(input: &Input, steps: usize) -> usize {
    let mut dp = FxHashMap::default();
    input.iter().map(|&n| count(n, steps, &mut dp)).sum()
}

fn part1(input: &Input) -> usize {
    solve(input, 25)
}

fn part2(input: &Input) -> usize {
    solve(input, 75)
}

aoc::main!(2024, 11, ex: 1);
