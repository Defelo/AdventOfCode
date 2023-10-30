#![feature(test)]

use itertools::Itertools;

type Input = Vec<usize>;

fn setup(input: &str) -> Input {
    input.lines().map(|line| line.parse().unwrap()).collect()
}

fn part1(input: &Input) -> usize {
    let mut d1 = 0;
    let mut d3 = 1;
    for (p, q) in std::iter::once(0)
        .chain(input.iter().copied().sorted_unstable())
        .tuple_windows()
    {
        d1 += (p + 1 == q) as usize;
        d3 += (p + 3 == q) as usize;
    }
    d1 * d3
}

fn part2(input: &Input) -> usize {
    let nums = input.iter().copied().sorted_unstable().collect_vec();
    let device = *nums.last().unwrap() + 3;
    let mut adapters = vec![false; device as _];
    adapters[0] = true;
    for i in nums {
        adapters[i] = true;
    }

    let mut dp = vec![1; device + 1];
    for i in 1..=device {
        dp[i] = (1..=3)
            .filter_map(|d| i.checked_sub(d))
            .filter(|&j| adapters[j])
            .map(|j| dp[j])
            .sum();
    }

    dp[device]
}

aoc::main!(2020, 10, ex: 1, 2);
