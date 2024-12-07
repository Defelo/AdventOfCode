#![feature(test)]

type Input = Vec<Equation>;

#[derive(Debug)]
struct Equation {
    result: i64,
    numbers: Vec<i64>,
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| {
            let mut nums = line.split_whitespace();
            let result = nums.next().unwrap().trim_end_matches(':').parse().unwrap();
            let numbers = nums.map(|n| n.parse().unwrap()).collect();
            Equation { result, numbers }
        })
        .collect()
}

fn solve<const P2: bool>(nums: &[i64], goal: i64) -> bool {
    if nums.len() == 1 {
        return nums[0] == goal;
    }

    let (&last, init) = nums.split_last().unwrap();

    let p = 10i64.pow(last.ilog10() + 1);

    (P2 && goal % p == last && solve::<P2>(init, goal / p))
        || (goal % last == 0 && solve::<P2>(init, goal / last))
        || solve::<P2>(init, goal - last)
}

fn part1(input: &Input) -> i64 {
    input
        .iter()
        .filter(|eq| solve::<false>(&eq.numbers, eq.result))
        .map(|eq| eq.result)
        .sum()
}

fn part2(input: &Input) -> i64 {
    input
        .iter()
        .filter(|eq| solve::<true>(&eq.numbers, eq.result))
        .map(|eq| eq.result)
        .sum()
}

aoc::main!(2024, 7, ex: 1);
