#![feature(test)]

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

fn solve(mut nums: Vec<i32>) -> i32 {
    for j in 1.. {
        let mut all_zero = true;
        let mut prev = nums[j - 1];
        for i in nums.iter_mut().skip(j) {
            *i = prev - *i;
            prev -= *i;

            all_zero &= *i == 0;
        }

        if all_zero {
            break;
        }
    }

    nums.into_iter().sum()
}

fn part1(input: &Input) -> i32 {
    input
        .clone()
        .into_iter()
        .map(|mut nums| {
            nums.reverse();
            solve(nums)
        })
        .sum()
}

fn part2(input: &Input) -> i32 {
    input.clone().into_iter().map(solve).sum()
}

aoc::main!(2023, 9, ex: 1);
