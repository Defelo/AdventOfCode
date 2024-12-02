#![feature(test)]

use itertools::Itertools;

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

#[derive(Debug, Clone, Copy, Default)]
struct DiffCounts {
    positive: usize,
    negative: usize,
    invalid: usize,
}

impl DiffCounts {
    fn from_nums(nums: &[i32]) -> Self {
        nums.iter()
            .tuple_windows()
            .fold(Self::default(), |c, (a, b)| c.add(b - a))
    }

    fn classify(diff: i32) -> Self {
        Self {
            positive: (1..=3).contains(&diff) as _,
            negative: (-3..=-1).contains(&diff) as _,
            invalid: (diff == 0 || diff.abs() > 3) as _,
        }
    }

    fn add(self, diff: i32) -> Self {
        let other = Self::classify(diff);
        Self {
            positive: self.positive + other.positive,
            negative: self.negative + other.negative,
            invalid: self.invalid + other.invalid,
        }
    }

    fn sub(self, diff: i32) -> Self {
        let other = Self::classify(diff);
        Self {
            positive: self.positive - other.positive,
            negative: self.negative - other.negative,
            invalid: self.invalid - other.invalid,
        }
    }

    fn check(self) -> bool {
        self.invalid == 0 && (self.positive == 0 || self.negative == 0)
    }
}

fn part1(input: &Input) -> usize {
    input
        .iter()
        .filter(|nums| DiffCounts::from_nums(nums).check())
        .count()
}

fn check_with_tolerance(nums: &[i32]) -> bool {
    let counts = nums
        .iter()
        .tuple_windows()
        .fold(DiffCounts::default(), |c, (a, b)| c.add(b - a));

    if counts.check()
        || counts.sub(nums[1] - nums[0]).check()
        || counts
            .sub(nums[nums.len() - 1] - nums[nums.len() - 2])
            .check()
    {
        return true;
    }

    nums.iter()
        .tuple_windows()
        .any(|(a, b, c)| counts.sub(b - a).sub(c - b).add(c - a).check())
}

fn part2(input: &Input) -> usize {
    input
        .iter()
        .filter(|nums| check_with_tolerance(nums))
        .count()
}

aoc::main!(2024, 2, ex: 1);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wolflu() {
        assert!(check_with_tolerance(&[18, 21, 23, 22, 23, 26, 28]));
    }
}
