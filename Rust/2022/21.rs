#![feature(test)]

use std::collections::HashMap;

#[derive(Debug)]
struct Input {
    root_monkey: usize,
    humn_monkey: usize,
    jobs: Vec<Job>,
}

#[derive(Debug, Clone, Copy)]
enum Job {
    Number(i64),
    Add(usize, usize),
    Sub(usize, usize),
    Mul(usize, usize),
    Div(usize, usize),
}

impl Job {
    fn monkeys(self) -> (usize, usize) {
        match self {
            Job::Number(_) => panic!("not an operation monkey"),
            Job::Add(a, b) => (a, b),
            Job::Sub(a, b) => (a, b),
            Job::Mul(a, b) => (a, b),
            Job::Div(a, b) => (a, b),
        }
    }
}

fn setup(input: &str) -> Input {
    let monkeys: HashMap<String, usize> = input
        .lines()
        .enumerate()
        .map(|(i, line)| (line[..4].into(), i))
        .collect();

    let jobs = input
        .lines()
        .map(|line| {
            let job = &line[6..];
            if let Ok(num) = job.parse() {
                Job::Number(num)
            } else {
                let mut job = job.split_whitespace();
                let a = job.next().unwrap();
                let op = job.next().unwrap();
                let b = job.next().unwrap();

                let a = *monkeys.get(a).unwrap();
                let b = *monkeys.get(b).unwrap();

                match op {
                    "+" => Job::Add(a, b),
                    "-" => Job::Sub(a, b),
                    "*" => Job::Mul(a, b),
                    "/" => Job::Div(a, b),
                    _ => panic!(),
                }
            }
        })
        .collect();

    Input {
        root_monkey: *monkeys.get("root").unwrap(),
        humn_monkey: *monkeys.get("humn").unwrap(),
        jobs,
    }
}

fn yell_recursive(dp: &mut [Option<i64>], jobs: &[Job], monkey: usize) -> i64 {
    if let Some(x) = dp[monkey] {
        return x;
    }

    let mut yell = |i| yell_recursive(dp, jobs, i);
    let value = match jobs[monkey] {
        Job::Number(x) => x,
        Job::Add(a, b) => yell(a) + yell(b),
        Job::Sub(a, b) => yell(a) - yell(b),
        Job::Mul(a, b) => yell(a) * yell(b),
        Job::Div(a, b) => yell(a) / yell(b),
    };

    dp[monkey] = Some(value);
    value
}

fn part1(input: &Input) -> i64 {
    let mut dp = input.jobs.iter().map(|_| None).collect::<Vec<_>>();
    yell_recursive(&mut dp, &input.jobs, input.root_monkey)
}

fn yell_recursive_opt(
    dp: &mut [Option<Option<i64>>],
    jobs: &[Job],
    monkey: usize,
    human: usize,
) -> Option<i64> {
    if monkey == human {
        return None;
    }
    if let Some(x) = dp[monkey] {
        return x;
    }

    let mut yell = |i| yell_recursive_opt(dp, jobs, i, human);
    let value = match jobs[monkey] {
        Job::Number(x) => Some(x),
        job => {
            let (a, b) = job.monkeys();
            let (Some(a), Some(b)) = (yell(a), yell(b)) else {
                return None;
            };
            match job {
                Job::Add(_, _) => Some(a + b),
                Job::Sub(_, _) => Some(a - b),
                Job::Mul(_, _) => Some(a * b),
                Job::Div(_, _) => Some(a / b),
                _ => unreachable!(),
            }
        }
    };

    dp[monkey] = Some(value);
    value
}

fn part2(input: &Input) -> i64 {
    let mut dp = input.jobs.iter().map(|_| None).collect::<Vec<_>>();
    let mut yell = |i| yell_recursive_opt(&mut dp, &input.jobs, i, input.humn_monkey);

    let (a, b) = input.jobs[input.root_monkey].monkeys();
    let (x, y) = (yell(a), yell(b));
    let (mut num, mut root) = match (x, y) {
        (None, Some(y)) => (y, a),
        (Some(x), None) => (x, b),
        _ => panic!(),
    };

    while root != input.humn_monkey {
        let (a, b) = input.jobs[root].monkeys();
        let (x, y) = (yell(a), yell(b));
        (num, root) = match (input.jobs[root], x, y) {
            (Job::Add(_, _), None, Some(y)) => (num - y, a),
            (Job::Add(_, _), Some(x), None) => (num - x, b),
            (Job::Sub(_, _), None, Some(y)) => (num + y, a),
            (Job::Sub(_, _), Some(x), None) => (x - num, b),
            (Job::Mul(_, _), None, Some(y)) => (num / y, a),
            (Job::Mul(_, _), Some(x), None) => (num / x, b),
            (Job::Div(_, _), None, Some(y)) => (num * y, a),
            (Job::Div(_, _), Some(x), None) => (x / num, b),
            _ => panic!(),
        };
    }
    num
}

aoc::main!(2022, 21, ex: 1);
