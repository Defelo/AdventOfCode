#![feature(test)]

use regex::Regex;

struct Policy {
    a: u32,
    b: u32,
    c: char,
}

type Input = Vec<(Policy, String)>;

fn setup(input: &str) -> Input {
    let mut out: Input = Vec::new();
    let regex = Regex::new(r"^(\d+)-(\d+) (.): (\w+)$").unwrap();
    for line in input.lines() {
        let result = regex.captures(line).unwrap();
        let a = result[1].parse::<u32>().unwrap();
        let b = result[2].parse::<u32>().unwrap();
        let c = result[3].parse::<char>().unwrap();
        let password = result[4].to_string();
        out.push((Policy { a, b, c }, password));
    }
    out
}

fn part1(input: &Input) -> String {
    let mut out: u32 = 0;
    for (policy, password) in input {
        let count = password.chars().filter(|c| c == &policy.c).count() as u32;
        if policy.a <= count && count <= policy.b {
            out += 1;
        }
    }
    out.to_string()
}

fn part2(input: &Input) -> String {
    let mut out: u32 = 0;
    for (policy, password) in input {
        let a = password.chars().nth(policy.a as usize - 1).unwrap();
        let b = password.chars().nth(policy.b as usize - 1).unwrap();
        if (a == policy.c) ^ (b == policy.c) {
            out += 1;
        }
    }
    out.to_string()
}

aoc::main!(2020, 2, ex: 1);
