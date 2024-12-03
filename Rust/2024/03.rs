#![feature(test)]

use regex::Regex;

type Input = String;

fn setup(input: &str) -> Input {
    input.trim().into()
}

fn part1(input: &Input) -> i64 {
    let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
    re.captures_iter(input)
        .map(|c| c[1].parse::<i64>().unwrap() * c[2].parse::<i64>().unwrap())
        .sum()
}

fn part2(input: &Input) -> i64 {
    let re = Regex::new(r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)").unwrap();
    let mut enabled = true;
    let mut out = 0;
    for c in re.captures_iter(input) {
        if &c[0] == "do()" {
            enabled = true;
        } else if &c[0] == "don't()" {
            enabled = false;
        } else if enabled {
            out += c[1].parse::<i64>().unwrap() * c[2].parse::<i64>().unwrap();
        }
    }
    out
}

aoc::main!(2024, 3, ex: 1, 2);
