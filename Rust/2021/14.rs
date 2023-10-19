#![feature(test)]

use counter::Counter;
use regex::Regex;
use rustc_hash::FxHashMap;

struct Input {
    template: String,
    rules: FxHashMap<(char, char), char>,
}

fn setup(input: &str) -> Input {
    let mut lines = input.lines();
    let template = lines.next().unwrap().to_string();
    lines.next();
    let regex = Regex::new(r"^([A-Z])([A-Z]) -> ([A-Z])$").unwrap();
    let rules = lines
        .map(|line| {
            let capture = regex.captures(line).unwrap();
            let get = |i| capture.get(i).unwrap().as_str().chars().next().unwrap();
            ((get(1), get(2)), get(3))
        })
        .collect();

    Input { template, rules }
}

fn solve(input: &Input, n: usize) -> usize {
    let mut adj: Counter<(char, char)> = input
        .template
        .chars()
        .zip(input.template[1..].chars())
        .collect();
    for _ in 0..n {
        let mut new: Counter<(char, char)> = Counter::new();
        for ((a, b), c) in adj.iter().map(|((a, b), c)| ((*a, *b), *c)) {
            match input.rules.get(&(a, b)).cloned() {
                None => {
                    new[&(a, b)] += c;
                }
                Some(x) => {
                    new[&(a, x)] += c;
                    new[&(x, b)] += c;
                }
            }
        }
        adj = new;
    }
    let mut chars = Counter::new();
    chars[&input.template.chars().next().unwrap()] = 1;
    for ((_, b), c) in &adj {
        chars[b] += *c;
    }
    chars.values().max().unwrap() - chars.values().min().unwrap()
}

fn part1(input: &Input) -> String {
    solve(input, 10).to_string()
}

fn part2(input: &Input) -> String {
    solve(input, 40).to_string()
}

aoc::main!(2021, 14, ex: 1);
