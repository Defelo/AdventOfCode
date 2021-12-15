#![feature(test)]

extern crate test;

use std::fs;
use test::Bencher;

type Input = Vec<String>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/10.txt").unwrap();
    puzzle.lines().map(|s| s.to_string()).collect()
}

fn part1(input: &Input) -> String {
    input.iter().map(|line| {
        let mut stack = Vec::new();
        for c in line.chars() {
            if "([{<".contains(c) {
                stack.push(c);
            } else {
                let x = stack.pop().unwrap();
                if !["()", "[]", "{}", "<>"].contains(&format!("{}{}", x, c).as_str()) {
                    return match c {
                        ')' => 3,
                        ']' => 57,
                        '}' => 1197,
                        '>' => 25137,
                        _ => panic!()
                    };
                }
            }
        }
        0
    }).sum::<u32>().to_string()
}

fn part2(input: &Input) -> String {
    let mut scores: Vec<usize> = input.iter().filter_map(|line| {
        let mut stack = Vec::new();
        for c in line.chars() {
            if "([{<".contains(c) {
                stack.push(c);
            } else {
                let x = stack.pop().unwrap();
                if !["()", "[]", "{}", "<>"].contains(&format!("{}{}", x, c).as_str()) {
                    return None;
                }
            }
        }
        let mut out = 0;
        for c in stack.iter().rev() {
            out = out * 5 + " ([{<".find(*c).unwrap();
        }
        Some(out)
    }).collect();
    scores.sort();
    scores[scores.len() / 2].to_string()
}

pub fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[bench]
fn bench_part1(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| part1(&input))
}

#[bench]
fn bench_part2(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| part2(&input))
}
