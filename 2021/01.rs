#![feature(test)]

extern crate test;

use std::fs;
use test::Bencher;

type Input = Vec<i32>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/01.txt").unwrap();
    return puzzle.lines().map(|x| x.parse().unwrap()).collect();
}

fn count(input: &Input, window: usize) -> i32 {
    let mut out = 0;
    for (a, b) in input.iter().zip(&input[window..]) {
        if b > a { out += 1; }
    }
    out
}

fn part1(input: &Input) -> String {
    count(input, 1).to_string()
}

fn part2(input: &Input) -> String {
    count(input, 3).to_string()
}

fn main() {
    let (part1, part2) = run();
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}

pub fn run() -> (String, String) {
    let input = get_input();
    (part1(&input), part2(&input))
}

#[bench]
fn bench_part1(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| { part1(&input) })
}

#[bench]
fn bench_part2(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| { part2(&input) })
}
