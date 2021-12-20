#![feature(test)]

extern crate test;

use std::fs;
use test::Bencher;

type Input = String;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("XXXX/XX.txt").unwrap();
    puzzle.to_string()
}

fn part1(input: &Input) -> String {
    panic!();
}

fn part2(input: &Input) -> String {
    panic!();
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
