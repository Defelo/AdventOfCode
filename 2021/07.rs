#![feature(test)]

extern crate test;

use std::fs;
use test::Bencher;

type Input = Vec<i32>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/07.txt").unwrap();
    puzzle.trim().split(",").map(|x| x.parse().unwrap()).collect()
}

fn part1(input: &Input) -> String {
    (0..=input.len()).map(|pos| {
        input.iter().map(|n| (pos as i32 - *n).abs()).sum::<i32>()
    }).min().unwrap().to_string()
}

fn part2(input: &Input) -> String {
    (0..=input.len()).map(|pos| {
        input.iter().map(|n| {
            let x = (pos as i32 - *n).abs();
            x * (x + 1) >> 1
        }).sum::<i32>()
    }).min().unwrap().to_string()
}

fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
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
