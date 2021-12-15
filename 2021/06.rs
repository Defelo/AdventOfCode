#![feature(test)]

extern crate test;

use std::fs;
use test::Bencher;

type Input = Vec<i32>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/06.txt").unwrap();
    puzzle.trim().split(",").map(|x| x.parse().unwrap()).collect()
}

fn solve(input: &Input, n: u32) -> String {
    let mut counter = vec![0; 9];
    for num in input {
        counter[*num as usize] += 1;
    }
    for _ in 0..n {
        let x = counter.remove(0);
        counter.push(x);
        counter[6] += x;
    }
    counter.iter().sum::<u64>().to_string()
}

fn part1(input: &Input) -> String {
    solve(input, 80)
}

fn part2(input: &Input) -> String {
    solve(input, 256)
}

pub fn main() {
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
