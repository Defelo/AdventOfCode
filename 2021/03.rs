#![feature(test)]
#![feature(drain_filter)]

extern crate test;

use std::fs;
use test::Bencher;

type Input = Vec<String>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/03.txt").unwrap();
    puzzle.lines().map(|s| s.to_string()).collect()
}

fn count(input: &Input, i: usize, chr: &char) -> usize {
    input.iter().filter(|s| s.chars().nth(i).unwrap() == *chr).count()
}

fn most_common(input: &Input, i: usize) -> char { "01".chars().max_by_key(|c| count(input, i, c)).unwrap() }

fn least_common(input: &Input, i: usize) -> char { "01".chars().min_by_key(|c| count(input, i, c)).unwrap() }

fn part1(input: &Input) -> String {
    let mut most = 0;
    let mut least = 0;
    for i in 0..input[0].len() {
        most = most << 1 | most_common(input, i) as u32 - '0' as u32;
        least = least << 1 | least_common(input, i) as u32 - '0' as u32;
    }
    (most * least).to_string()
}

fn find(input: &Input, x: bool) -> isize {
    let mut out = input.clone();
    for i in 0..input[0].len() {
        let mx = if x {least_common(&out, i) } else {most_common(&out, i)};
        out.drain_filter(|x|x.chars().nth(i).unwrap() != mx).count();
        if out.len() == 1 {
            return isize::from_str_radix(out[0].as_str(), 2).unwrap();
        }
    }
    panic!();
}

fn part2(input: &Input) -> String {
    (find(input, true) * find(input, false)).to_string()
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
