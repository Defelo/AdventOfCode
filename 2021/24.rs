#![feature(test)]

extern crate test;

use std::fs;
use test::Bencher;

type Input = Vec<(i8, i8)>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/24.txt").unwrap();
    let lines: Vec<String> = puzzle.lines().map(|line| line.to_string()).collect();
    (0..14).map(|i| {
        let get = |j: usize| lines[i * 18 + j].split(" ").nth(2).unwrap().parse().unwrap();
        (get(5), get(15))
    }).collect()
}

fn part1(input: &Input) -> String {
    let mut out = [9u8; 14];
    let mut stack = vec![];
    for (i, (mut x, y)) in input.iter().cloned().enumerate() {
        if x < 0 {
            let (j, y) = stack.pop().unwrap();
            x += y;
            if x < 0 {
                out[i] -= -x as u8;
            } else {
                out[j] -= x as u8;
            }
        } else {
            stack.push((i, y));
        }
    }
    out.iter().map(|&x| (x + 0x30) as char).collect()
}

fn part2(input: &Input) -> String {
    let mut out = [1u8; 14];
    let mut stack = vec![];
    for (i, (mut x, y)) in input.iter().cloned().enumerate() {
        if x < 0 {
            let (j, y) = stack.pop().unwrap();
            x += y;
            if x < 0 {
                out[j] += -x as u8;
            } else {
                out[i] += x as u8;
            }
        } else {
            stack.push((i, y));
        }
    }
    out.iter().map(|&x| (x + 0x30) as char).collect()
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
    b.iter(|| part1(&input))
}

#[bench]
fn bench_part2(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| part2(&input))
}
