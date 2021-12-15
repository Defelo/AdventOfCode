#![feature(test)]

extern crate test;

use std::collections::HashMap;
use std::fs;
use test::Bencher;
use regex::Regex;

type Line = (i32, i32, i32, i32);
type Input = Vec<Line>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/05.txt").unwrap();
    let regex = Regex::new(r"^(\d+),(\d+) -> (\d+),(\d+)$").unwrap();
    puzzle.lines().map(|line| {
        let capture = regex.captures(line).unwrap();
        (
            capture[1].parse().unwrap(),
            capture[2].parse().unwrap(),
            capture[3].parse().unwrap(),
            capture[4].parse().unwrap(),
        )
    }).collect()
}

fn iter_line(line: &Line) -> Vec<(i32, i32)> {
    let (mut x, mut y, x2, y2) = line;
    let mut result = vec![(x, y)];
    while (x != *x2) || (y != *y2) {
        if x < *x2 { x += 1; }
        else if x > *x2 { x -= 1; }
        if y < *y2 { y += 1; }
        else if y > *y2 { y -= 1; }
        result.push((x, y));
    }
    result
}

fn part1(input: &Input) -> String {
    let mut counter = HashMap::new();
    for line in input {
        if (line.0 != line.2) && (line.1 != line.3) { continue }
        for (x, y) in iter_line(line) {
            let cnt = *counter.entry((x, y)).or_insert(0) + 1;
            counter.insert((x, y), cnt);
        }
    }
    counter.iter().filter(|&(_, &cnt)| cnt > 1).count().to_string()
}

fn part2(input: &Input) -> String {
    let mut counter = HashMap::new();
    for line in input {
        for (x, y) in iter_line(line) {
            let cnt = *counter.entry((x, y)).or_insert(0) + 1;
            counter.insert((x, y), cnt);
        }
    }
    counter.iter().filter(|&(_, &cnt)| cnt > 1).count().to_string()
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
