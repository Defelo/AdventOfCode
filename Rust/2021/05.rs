#![feature(test)]

use std::cmp::Ordering;

use regex::Regex;
use rustc_hash::FxHashMap;

type Line = (i32, i32, i32, i32);
type Input = Vec<Line>;

fn setup(input: &str) -> Input {
    let regex = Regex::new(r"^(\d+),(\d+) -> (\d+),(\d+)$").unwrap();
    input
        .lines()
        .map(|line| {
            let capture = regex.captures(line).unwrap();
            (
                capture[1].parse().unwrap(),
                capture[2].parse().unwrap(),
                capture[3].parse().unwrap(),
                capture[4].parse().unwrap(),
            )
        })
        .collect()
}

fn iter_line(line: &Line) -> Vec<(i32, i32)> {
    let (mut x, mut y, x2, y2) = line;
    let mut result = vec![(x, y)];
    while (x != *x2) || (y != *y2) {
        match x.cmp(x2) {
            Ordering::Less => x += 1,
            Ordering::Greater => x -= 1,
            _ => {}
        }
        match y.cmp(y2) {
            Ordering::Less => y += 1,
            Ordering::Greater => y -= 1,
            _ => {}
        }
        result.push((x, y));
    }
    result
}

fn part1(input: &Input) -> String {
    let mut counter = FxHashMap::default();
    for line in input {
        if (line.0 != line.2) && (line.1 != line.3) {
            continue;
        }
        for (x, y) in iter_line(line) {
            let cnt = *counter.entry((x, y)).or_insert(0) + 1;
            counter.insert((x, y), cnt);
        }
    }
    counter
        .iter()
        .filter(|&(_, &cnt)| cnt > 1)
        .count()
        .to_string()
}

fn part2(input: &Input) -> String {
    let mut counter = FxHashMap::default();
    for line in input {
        for (x, y) in iter_line(line) {
            let cnt = *counter.entry((x, y)).or_insert(0) + 1;
            counter.insert((x, y), cnt);
        }
    }
    counter
        .iter()
        .filter(|&(_, &cnt)| cnt > 1)
        .count()
        .to_string()
}

aoc::main!(2021, 5, ex: 1);
