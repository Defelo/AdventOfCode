#![feature(test)]

use std::collections::HashSet;

type Input = Vec<i32>;

fn setup(input: &str) -> Input {
    input.lines().map(|i| i.parse().unwrap()).collect()
}

fn part1(input: &Input) -> i32 {
    let mut seen: HashSet<i32> = HashSet::new();
    for &x in input {
        let y = 2020 - x;
        if seen.contains(&y) {
            return x * y;
        }
        seen.insert(x);
    }
    panic!();
}

fn part2(input: &Input) -> i32 {
    for (i, &x) in input.iter().enumerate() {
        let mut seen: HashSet<i32> = HashSet::new();
        for &y in &input[i + 1..] {
            let z = 2020 - x - y;
            if seen.contains(&z) {
                return x * y * z;
            }
            seen.insert(y);
        }
    }
    panic!();
}

aoc::main!(2020, 1, ex: 1);
