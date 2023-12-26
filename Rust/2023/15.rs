#![feature(test)]

use indexmap::IndexMap;
use regex::Regex;

type Input = Vec<Step>;

#[derive(Debug)]
struct Step {
    raw: String,
    label: String,
    op: Operation,
}

#[derive(Debug, Clone, Copy)]
enum Operation {
    Remove,
    Insert(u8),
}

fn setup(input: &str) -> Input {
    let re = Regex::new(r"^([^=-]+)(-|=(\d+))$").unwrap();
    input
        .split(',')
        .map(|step| {
            let step = step.trim();
            let caps = re.captures(step).unwrap();
            Step {
                raw: step.into(),
                label: caps[1].into(),
                op: match caps.get(3) {
                    Some(focal_length) => Operation::Insert(focal_length.as_str().parse().unwrap()),
                    None => Operation::Remove,
                },
            }
        })
        .collect()
}

fn hash(input: &str) -> usize {
    input
        .bytes()
        .fold(0, |acc, b| (acc + b as usize) * 17 % 256)
}

fn part1(input: &Input) -> usize {
    input.iter().map(|s| hash(&s.raw)).sum()
}

fn part2(input: &Input) -> usize {
    let mut boxes = vec![IndexMap::new(); 256];
    for step in input {
        let bx = &mut boxes[hash(&step.label)];
        match step.op {
            Operation::Remove => bx.shift_remove(&step.label),
            Operation::Insert(focal_length) => bx.insert(&step.label, focal_length),
        };
    }

    boxes
        .into_iter()
        .enumerate()
        .flat_map(|(i, bx)| {
            bx.into_values()
                .enumerate()
                .map(move |(j, focal_length)| (i + 1) * (j + 1) * focal_length as usize)
        })
        .sum()
}

aoc::main!(2023, 15, ex: 1);
