#![feature(test)]

use aoc::grid::Direction;
use regex::Regex;

#[derive(Debug)]
struct Input {
    part1: Vec<Instruction>,
    part2: Vec<Instruction>,
}

#[derive(Debug)]
struct Instruction {
    direction: Direction,
    steps: isize,
}

fn setup(input: &str) -> Input {
    let regex = Regex::new(r"^([UDLR]) (\d+) \(#(.{5})([0123])\)$").unwrap();
    let (part1, part2) = input
        .lines()
        .map(|line| {
            let cap = regex.captures(line).unwrap();

            let p1 = Instruction {
                direction: Direction::from(cap[1].chars().next().unwrap()),
                steps: cap[2].parse().unwrap(),
            };

            let p2 = Instruction {
                direction: match &cap[4] {
                    "0" => Direction::East,
                    "1" => Direction::South,
                    "2" => Direction::West,
                    "3" => Direction::North,
                    _ => unreachable!(),
                },
                steps: isize::from_str_radix(&cap[3], 16).unwrap(),
            };

            (p1, p2)
        })
        .unzip();

    Input { part1, part2 }
}

fn solve(instructions: &[Instruction]) -> usize {
    let (_, a, b) = instructions
        .iter()
        .fold((0, 0, 0), |(x, a, b), i| match i.direction {
            Direction::North => (x, a - i.steps * x, b + i.steps),
            Direction::East => (x + i.steps, a, b + i.steps),
            Direction::South => (x, a + i.steps * x, b + i.steps),
            Direction::West => (x - i.steps, a, b + i.steps),
        });

    a.unsigned_abs() + b.unsigned_abs() / 2 + 1
}

fn part1(input: &Input) -> usize {
    solve(&input.part1)
}

fn part2(input: &Input) -> usize {
    solve(&input.part2)
}

aoc::main!(2023, 18, ex: 1);
