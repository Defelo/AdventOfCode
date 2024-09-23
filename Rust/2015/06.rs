#![feature(test)]

use std::sync::LazyLock;

use regex::Regex;

type Input = Vec<Instruction>;

#[derive(Debug)]
struct Instruction {
    cmd: Command,
    start: Point,
    end: Point,
}

#[derive(Debug)]
enum Command {
    On,
    Off,
    Toggle,
}

type Point = (usize, usize);

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| {
            static REGEX: LazyLock<Regex> = LazyLock::new(|| {
                Regex::new(r"^(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)$").unwrap()
            });
            let caps = REGEX.captures(line).unwrap();
            let cmd = match &caps[1] {
                "turn on" => Command::On,
                "turn off" => Command::Off,
                "toggle" => Command::Toggle,
                _ => unreachable!(),
            };

            let start = (caps[2].parse().unwrap(), caps[3].parse().unwrap());
            let end = (caps[4].parse().unwrap(), caps[5].parse().unwrap());

            Instruction { cmd, start, end }
        })
        .collect()
}

fn part1(input: &Input) -> usize {
    let mut grid = vec![false; 1000 * 1000];
    for Instruction { cmd, start, end } in input {
        for x in start.0..=end.0 {
            for y in start.1..=end.1 {
                let i = y * 1000 + x;
                grid[i] = match cmd {
                    Command::On => true,
                    Command::Off => false,
                    Command::Toggle => !grid[i],
                }
            }
        }
    }
    grid.into_iter().filter(|&x| x).count()
}

fn part2(input: &Input) -> u64 {
    let mut grid = vec![0u64; 1000 * 1000];
    for Instruction { cmd, start, end } in input {
        for x in start.0..=end.0 {
            for y in start.1..=end.1 {
                let i = y * 1000 + x;
                grid[i] = match cmd {
                    Command::On => grid[i] + 1,
                    Command::Off => grid[i].saturating_sub(1),
                    Command::Toggle => grid[i] + 2,
                }
            }
        }
    }
    grid.into_iter().sum()
}

aoc::main!(2015, 6);
