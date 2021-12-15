#![feature(test)]

extern crate test;

use std::collections::HashSet;
use std::fs;
use test::Bencher;

use regex::Regex;

enum Instruction { X(u32), Y(u32) }

struct Input {
    dots: HashSet<(u32, u32)>,
    instructions: Vec<Instruction>,
}

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/13.txt").unwrap();
    let mut blocks = puzzle.split("\n\n");
    let dots = blocks.next().unwrap().lines().map(|line| {
        let mut coords = line.split(",");
        (coords.next().unwrap().parse().unwrap(), coords.next().unwrap().parse().unwrap())
    }).collect();
    let regex = Regex::new(r"^fold along (.)=(\d+)$").unwrap();
    let instructions = blocks.next().unwrap().lines().map(|line| {
        let capture = regex.captures(line).unwrap();
        let n: u32 = capture.get(2).unwrap().as_str().parse().unwrap();
        match capture.get(1).unwrap().as_str() {
            "x" => Instruction::X(n),
            "y" => Instruction::Y(n),
            _ => panic!()
        }
    }).collect();
    Input { dots, instructions }
}

fn fold(dots: &HashSet<(u32, u32)>, instruction: &Instruction) -> HashSet<(u32, u32)> {
    dots.iter().cloned().map(|(x, y)| {
        match instruction {
            Instruction::X(n) => (x.min(2 * n - x), y),
            Instruction::Y(n) => (x, y.min(2 * n - y)),
        }
    }).collect()
}

fn part1(input: &Input) -> String {
    let mut dots = input.dots.clone();
    let instruction = input.instructions.first().unwrap();
    dots = fold(&dots, instruction);
    dots.len().to_string()
}

fn part2(input: &Input) -> String {
    let mut dots = input.dots.clone();
    for instruction in &input.instructions {
        dots = fold(&dots, instruction);
    }
    let (minx, maxx, miny, maxy) = dots.iter().cloned().fold((u32::MAX, u32::MIN, u32::MAX, u32::MIN), |(minx, maxx, miny, maxy), (x, y)| {
        (minx.min(x), maxx.max(x), miny.min(y), maxy.max(y))
    });
    (miny..=maxy).map(|y| -> String {
        "\n        ".to_string() + (minx..=maxx).map(|x| -> String {
            (if dots.contains(&(x, y)) { "##" } else { "  " }).to_string()
        }).collect::<String>().as_str()
    }).collect::<String>()
}

pub fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
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
