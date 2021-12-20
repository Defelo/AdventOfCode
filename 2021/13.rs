#![feature(test)]

extern crate test;

use std::borrow::Borrow;
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

    (0..8).map(|n| {
        let k = (5 * n..5 * n+4).flat_map(|i| {
            let dots = &dots;
            (0..6).map(move |j| {
                dots.contains(&(i, j))
            })
        }).fold(0, |acc, x| { (acc << 1) | (x as i32) });
        match k {
            0b011111100100100100011111 => 'A',
            0b111111101001101001010110 => 'B',
            0b011110100001100001010010 => 'C',
            0b111111101001101001100001 => 'E',
            0b111111101000101000100000 => 'F',
            0b011110100001100101010111 => 'G',
            0b111111001000001000111111 => 'H',
            0b000010000001100001111111 => 'J',
            0b111111001000010110100001 => 'K',
            0b111111000001000001000001 => 'L',
            0b111111100100100100011000 => 'P',
            0b111111100100100110011001 => 'R',
            0b111110000001000001111110 => 'U',
            0b100011100101101001110001 => 'Z',
            _ => '?'
        }
    }).collect::<String>()
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
