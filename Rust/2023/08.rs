#![feature(test)]

use itertools::Itertools;
use num::Integer;
use rayon::prelude::*;
use regex::Regex;
use rustc_hash::FxHashMap;

#[derive(Debug)]
struct Input {
    instructions: Vec<Instruction>,
    nodes: Vec<Node>,
    start: usize,
    goal: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy)]
struct Node {
    left: usize,
    right: usize,
    start: bool,
    goal: bool,
}

fn setup(input: &str) -> Input {
    let instructions = input
        .lines()
        .next()
        .unwrap()
        .bytes()
        .map(|b| match b {
            b'L' => Instruction::Left,
            b'R' => Instruction::Right,
            _ => panic!(),
        })
        .collect();

    let names = input
        .lines()
        .skip(2)
        .enumerate()
        .map(|(i, line)| (line.split_whitespace().next().unwrap(), i))
        .collect::<FxHashMap<_, _>>();

    let regex = Regex::new(r"([^, ]+) = \(([^, ]+), ([^, ]+)\)").unwrap();
    let nodes = input
        .lines()
        .skip(2)
        .map(|line| {
            let captures = regex.captures(line).unwrap();
            let left = names[&captures[2]];
            let right = names[&captures[3]];
            Node {
                left,
                right,
                start: captures[1].ends_with('A'),
                goal: captures[1].ends_with('Z'),
            }
        })
        .collect();

    Input {
        instructions,
        nodes,
        start: names.get("AAA").copied().unwrap_or_default(),
        goal: names.get("ZZZ").copied().unwrap_or_default(),
    }
}

fn solve(input: &Input, start: usize, is_goal: impl Fn(usize) -> bool) -> usize {
    Itertools::take_while_inclusive(
        input
            .instructions
            .iter()
            .cycle()
            .scan(start, |p, instruction| {
                let node = input.nodes[*p];
                *p = match instruction {
                    Instruction::Left => node.left,
                    Instruction::Right => node.right,
                };
                Some(*p)
            }),
        |&p| !is_goal(p),
    )
    .count()
}

fn part1(input: &Input) -> usize {
    solve(input, input.start, |p| p == input.goal)
}

fn part2(input: &Input) -> usize {
    input
        .nodes
        .par_iter()
        .enumerate()
        .filter(|(_, node)| node.start)
        .map(|(i, _)| solve(input, i, |p| input.nodes[p].goal))
        .reduce(|| 1, |a, b| a.lcm(&b))
}

aoc::main!(2023, 8, ex: 1[a], 2[a], 3[b]);
