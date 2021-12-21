#![feature(test)]

extern crate test;

use std::fs;
use test::Bencher;

use rustc_hash::FxHashMap;

type Input = (u32, u32);

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/21.txt").unwrap();
    let mut lines = puzzle.lines();
    let mut next = || lines.next().unwrap().split(" ").last().unwrap().parse().unwrap();
    (next(), next())
}

fn part1(input: &Input) -> String {
    let mut players = [input.0, input.1];
    let mut scores = [0u32; 2];
    let mut die = 0u32;
    let mut k = 0usize;
    while scores[0].max(scores[1]) < 1000 {
        let x = (0..3).map(|i| (die + i) % 100 + 1).sum::<u32>();
        die += 3;
        players[k] = (players[k] - 1 + x) % 10 + 1;
        scores[k] += players[k];
        k = (k + 1) % 2;
    }
    (die * scores[k]).to_string()
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct State {
    p1: u32,
    p2: u32,
    s1: u32,
    s2: u32,
}

fn dirac(state: State, mem: &mut FxHashMap<State, (u64, u64)>) -> (u64, u64) {
    if state.s2 >= 21 { return (0, 1); }

    match mem.get(&state) {
        Some(&result) => result,
        None => {
            let result = (0u32..27).fold((0, 0), |acc, i| {
                let x = i / 9 % 3 + i / 3 % 3 + i % 3 + 3;
                let q = (state.p1 - 1 + x) % 10 + 1;
                let res = dirac(State { p1: state.p2, p2: q, s1: state.s2, s2: state.s1 + q }, mem);
                (acc.0 + res.1, acc.1 + res.0)
            });

            mem.insert(state, result);
            result
        }
    }
}

fn part2(input: &Input) -> String {
    let (a, b) = dirac(State {
        p1: input.0,
        p2: input.1,
        s1: 0,
        s2: 0,
    }, &mut FxHashMap::default());
    a.max(b).to_string()
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
