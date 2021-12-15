#![feature(test)]

extern crate test;

use std::collections::HashSet;
use std::fs;
use test::Bencher;

struct Input {
    nums: Vec<i32>,
    boards: Vec<Vec<Vec<i32>>>,
}

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/04.txt").unwrap();
    let nums = puzzle.lines().next().unwrap().split(",").map(|n| n.parse().unwrap()).collect();
    let boards = puzzle.split("\n\n").skip(1).map(|board| {
        board.lines().map(|line| {
            line.split_whitespace().map(|n| n.parse().unwrap()).collect()
        }).collect()
    }).collect();
    return Input { nums, boards };
}

struct State {
    boards: Vec<Vec<Vec<i32>>>,
    marked: HashSet<i32>,
}

impl State {
    fn from_input(input: &Input) -> State {
        State {boards: input.boards.clone(), marked: HashSet::new()}
    }

    fn mark(&mut self, num: i32) {
        self.marked.insert(num);
    }

    fn check_board(&self, board: usize) -> bool {
        let board = &self.boards[board];
        for row in board {
            if row.iter().all(|n| self.marked.contains(n)) { return true; }
        }
        for i in 0..board[0].len() {
            if board.iter().all(|row| self.marked.contains(&row[i])) { return true; }
        }
        false
    }

    fn get_score(&self, board: usize) -> i32 {
        *&self.boards[board].iter().flatten().filter(|n| {
            !self.marked.contains(n)
        }).map(|n|*n).reduce(|a, b| a + b).unwrap()
    }
}

fn part1(input: &Input) -> String {
    let mut state = State::from_input(input);
    for num in &input.nums {
        state.mark(*num);
        for i in 0..state.boards.len() {
            if state.check_board(i) {
                return (num * state.get_score(i)).to_string();
            }
        }
    }
    panic!();
}

fn part2(input: &Input) -> String {
    let mut state = State::from_input(input);
    for num in &input.nums {
        state.mark(*num);
        let mut i = 0;
        while i < state.boards.len() {
            if !state.check_board(i) { i += 1; continue }
            if state.boards.len() == 1 { return (num * state.get_score(i)).to_string()}
            state.boards.remove(i);
        }
    }
    panic!()
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
