#![feature(test)]

extern crate test;

use std::fs;
use test::Bencher;

#[derive(Copy, Clone, Eq, PartialEq)]
enum Type { EMPTY, RIGHT, DOWN }

type Input = Vec<Vec<Type>>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/25.txt").unwrap();
    puzzle.lines().map(|line| {
        line.chars().map(|c| {
            match c {
                'v' => Type::DOWN,
                '>' => Type::RIGHT,
                _ => Type::EMPTY
            }
        }).collect()
    }).collect()
}

fn solve(input: &Input) -> String {
    let mut grid = input.clone();
    let mut cnt = 0;
    loop {
        let mut moved = false;
        let mut new_grid = grid.clone();
        for (i, line) in grid.iter().enumerate() {
            for (j, &c) in line.iter().enumerate() {
                if c == Type::RIGHT {
                    let k = (j + 1) % line.len();
                    if grid[i][k] == Type::EMPTY {
                        new_grid[i][k] = Type::RIGHT;
                        new_grid[i][j] = Type::EMPTY;
                        moved = true;
                    }
                }
            }
        }
        grid = new_grid;
        let mut new_grid = grid.clone();
        for (i, line) in grid.iter().enumerate() {
            for (j, &c) in line.iter().enumerate() {
                if c == Type::DOWN {
                    let k = (i + 1) % grid.len();
                    if grid[k][j] == Type::EMPTY {
                        new_grid[k][j] = Type::DOWN;
                        new_grid[i][j] = Type::EMPTY;
                        moved = true;
                    }
                }
            }
        }
        grid = new_grid;
        cnt += 1;
        if !moved { break; }
    }
    cnt.to_string()
}

fn main() {
    let (part1, _) = run();
    println!("Part 1: {}", part1);
}

pub fn run() -> (String, String) {
    let input = get_input();
    (solve(&input), "".to_string())
}

#[bench]
fn bench(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| solve(&input))
}
