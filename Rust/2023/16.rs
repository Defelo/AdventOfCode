#![feature(test)]

use core::panic;

use aoc::grid::Direction;
use rayon::prelude::*;

type Input = Vec<Vec<Cell>>;
type Position = (usize, usize);

#[derive(Debug, Clone, Copy)]
enum Cell {
    Empty,
    Mirror1,
    Mirror2,
    SplitterVert,
    SplitterHoriz,
}

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            line.bytes()
                .map(|b| match b {
                    b'.' => Cell::Empty,
                    b'/' => Cell::Mirror1,
                    b'\\' => Cell::Mirror2,
                    b'|' => Cell::SplitterVert,
                    b'-' => Cell::SplitterHoriz,
                    _ => panic!(),
                })
                .collect()
        })
        .collect()
}

impl Cell {
    fn step1(
        self,
        (x, y): Position,
        d: Direction,
        w: usize,
        h: usize,
    ) -> Option<(Position, Direction)> {
        let d = match self {
            Cell::Empty => d,
            Cell::Mirror1 => match d {
                Direction::North => Direction::East,
                Direction::East => Direction::North,
                Direction::South => Direction::West,
                Direction::West => Direction::South,
            },
            Cell::Mirror2 => match d {
                Direction::North => Direction::West,
                Direction::East => Direction::South,
                Direction::South => Direction::East,
                Direction::West => Direction::North,
            },
            Cell::SplitterVert => match d {
                Direction::East | Direction::West => Direction::North,
                _ => d,
            },
            Cell::SplitterHoriz => match d {
                Direction::North | Direction::South => Direction::West,
                _ => d,
            },
        };
        Some((d.step(x, y, w, h)?, d))
    }

    fn step2(
        self,
        (x, y): Position,
        d: Direction,
        w: usize,
        h: usize,
    ) -> Option<(Position, Direction)> {
        let d = match self {
            Cell::SplitterVert => match d {
                Direction::East | Direction::West => Direction::South,
                _ => d,
            },
            Cell::SplitterHoriz => match d {
                Direction::North | Direction::South => Direction::East,
                _ => d,
            },
            _ => return None,
        };
        Some((d.step(x, y, w, h)?, d))
    }
}

fn count(input: &Input, start: Position, direction: Direction) -> usize {
    let n = input.len() * input[0].len();
    let mut queue = Vec::new();
    let mut visited = vec![false; n * 4];
    let mut visited_tiles = vec![false; n];
    queue.push((start, direction));
    while let Some((p, d)) = queue.pop() {
        let i = (p.0 + p.1 * input.len()) * 4 + d as usize;
        if visited[i] {
            continue;
        }
        visited[i] = true;
        visited_tiles[i / 4] = true;

        let c = input[p.1][p.0];
        queue.extend(c.step1(p, d, input[0].len(), input.len()));
        queue.extend(c.step2(p, d, input[0].len(), input.len()));
    }
    visited_tiles.into_iter().filter(|&x| x).count()
}

fn part1(input: &Input) -> usize {
    count(input, (0, 0), Direction::East)
}

fn part2(input: &Input) -> usize {
    (0..input.len())
        .flat_map(|y| {
            [
                (0, y, Direction::East),
                (input[0].len() - 1, y, Direction::West),
            ]
        })
        .chain((0..input[0].len()).flat_map(|x| {
            [
                (x, 0, Direction::South),
                (x, input.len() - 1, Direction::North),
            ]
        }))
        .par_bridge()
        .map(|(x, y, d)| count(input, (x, y), d))
        .max()
        .unwrap()
}

aoc::main!(2023, 16, ex: 1);
