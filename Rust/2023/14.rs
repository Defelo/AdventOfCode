#![feature(test)]

use std::{
    hash::Hash,
    ops::{Index, IndexMut},
};

use itertools::Itertools;
use rustc_hash::FxHashMap;

type Input = Grid;

#[derive(Debug, Clone, Copy)]
enum Cell {
    Empty,
    Cube,
    Rounded,
}

#[derive(Debug, Clone)]
struct Grid {
    grid: Vec<Vec<Cell>>,
    rotation: u8,
}

fn setup(input: &str) -> Input {
    let grid = input
        .lines()
        .map(|line| {
            line.bytes()
                .map(|b| match b {
                    b'.' => Cell::Empty,
                    b'#' => Cell::Cube,
                    b'O' => Cell::Rounded,
                    _ => panic!(),
                })
                .collect()
        })
        .collect();
    Grid { grid, rotation: 0 }
}

impl Grid {
    fn width(&self) -> usize {
        if self.rotation % 2 == 0 {
            self.grid[0].len()
        } else {
            self.grid.len()
        }
    }

    fn height(&self) -> usize {
        if self.rotation % 2 == 0 {
            self.grid.len()
        } else {
            self.grid[0].len()
        }
    }

    fn index(&self, i: usize, j: usize) -> (usize, usize) {
        let w = self.grid[0].len();
        let h = self.grid.len();
        match self.rotation % 4 {
            0 => (i, j),
            1 => (w - j - 1, i),
            2 => (h - i - 1, w - j - 1),
            3 => (j, h - i - 1),
            _ => unreachable!(),
        }
    }

    fn tilt_north(&mut self) {
        for i in 1..self.height() {
            for j in 0..self.width() {
                if !matches!(self[(i, j)], Cell::Rounded) {
                    continue;
                }
                let mut k = i;
                while k > 0 && matches!(self[(k - 1, j)], Cell::Empty) {
                    self[(k, j)] = Cell::Empty;
                    self[(k - 1, j)] = Cell::Rounded;
                    k -= 1;
                }
            }
        }
    }

    fn total_load(&self) -> usize {
        self.grid
            .iter()
            .rev()
            .enumerate()
            .map(|(i, row)| row.iter().filter(|x| matches!(x, Cell::Rounded)).count() * (i + 1))
            .sum()
    }

    fn cycle(&mut self) {
        for i in 0..4 {
            self.rotation = i;
            self.tilt_north();
        }
    }

    fn hash(&self) -> impl Hash + Eq {
        type Chunk = u128;
        self.grid
            .iter()
            .flatten()
            .chunks(Chunk::BITS as _)
            .into_iter()
            .map(|c| c.fold(0, |acc, x| acc << 1 | matches!(x, Cell::Rounded) as Chunk))
            .fold(0, |a, b| a ^ b)
    }
}

impl Index<(usize, usize)> for Grid {
    type Output = Cell;

    fn index(&self, (i, j): (usize, usize)) -> &Self::Output {
        let (i, j) = self.index(i, j);
        &self.grid[i][j]
    }
}

impl IndexMut<(usize, usize)> for Grid {
    fn index_mut(&mut self, (i, j): (usize, usize)) -> &mut Self::Output {
        let (i, j) = self.index(i, j);
        &mut self.grid[i][j]
    }
}

fn part1(input: &Input) -> usize {
    let mut grid = input.clone();
    grid.tilt_north();
    grid.total_load()
}

fn part2(input: &Input) -> usize {
    let mut grid = input.clone();
    let mut n = 1000000000u32;
    let mut hist = FxHashMap::default();
    while n > 0 {
        if let Some(k) = hist.insert(grid.hash(), n) {
            let cycle_length = k - n;
            n %= cycle_length;
            break;
        }
        grid.cycle();
        n -= 1;
    }
    for _ in 0..n {
        grid.cycle();
    }
    grid.total_load()
}

aoc::main!(2023, 14, ex: 1);
