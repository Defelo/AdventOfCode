#![feature(test)]

use aoc::{grid::Direction, iter_ext::IterExt};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use rustc_hash::FxHashSet;

type Input = Vec<Vec<Cell>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cell {
    Guard,       // ^
    Obstruction, // #
    Empty,       // .
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| {
            line.trim()
                .bytes()
                .map(|b| match b {
                    b'^' => Cell::Guard,
                    b'#' => Cell::Obstruction,
                    b'.' => Cell::Empty,
                    _ => panic!(),
                })
                .collect()
        })
        .collect()
}

#[derive(Debug)]
struct Walk<'a> {
    x: usize,
    y: usize,
    d: Direction,
    grid: &'a Input,
    init: bool,
    extra_obstruction: Option<(usize, usize)>,
}

impl Walk<'_> {
    fn from_input(input: &Input, extra_obstruction: Option<(usize, usize)>) -> Walk {
        let (x, y) = input
            .iter()
            .enumerate()
            .find_map(|(y, row)| row.iter().index(&Cell::Guard).map(|x| (x, y)))
            .unwrap();
        Walk {
            x,
            y,
            d: Direction::North,
            grid: input,
            init: false,
            extra_obstruction,
        }
    }

    fn check_cycle(self) -> bool {
        let mut visited = FxHashSet::default();
        for p in self {
            if !visited.insert(p) {
                return true;
            }
        }
        false
    }
}

impl Iterator for Walk<'_> {
    type Item = (usize, usize, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        if !self.init {
            self.init = true;
            return Some((self.x, self.y, self.d));
        }

        loop {
            let (nx, ny) = self
                .d
                .step(self.x, self.y, self.grid[0].len(), self.grid.len())?;
            if !matches!(self.grid[ny][nx], Cell::Obstruction)
                && self.extra_obstruction != Some((nx, ny))
            {
                (self.x, self.y) = (nx, ny);
                break;
            }
            self.d = self.d.rotate_right();
        }

        Some((self.x, self.y, self.d))
    }
}

fn part1(input: &Input) -> usize {
    Walk::from_input(input, None)
        .map(|(x, y, _)| (x, y))
        .collect::<FxHashSet<_>>()
        .len()
}

fn part2(input: &Input) -> usize {
    let candidates = Walk::from_input(input, None)
        .flat_map(|(x, y, d)| {
            [d, d.rotate_right()]
                .into_iter()
                .flat_map(move |d| d.step(x, y, input[0].len(), input.len()))
        })
        .filter(|&(x, y)| matches!(input[y][x], Cell::Empty))
        .collect::<FxHashSet<_>>();

    candidates
        .into_par_iter()
        .filter(|&(ox, oy)| Walk::from_input(input, Some((ox, oy))).check_cycle())
        .count()
}

aoc::main!(2024, 6, ex: 1);
