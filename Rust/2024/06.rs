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
}

impl Walk<'_> {
    fn from_input(input: &Input) -> Walk {
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
        }
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
            if !matches!(self.grid[ny][nx], Cell::Obstruction) {
                (self.x, self.y) = (nx, ny);
                break;
            }
            self.d = self.d.rotate_right();
        }

        Some((self.x, self.y, self.d))
    }
}

#[derive(Debug)]
struct Grid {
    width: usize,
    height: usize,
    start: (usize, usize),
    north: Vec<LookupRow>,
    east: Vec<LookupRow>,
    south: Vec<LookupRow>,
    west: Vec<LookupRow>,
}

#[derive(Debug)]
struct LookupRow(Vec<usize>);

impl LookupRow {
    fn lookup(&self, n: usize, extra: Option<usize>) -> usize {
        match extra {
            Some(x) if x <= n => self.0[n].min(n - x),
            _ => self.0[n],
        }
    }
}

impl FromIterator<bool> for LookupRow {
    fn from_iter<T: IntoIterator<Item = bool>>(iter: T) -> Self {
        Self(
            iter.into_iter()
                .scan(usize::MAX, |s, x| {
                    *s = if x { 0 } else { s.saturating_add(1) };
                    Some(*s)
                })
                .collect(),
        )
    }
}

impl Grid {
    fn from_input(input: &Input) -> Self {
        let width = input[0].len();
        let height = input.len();

        let start = input
            .iter()
            .enumerate()
            .find_map(|(y, row)| row.iter().index(&Cell::Guard).map(|x| (x, y)))
            .unwrap();

        let is_obstruction = |x: usize, y: usize| matches!(input[y][x], Cell::Obstruction);

        Self {
            width,
            height,
            start,
            north: (0..width)
                .into_par_iter()
                .map(|x| (0..height).map(|y| is_obstruction(x, y)).collect())
                .collect(),
            east: (0..height)
                .into_par_iter()
                .map(|y| (0..width).rev().map(|x| is_obstruction(x, y)).collect())
                .collect(),
            south: (0..width)
                .into_par_iter()
                .map(|x| (0..height).rev().map(|y| is_obstruction(x, y)).collect())
                .collect(),
            west: (0..height)
                .into_par_iter()
                .map(|y| (0..width).map(|x| is_obstruction(x, y)).collect())
                .collect(),
        }
    }

    fn check_cycle(&self, extra_obstruction: (usize, usize)) -> bool {
        let (mut x, mut y) = self.start;
        let mut d = Direction::North;
        let mut visited = FxHashSet::default();
        while visited.insert((x, y, d)) {
            let Some((nx, ny)) = self.walk(x, y, d, extra_obstruction) else {
                return false;
            };
            (x, y, d) = (nx, ny, d.rotate_right())
        }
        true
    }

    fn walk(
        &self,
        x: usize,
        y: usize,
        direction: Direction,
        extra_obstruction: (usize, usize),
    ) -> Option<(usize, usize)> {
        let eo = Some(extra_obstruction);
        Some(match direction {
            Direction::North => self.north[x].lookup(y, eo.filter(|eo| eo.0 == x).map(|eo| eo.1)),
            Direction::East => self.east[y].lookup(
                self.width - x - 1,
                eo.filter(|eo| eo.1 == y).map(|eo| self.width - eo.0 - 1),
            ),
            Direction::South => self.south[x].lookup(
                self.height - y - 1,
                eo.filter(|eo| eo.0 == x).map(|eo| self.height - eo.1 - 1),
            ),
            Direction::West => self.west[y].lookup(x, eo.filter(|eo| eo.1 == y).map(|eo| eo.0)),
        })
        .filter(|&r| r != usize::MAX)
        .map(|r| {
            let r = r - 1;
            match direction {
                Direction::North => (x, y - r),
                Direction::East => (x + r, y),
                Direction::South => (x, y + r),
                Direction::West => (x - r, y),
            }
        })
    }
}

fn part1(input: &Input) -> usize {
    Walk::from_input(input)
        .map(|(x, y, _)| (x, y))
        .collect::<FxHashSet<_>>()
        .len()
}

fn part2(input: &Input) -> usize {
    let candidates = Walk::from_input(input)
        .map(|(x, y, _)| (x, y))
        .filter(|&(x, y)| matches!(input[y][x], Cell::Empty))
        .collect::<FxHashSet<_>>();

    let grid = Grid::from_input(input);

    candidates
        .into_par_iter()
        .filter(|&(ox, oy)| grid.check_cycle((ox, oy)))
        .count()
}

aoc::main!(2024, 6, ex: 1);
