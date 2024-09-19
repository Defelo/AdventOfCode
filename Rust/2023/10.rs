#![feature(test)]

use aoc::iter_ext::IterExt;
use itertools::Itertools;
use rustc_hash::FxHashSet;

#[derive(Debug)]
struct Input {
    grid: Vec<Vec<Cell>>,
    start: Position,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cell {
    Vertical,
    Horizontal,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
    Ground,
    Start,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Position {
    x: usize,
    y: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    fn invert(self) -> Self {
        use Direction::{East, North, South, West};
        match self {
            North => South,
            East => West,
            South => North,
            West => East,
        }
    }
}

impl Position {
    fn step(self, direction: Direction) -> Option<Self> {
        let x = match direction {
            Direction::West => self.x.checked_sub(1)?,
            Direction::East => self.x + 1,
            Direction::North | Direction::South => self.x,
        };
        let y = match direction {
            Direction::North => self.y.checked_sub(1)?,
            Direction::South => self.y + 1,
            Direction::West | Direction::East => self.y,
        };
        Some(Self { x, y })
    }
}

impl Cell {
    fn connections(self) -> &'static [Direction] {
        use Direction::{East, North, South, West};
        match self {
            Cell::Vertical => &[North, South],
            Cell::Horizontal => &[East, West],
            Cell::NorthEast => &[North, East],
            Cell::NorthWest => &[North, West],
            Cell::SouthWest => &[South, West],
            Cell::SouthEast => &[South, East],
            Cell::Ground => &[],
            Cell::Start => &[North, East, South, West],
        }
    }
}

fn setup(input: &str) -> Input {
    let grid = input
        .lines()
        .map(|line| {
            line.bytes()
                .map(|b| match b {
                    b'|' => Cell::Vertical,
                    b'-' => Cell::Horizontal,
                    b'L' => Cell::NorthEast,
                    b'J' => Cell::NorthWest,
                    b'7' => Cell::SouthWest,
                    b'F' => Cell::SouthEast,
                    b'.' => Cell::Ground,
                    b'S' => Cell::Start,
                    _ => panic!(),
                })
                .collect_vec()
        })
        .collect_vec();

    let start = grid
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter()
                .enumerate()
                .filter(|&(_, &cell)| cell == Cell::Start)
                .map(move |(x, _)| Position { x, y })
        })
        .next()
        .unwrap();

    Input { grid, start }
}

fn checked_step(input: &Input, p: Position, d: Direction) -> Option<Position> {
    p.step(d)
        .filter(|q| input.grid[q.y][q.x].connections().contains(&d.invert()))
}

fn find_loop(input: &Input) -> impl Iterator<Item = Position> + '_ {
    IterExt::take_while_inclusive(
        std::iter::successors(
            Some((
                input.start,
                input
                    .start
                    .step(start_cell(input).connections()[0])
                    .unwrap(),
            )),
            |&(p, q)| {
                let r = input.grid[q.y][q.x]
                    .connections()
                    .iter()
                    .filter_map(|&d| checked_step(input, q, d))
                    .find(|&r| r != p)
                    .unwrap();

                Some((q, r))
            },
        ),
        |&(_, q)| q != input.start,
    )
    .map(|(p, _)| p)
}

fn start_cell(input: &Input) -> Cell {
    let north = checked_step(input, input.start, Direction::North).is_some();
    let east = checked_step(input, input.start, Direction::East).is_some();
    let south = checked_step(input, input.start, Direction::South).is_some();
    let west = checked_step(input, input.start, Direction::West).is_some();
    match (north, east, south, west) {
        (true, false, true, false) => Cell::Vertical,
        (false, true, false, true) => Cell::Horizontal,
        (true, true, false, false) => Cell::NorthEast,
        (true, false, false, true) => Cell::NorthWest,
        (false, false, true, true) => Cell::SouthWest,
        (false, true, true, false) => Cell::SouthEast,
        _ => panic!(),
    }
}

fn part1(input: &Input) -> usize {
    find_loop(input).count() / 2
}

fn part2(input: &Input) -> usize {
    let path = find_loop(input).collect::<FxHashSet<_>>();
    input
        .grid
        .iter()
        .enumerate()
        .map(|(y, row)| {
            row.iter()
                .map(|&cell| match cell {
                    Cell::Start => start_cell(input),
                    _ => cell,
                })
                .enumerate()
                .map(|(x, cell)| (cell, path.contains(&Position { x, y })))
                .fold((false, 0), |(inside, cnt), (cell, on_loop)| match cell {
                    Cell::Vertical | Cell::SouthEast | Cell::SouthWest if on_loop => (!inside, cnt),
                    _ => (inside, cnt + (inside && !on_loop) as usize),
                })
                .1
        })
        .sum()
}

aoc::main!(2023, 10, ex: 1[a], 2[a], 3[b], 4[b], 5[b]);
