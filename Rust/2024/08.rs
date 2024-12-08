#![feature(test)]

use std::iter::successors;

use num::Integer;
use rustc_hash::{FxHashMap, FxHashSet};

type Input = Vec<Vec<u8>>;
type Position = (usize, usize);

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| line.bytes().collect())
        .collect()
}

fn solve<I>(input: &Input, antinode_positions: impl Fn(Position, Position) -> I) -> usize
where
    I: Iterator<Item = Position>,
{
    let mut frequency_map = FxHashMap::<_, Vec<_>>::default();
    for (y, row) in input.iter().enumerate() {
        for (x, &c) in row.iter().enumerate() {
            if c != b'.' {
                frequency_map.entry(c).or_default().push((x, y));
            }
        }
    }

    let mut antinodes = FxHashSet::default();
    for antennas in frequency_map.values() {
        for (i, &a) in antennas.iter().enumerate() {
            for &b in &antennas[..i] {
                antinodes.extend(antinode_positions(a, b));
            }
        }
    }

    antinodes.len()
}

fn part1(input: &Input) -> usize {
    solve(input, |(x1, y1), (x2, y2)| {
        let (x1, y1, x2, y2) = (x1 as isize, y1 as isize, x2 as isize, y2 as isize);
        let (dx, dy) = (x2 - x1, y2 - y1);
        [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]
            .into_iter()
            .flat_map(|(x, y)| Some((x.try_into().ok()?, y.try_into().ok()?)))
            .filter(|&(x, y)| x < input[0].len() && y < input.len())
    })
}

fn part2(input: &Input) -> usize {
    let on_grid = |(x, y): &(isize, isize)| {
        (0..input[0].len() as _).contains(x) && (0..input.len() as _).contains(y)
    };
    solve(input, |(x1, y1), (x2, y2)| {
        let (x1, y1, x2, y2) = (x1 as isize, y1 as isize, x2 as isize, y2 as isize);
        let (dx, dy) = (x2 - x1, y2 - y1);
        let gcd = dx.abs().gcd(&dy.abs());
        let (dx, dy) = (dx / gcd, dy / gcd);

        successors(Some((x1, y1)), move |&(x, y)| {
            Some((x + dx, y + dy)).filter(on_grid)
        })
        .chain(successors(Some((x2, y2)), move |&(x, y)| {
            Some((x - dx, y - dy)).filter(on_grid)
        }))
        .map(|(x, y)| (x as _, y as _))
    })
}

aoc::main!(2024, 8, ex: 1);
