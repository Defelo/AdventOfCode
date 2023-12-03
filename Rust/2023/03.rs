#![feature(test)]

use itertools::{FoldWhile::Continue, Itertools};
use rustc_hash::FxHashMap;

type Input = Vec<Vec<Cell>>;

#[derive(Debug, Clone, Copy)]
enum Cell {
    Digit(u8),
    Symbol { gear: bool },
    Empty,
}

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            line.bytes()
                .map(|b| match b {
                    b'0'..=b'9' => Cell::Digit(b - b'0'),
                    b'.' => Cell::Empty,
                    b'*' => Cell::Symbol { gear: true },
                    _ => Cell::Symbol { gear: false },
                })
                .collect()
        })
        .collect()
}

fn find_part_numbers(
    input: &Input,
) -> impl Iterator<Item = (u32, impl Iterator<Item = (usize, usize)> + '_)> + '_ {
    input.iter().enumerate().flat_map(move |(i, line)| {
        line.iter()
            .copied()
            .enumerate()
            .batching(|it| {
                let (num, j) = it.find_map(|(j, c)| match c {
                    Cell::Digit(d) => Some((d as u32, j)),
                    _ => None,
                })?;
                let (num, k) = it
                    .fold_while((num, j), |acc @ (num, _), (k, c)| {
                        match match c {
                            Cell::Digit(d) => Some(d),
                            _ => None,
                        } {
                            Some(d) => Continue((num * 10 + d as u32, k)),
                            None => itertools::FoldWhile::Done(acc),
                        }
                    })
                    .into_inner();
                Some((num, j, k))
            })
            .map(move |(num, j, k)| {
                let gears = (i.saturating_sub(1)..=(i + 1).min(input.len() - 1))
                    .dedup()
                    .flat_map(move |i| {
                        (j.saturating_sub(1)..=(k + 1).min(input[i].len() - 1))
                            .filter(move |&j| matches!(input[i][j], Cell::Symbol { .. }))
                            .map(move |j| (i, j))
                    });
                (num, gears)
            })
    })
}

fn part1(input: &Input) -> u32 {
    find_part_numbers(input)
        .filter_map(|(num, mut parts)| parts.next().map(|_| num))
        .sum()
}

fn part2(input: &Input) -> u32 {
    let mut gears = FxHashMap::<(usize, usize), _>::default();
    for (num, gs) in find_part_numbers(input) {
        for g in gs {
            gears.entry(g).or_insert_with(Vec::new).push(num);
        }
    }
    gears
        .into_iter()
        .filter(|&((i, j), ref nums)| {
            matches!(input[i][j], Cell::Symbol { gear: true }) && nums.len() == 2
        })
        .map(|(_, nums)| nums.into_iter().product::<u32>())
        .sum()
}

aoc::main!(2023, 3, ex: 1);
