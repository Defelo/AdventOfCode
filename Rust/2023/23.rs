#![feature(test)]

use aoc::{grid::Direction, iter_ext::IterExt, tuples::TupleExt};
use indexmap::IndexMap;
use itertools::Itertools;
use rayon::prelude::*;
use rustc_hash::FxHashMap;

type Input = Vec<Vec<Tile>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Path,
    Forest,
    Slope(Direction),
}

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            line.bytes()
                .map(|b| match b {
                    b'.' => Tile::Path,
                    b'#' => Tile::Forest,
                    b => Tile::Slope(b.into()),
                })
                .collect()
        })
        .collect()
}

fn trace_path<const IGNORE_SLOPES: bool>(
    input: &Input,
    p: (usize, usize),
    d: Direction,
) -> impl Iterator<Item = (usize, usize)> + '_ {
    let (w, h) = (input[0].len(), input.len());
    std::iter::successors(Some((p, d)), move |&(p, d)| {
        [d.rotate_left(), d, d.rotate_right()]
            .into_iter()
            .filter(|&dir| IGNORE_SLOPES || !matches!(input[p.1][p.0], Tile::Slope(s) if s != dir))
            .filter_map(|dir| dir.step(p.0, p.1, w, h).map(|q| (q, dir)))
            .find(|&((x, y), _)| input[y][x] != Tile::Forest)
    })
    .map(|(p, _)| p)
}

fn longest_path(junctions: &[FxHashMap<usize, usize>], p: usize, visited: u64) -> Option<usize> {
    if p == 1 {
        Some(0)
    } else if visited.count_ones() < 10 {
        junctions[p]
            .par_iter()
            .filter(|&(&q, _)| visited & (1 << q) == 0)
            .filter_map(|(&q, &d)| longest_path(junctions, q, visited | (1 << p)).map(|x| x + d))
            .max()
    } else {
        junctions[p]
            .iter()
            .filter(|&(&q, _)| visited & (1 << q) == 0)
            .filter_map(|(&q, &d)| longest_path(junctions, q, visited | (1 << p)).map(|x| x + d))
            .max()
    }
}

fn solve<const IGNORE_SLOPES: bool>(input: &Input) -> usize {
    let (w, h) = (input[0].len(), input.len());
    let start = (input[0].iter().index(&Tile::Path).unwrap(), 0);
    let end = (input[h - 1].iter().index(&Tile::Path).unwrap(), h - 1);

    let junction_ids = [start, end]
        .into_iter()
        .chain(
            (0..w)
                .flat_map(|x| (0..h).map(move |y| (x, y)))
                .filter(|&(x, y)| input[y][x] == Tile::Path)
                .filter(|&(x, y)| {
                    Direction::iter()
                        .filter_map(|d| d.step(x, y, w, h))
                        .filter(|&(x, y)| input[y][x] != Tile::Forest)
                        .count()
                        > 2
                }),
        )
        .enumerate()
        .map(TupleExt::swap)
        .collect::<IndexMap<(usize, usize), usize>>();

    let junctions = junction_ids
        .iter()
        .map(|(&p, _)| {
            Direction::iter()
                .filter_map(|d| {
                    let p = d
                        .step(p.0, p.1, w, h)
                        .filter(|&(x, y)| input[y][x] != Tile::Forest)?;
                    trace_path::<IGNORE_SLOPES>(input, p, d)
                        .enumerate()
                        .find_map(|(dist, p)| junction_ids.get(&p).map(|&m| (m, dist + 1)))
                })
                .collect()
        })
        .collect_vec();

    longest_path(&junctions, 0, 0).unwrap()
}

fn part1(input: &Input) -> usize {
    solve::<false>(input)
}

fn part2(input: &Input) -> usize {
    solve::<true>(input)
}

aoc::main!(2023, 23, ex: 1);
