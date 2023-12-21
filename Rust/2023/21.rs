#![feature(test)]

use std::collections::VecDeque;

use aoc::{grid::Direction, tuples::HomogeneousTupleExt};
use itertools::Itertools;

type Position = (usize, usize);

#[derive(Debug)]
struct Input {
    grid: Vec<Vec<bool>>,
    size: usize,
    start: Position,
}

fn setup(input: &str) -> Input {
    let grid = input
        .lines()
        .map(|line| line.bytes().map(|b| b != b'#').collect_vec())
        .collect_vec();
    let start = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| line.bytes().position(|b| b == b'S').map(|x| (x, y)))
        .next()
        .unwrap();

    let size = grid.len();

    Input { grid, size, start }
}

fn bfs(input: &Input) -> impl Iterator<Item = u64> + '_ {
    let mut queue = VecDeque::from([(false, input.start.bimap(|x| x as isize))]);
    let mut visited = vec![false; input.size * input.size * 25];
    let size = input.size as isize * 5;
    std::iter::repeat_with(move || {
        let (d, p) = queue.pop_front()?;

        let (rx, ry) = p.bimap(|x| x.rem_euclid(size) as usize);
        let i = rx + ry * input.size * 5;
        if visited[i] {
            return None;
        }
        visited[i] = true;

        queue.extend(
            Direction::iter()
                .map(|d| d.step_signed(p))
                .filter(|q| {
                    let (x, y) = q.bimap(|x| x.rem_euclid(input.size as _) as usize);
                    input.grid[y][x]
                })
                .map(|q| (!d, q)),
        );

        Some(d)
    })
    .flatten()
    .tuple_windows()
    .map(|(d1, d2)| d1 != d2)
    .scan((0, 0), |(cnt1, cnt2), swap| {
        *cnt1 += 1;
        Some(swap.then(|| {
            std::mem::swap(cnt1, cnt2);
            *cnt2
        }))
    })
    .flatten()
}

fn part1(input: &Input) -> u64 {
    bfs(input).nth(64).unwrap()
}

fn part2(input: &Input) -> u64 {
    const N: usize = 26501365;

    let (f0, f1, f2) = bfs(input)
        .skip(N % input.size)
        .step_by(input.size)
        .take(3)
        .collect_tuple()
        .unwrap();

    let c = f0;
    let b = (4 * f1 - f2 - 3 * f0) / 2;
    let a = (f2 - 2 * f1 + f0) / 2;
    let x = (N / input.size) as u64;

    a * x * x + b * x + c
}

aoc::main!(2023, 21);
