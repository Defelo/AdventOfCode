#![feature(test)]

use std::{cmp::Reverse, collections::BinaryHeap};

use aoc::grid::Direction;
use rustc_hash::{FxHashMap, FxHashSet};

struct Input {
    start: (usize, usize),
    end: (usize, usize),
    grid: Vec<Vec<bool>>,
}

fn setup(input: &str) -> Input {
    let mut start = None;
    let mut end = None;
    let grid = input
        .trim()
        .lines()
        .enumerate()
        .map(|(y, l)| {
            l.bytes()
                .enumerate()
                .map(|(x, b)| match b {
                    b'.' => true,
                    b'#' => false,
                    b'S' => {
                        start = Some((x, y));
                        true
                    }
                    b'E' => {
                        end = Some((x, y));
                        true
                    }
                    _ => panic!(),
                })
                .collect()
        })
        .collect();

    Input {
        start: start.unwrap(),
        end: end.unwrap(),
        grid,
    }
}

fn part1(input: &Input) -> usize {
    let mut pq = BinaryHeap::from([Reverse((0, input.start, Direction::East))]);
    let mut visited = FxHashSet::default();
    while let Some(Reverse((d, p @ (x, y), dir))) = pq.pop() {
        if !visited.insert((p, dir)) {
            continue;
        }

        if p == input.end {
            return d;
        }

        pq.push(Reverse((d + 1000, p, dir.rotate_left())));
        pq.push(Reverse((d + 1000, p, dir.rotate_right())));
        pq.extend(
            dir.step(x, y, input.grid[0].len(), input.grid.len())
                .filter(|&(qx, qy)| input.grid[qy][qx])
                .map(|q| Reverse((d + 1, q, dir))),
        );
    }

    panic!()
}

fn part2(input: &Input) -> usize {
    let mut pq = BinaryHeap::from([Reverse((0usize, input.start, Direction::East))]);
    let mut costs = FxHashMap::default();
    while let Some(Reverse((d, p @ (x, y), dir))) = pq.pop() {
        if costs.contains_key(&(p, dir)) {
            continue;
        }
        costs.insert((p, dir), d);

        if p == input.end {
            continue;
        }

        pq.push(Reverse((d + 1000, p, dir.rotate_left())));
        pq.push(Reverse((d + 1000, p, dir.rotate_right())));
        pq.extend(
            dir.step(x, y, input.grid[0].len(), input.grid.len())
                .filter(|&(qx, qy)| input.grid[qy][qx])
                .map(|q| Reverse((d + 1, q, dir))),
        );
    }

    let total_cost = Direction::iter()
        .flat_map(|d| costs.get(&(input.end, d)))
        .copied()
        .min()
        .unwrap();
    let mut stack = Direction::iter()
        .filter(|&dir| costs.get(&(input.end, dir)) == Some(&total_cost))
        .map(|dir| (input.end, dir))
        .collect::<Vec<_>>();
    let mut visited = FxHashSet::default();
    let mut tiles = FxHashSet::default();
    while let Some((p @ (x, y), dir)) = stack.pop() {
        tiles.insert(p);
        if !visited.insert((p, dir)) {
            continue;
        }

        if p == input.start {
            continue;
        }

        let d = costs[&(p, dir)];

        stack.extend(
            [
                (
                    1,
                    dir.invert()
                        .step(x, y, input.grid[0].len(), input.grid.len())
                        .filter(|&(qx, qy)| input.grid[qy][qx]),
                    dir,
                ),
                (1000, Some(p), dir.rotate_left()),
                (1000, Some(p), dir.rotate_right()),
            ]
            .into_iter()
            .flat_map(|(c, p, dir)| {
                let p = p?;
                costs
                    .get(&(p, dir))
                    .is_some_and(|&pc| pc + c == d)
                    .then_some((p, dir))
            }),
        );
    }

    tiles.len()
}

aoc::main!(2024, 16, ex: 1, 2);
