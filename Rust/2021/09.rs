#![feature(test)]
#![feature(binary_heap_into_iter_sorted)]

extern crate test;

use std::collections::{BinaryHeap, VecDeque};

use rustc_hash::FxHashSet;

type Input = Vec<Vec<u32>>;

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| line.trim().chars().map(|c| (c as u32) - 48).collect())
        .collect()
}

fn get_neighbors(i: i32, j: i32, w: i32, h: i32) -> impl Iterator<Item = (i32, i32)> {
    [(-1, 0), (1, 0), (0, -1), (0, 1)]
        .iter()
        .cloned()
        .filter_map(move |(dx, dy)| {
            let x = j + dx;
            let y = i + dy;
            if x >= 0 && y >= 0 && x < w && y < h {
                Option::Some((x, y))
            } else {
                Option::None
            }
        })
}

fn part1(input: &Input) -> String {
    let mut out = 0;
    for (i, line) in input.iter().enumerate() {
        for (j, c) in line.iter().enumerate() {
            if get_neighbors(i as i32, j as i32, line.len() as i32, input.len() as i32)
                .all(|(p, q)| c < &input[q as usize][p as usize].clone())
            {
                out += c + 1;
            }
        }
    }
    out.to_string()
}

fn part2(input: &Input) -> String {
    let mut sizes = BinaryHeap::new();
    for (i, line) in input.iter().enumerate() {
        for (j, c) in line.iter().enumerate() {
            if get_neighbors(i as i32, j as i32, line.len() as i32, input.len() as i32)
                .all(|(p, q)| c < &input[q as usize][p as usize].clone())
            {
                let mut queue = VecDeque::from([(i, j)]);
                let mut visited = FxHashSet::default();
                while !queue.is_empty() {
                    let (y, x) = queue.pop_front().unwrap();

                    if input[y][x] == 9 {
                        continue;
                    }

                    if visited.contains(&(y, x)) {
                        continue;
                    }
                    visited.insert((y, x));

                    for (p, q) in
                        get_neighbors(y as i32, x as i32, line.len() as i32, input.len() as i32)
                    {
                        queue.push_back((q as usize, p as usize));
                    }
                }
                sizes.push(visited.len());
            }
        }
    }
    sizes
        .into_iter_sorted()
        .take(3)
        .product::<usize>()
        .to_string()
}

aoc::main!(2021, 9, ex: 1);
