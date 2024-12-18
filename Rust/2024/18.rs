#![feature(test)]

use std::collections::VecDeque;

use aoc::grid::Direction;
use itertools::Itertools;

struct Input {
    width: usize,
    height: usize,
    grid: Vec<usize>,
    prefix_len: usize,
    bytes: Vec<(usize, usize)>,
}

fn setup(input: &str) -> Input {
    let mut lines = input.trim().lines().peekable();
    let (width, height, prefix_len) = match lines.next_if(|l| l.starts_with('#')) {
        Some(line) => line
            .trim_start_matches('#')
            .split_whitespace()
            .map(|n| n.parse().unwrap())
            .collect_tuple()
            .unwrap(),
        None => (71, 71, 1024),
    };

    let bytes = lines
        .map(|l| {
            l.split(',')
                .map(|n| n.parse().unwrap())
                .collect_tuple()
                .unwrap()
        })
        .collect::<Vec<_>>();

    let mut grid = vec![usize::MAX; width * height];
    for (i, &(x, y)) in bytes.iter().enumerate() {
        grid[y * width + x] = i;
    }

    Input {
        width,
        height,
        grid,
        prefix_len,
        bytes,
    }
}

struct UnionFind {
    parents: Vec<usize>,
}

impl UnionFind {
    fn new(len: usize) -> Self {
        Self {
            parents: (0..len).collect(),
        }
    }

    fn find(&mut self, x: usize) -> usize {
        if self.parents[x] == x {
            return x;
        }
        self.parents[x] = self.find(self.parents[x]);
        self.parents[x]
    }

    fn merge(&mut self, x: usize, y: usize) {
        let x = self.find(x);
        let y = self.find(y);
        self.parents[x] = y;
    }
}

fn part1(input: &Input) -> usize {
    let mut queue = VecDeque::from([(0, 0, 0)]);
    let mut visited = vec![false; input.width * input.height];
    while let Some((d, x, y)) = queue.pop_front() {
        let idx = y * input.width + x;
        if std::mem::replace(&mut visited[idx], true) {
            continue;
        }

        if (x, y) == (input.width - 1, input.height - 1) {
            return d;
        }

        queue.extend(
            Direction::iter()
                .flat_map(|d| d.step(x, y, input.width, input.height))
                .filter(|&(nx, ny)| input.grid[ny * input.width + nx] >= input.prefix_len)
                .map(|(nx, ny)| (d + 1, nx, ny)),
        );
    }

    panic!()
}

fn part2(input: &Input) -> String {
    let mut uf = UnionFind::new(input.width * input.height);
    for y in 0..input.height {
        for x in 0..input.width {
            let idx = y * input.width + x;
            if x + 1 < input.width
                && input.grid[idx] == usize::MAX
                && input.grid[idx + 1] == usize::MAX
            {
                uf.merge(idx, idx + 1);
            }
            if y + 1 < input.height
                && input.grid[idx] == usize::MAX
                && input.grid[idx + input.width] == usize::MAX
            {
                uf.merge(idx, idx + input.width);
            }
        }
    }

    for (i, &(x, y)) in input.bytes.iter().enumerate().rev() {
        for (nx, ny) in Direction::iter()
            .flat_map(|d| d.step(x, y, input.width, input.height))
            .filter(|&(nx, ny)| input.grid[ny * input.width + nx] > i)
        {
            uf.merge(y * input.width + x, ny * input.width + nx);
        }
        if uf.find(0) == uf.find(input.width * input.height - 1) {
            return format!("{x},{y}");
        }
    }

    panic!()
}

aoc::main!(2024, 18, ex: 1);
