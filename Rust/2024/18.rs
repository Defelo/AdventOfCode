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

fn bfs(input: &Input, prefix_len: usize) -> Option<usize> {
    let mut queue = VecDeque::from([(0, 0, 0)]);
    let mut visited = vec![false; input.width * input.height];
    while let Some((d, x, y)) = queue.pop_front() {
        let idx = y * input.width + x;
        if std::mem::replace(&mut visited[idx], true) {
            continue;
        }

        if (x, y) == (input.width - 1, input.height - 1) {
            return Some(d);
        }

        queue.extend(
            Direction::iter()
                .flat_map(|d| d.step(x, y, input.width, input.height))
                .filter(|&(nx, ny)| input.grid[ny * input.width + nx] >= prefix_len)
                .map(|(nx, ny)| (d + 1, nx, ny)),
        );
    }

    None
}

fn part1(input: &Input) -> usize {
    bfs(input, input.prefix_len).unwrap()
}

fn part2(input: &Input) -> String {
    let mut left = 0;
    let mut right = input.bytes.len();
    while left + 1 < right {
        let m = left.midpoint(right);
        match bfs(input, m) {
            Some(_) => left = m,
            None => right = m,
        }
    }
    debug_assert_eq!(left + 1, right);
    let (x, y) = input.bytes[right - 1];
    format!("{x},{y}")
}

aoc::main!(2024, 18, ex: 1);
