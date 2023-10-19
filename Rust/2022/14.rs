#![feature(test)]

use std::cmp::Ordering;

struct Input {
    grid: Vec<Vec<bool>>,
    x_offset: usize,
}

fn setup(input: &str) -> Input {
    let lines = input
        .trim()
        .lines()
        .map(|line| {
            line.split(" -> ")
                .map(|p| {
                    let mut it = p.split(',');
                    Some((it.next()?.parse().ok()?, it.next()?.parse().ok()?))
                })
                .collect::<Option<Vec<(usize, usize)>>>()
                .unwrap()
        })
        .collect::<Vec<_>>();
    let mut min_x = 500;
    let mut max_x = 500;
    let mut max_y = 0;
    for &(x, y) in lines.iter().flatten() {
        min_x = min_x.min(x);
        max_x = max_x.max(x);
        max_y = max_y.max(y);
    }
    let mut grid = vec![vec![false; max_x - min_x + 3]; max_y + 2];
    let x_offset = min_x - 1;
    for line in lines {
        let mut it = line.into_iter();
        let (mut x, mut y) = it.next().unwrap();
        grid[y][x - x_offset] = true;
        for (p, q) in it {
            let dx = match x.cmp(&p) {
                Ordering::Less => 2,
                Ordering::Equal => 1,
                Ordering::Greater => 0,
            };
            let dy = match y.cmp(&q) {
                Ordering::Less => 2,
                Ordering::Equal => 1,
                Ordering::Greater => 0,
            };
            while (x, y) != (p, q) {
                x = x + dx - 1;
                y = y + dy - 1;
                grid[y][x - x_offset] = true;
            }
        }
    }
    Input { grid, x_offset }
}

fn part1(input: &Input) -> usize {
    let mut grid = input.grid.clone();
    let mut out = 0;
    loop {
        let (mut x, mut y) = (500, 0);
        while y < grid.len() - 1 {
            if !grid[y + 1][x - input.x_offset] {
                y += 1;
            } else if !grid[y + 1][x - 1 - input.x_offset] {
                x -= 1;
                y += 1;
            } else if !grid[y + 1][x + 1 - input.x_offset] {
                x += 1;
                y += 1;
            } else {
                grid[y][x - input.x_offset] = true;
                out += 1;
                break;
            }
        }
        if y >= grid.len() - 1 {
            break;
        }
    }
    out
}

fn part2(input: &Input) -> usize {
    let mut grid = input.grid.clone();
    let mut out = 0;
    for y in 0..grid.len() {
        for x in 500 - y..=500 + y {
            if x < input.x_offset
                || x - input.x_offset >= grid[y].len()
                || !grid[y][x - input.x_offset]
            {
                out += 1;
            } else if x > input.x_offset
                && x + 1 < grid[y].len() + input.x_offset
                && grid[y][x - 1 - input.x_offset]
                && grid[y][x - input.x_offset]
                && grid[y][x + 1 - input.x_offset]
                && y + 1 < grid.len()
            {
                grid[y + 1][x - input.x_offset] = true;
            }
        }
    }
    out
}

aoc::main!(2022, 14, ex: 1);
