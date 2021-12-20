#![feature(test)]

extern crate test;

use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::fs;
use test::Bencher;
use rustc_hash::FxHashSet;

type Input = Vec<Vec<u32>>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/15.txt").unwrap();
    puzzle.lines().map(|line| {
        line.chars().map(|c| (c as u32) - 0x30).collect()
    }).collect()
}

fn dijkstra(grid: &Input, k: usize) -> u32 {
    let w = grid[0].len();
    let h = grid.len();
    let mut queue = BinaryHeap::new();
    queue.push(Reverse((0u32, 0usize, 0usize)));
    let mut visited = FxHashSet::default();
    while !queue.is_empty() {
        let (d, x, y) = queue.pop().unwrap().0;

        if visited.contains(&(x, y)) { continue; }
        visited.insert((x, y));

        if x == w * k - 1 && y == h * k - 1 { return d; }

        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let p = (x as i32) + dx;
            let q = (y as i32) + dy;
            if p < 0 || q < 0 { continue; }
            let p = p as usize;
            let q = q as usize;
            if p >= k * w || q >= k * h { continue; }
            if visited.contains(&(p, q)) { continue; }
            let c = grid[q % h][p % w] + (q / h + p / w) as u32;
            queue.push(Reverse((d + (c - 1) % 9 + 1, p, q)));
        }
    }
    panic!();
}

fn part1(input: &Input) -> String {
    dijkstra(input, 1).to_string()
}

fn part2(input: &Input) -> String {
    dijkstra(input, 5).to_string()
}

fn main() {
    let (part1, part2) = run();
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}

pub fn run() -> (String, String) {
    let input = get_input();
    (part1(&input), part2(&input))
}

#[bench]
fn bench_part1(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| part1(&input))
}

#[bench]
fn bench_part2(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| part2(&input))
}
