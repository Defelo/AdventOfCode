#![feature(test)]
#![feature(binary_heap_into_iter_sorted)]

extern crate test;

use std::collections::{BinaryHeap, HashSet, VecDeque};
use std::fs;
use test::Bencher;

type Input = Vec<Vec<u32>>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/09.txt").unwrap();
    puzzle.trim().lines().map(|line| {
        line.trim().chars().map(|c| (c as u32) - 48).collect()
    }).collect()
}

fn get_neighbors(i: i32, j: i32, w: i32, h: i32) -> impl Iterator<Item=(i32, i32)> {
    [(-1, 0), (1, 0), (0, -1), (0, 1)].iter().cloned().filter_map(move |(dx, dy)| {
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
                .all(|(p, q)| c < &input[q as usize][p as usize].clone()) {
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
                .all(|(p, q)| c < &input[q as usize][p as usize].clone()) {
                let mut queue = VecDeque::from([(i, j)]);
                let mut visited = HashSet::new();
                while !queue.is_empty() {
                    let (y, x) = queue.pop_front().unwrap();

                    if input[y][x] == 9 { continue; }

                    if visited.contains(&(y, x)) { continue; }
                    visited.insert((y, x));

                    for (p, q) in get_neighbors(y as i32, x as i32, line.len() as i32, input.len() as i32) {
                        queue.push_back((q as usize, p as usize));
                    }
                }
                sizes.push(visited.len());
            }
        }
    }
    sizes.into_iter_sorted().take(3).product::<usize>().to_string()
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
    b.iter(|| { part1(&input) })
}

#[bench]
fn bench_part2(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| { part2(&input) })
}
