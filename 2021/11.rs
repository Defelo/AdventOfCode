#![feature(test)]

extern crate test;

use std::collections::HashSet;
use std::fs;
use test::Bencher;

type Input = Vec<Vec<u8>>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/11.txt").unwrap();
    puzzle.lines().map(|line| line.chars().map(|c| (c as u8) - 48).collect()).collect()
}

fn flash(grid: &mut Input) -> usize {
    let mut flashes: Vec<(usize, usize)> = vec![];
    for (i, row) in grid.iter_mut().enumerate() {
        for (j, x) in row.iter_mut().enumerate() {
            if *x >= 9 {
                flashes.push((i, j));
            } else {
                *x += 1;
            }
        }
    }

    let mut visited: HashSet<(usize, usize)> = HashSet::new();
    while !flashes.is_empty() {
        let (i, j) = flashes.pop().unwrap();

        if visited.contains(&(i, j)) { continue }
        visited.insert((i, j));

        grid[i][j] = 0;

        for k in 0..9 {
            if k == 4 {continue;}
            let p = (i as i32) + k / 3 - 1;
            let q = (j as i32) + k % 3 - 1;
            if p < 0 || q < 0 {continue;}
            let (p, q) = (p as usize, q as usize);
            if p >= grid.len() || q >= grid[0].len() {continue;}
            if grid[p][q] >= 9 {
                flashes.push((p, q));
            } else if !visited.contains(&(p, q)) {
                grid[p][q] += 1;
            }
        }
    }
    visited.len()
}

fn part1(input: &Input) -> String {
    let mut grid = input.clone();
    (0..100).map(|_| flash(&mut grid)).sum::<usize>().to_string()
}

fn part2(input: &Input) -> String {
    let mut grid = input.clone();
    let size = grid.len() * grid[0].len();
    (1..).filter(|_| flash(&mut grid) == size).next().unwrap().to_string()
}

pub fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
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
