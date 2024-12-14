#![feature(test)]

use std::cmp::Ordering;

use itertools::Itertools;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};

#[derive(Debug, Clone)]
struct Input {
    width: i64,
    height: i64,
    robots: Vec<Robot>,
}

#[derive(Debug, Clone, Copy)]
struct Robot {
    px: i64,
    py: i64,
    vx: i64,
    vy: i64,
}

fn setup(input: &str) -> Input {
    let mut lines = input.trim().lines().peekable();
    let (width, height) = if let Some(l) = lines.next_if(|l| l.starts_with('#')) {
        l.trim_matches('#')
            .trim()
            .split(',')
            .map(|n| n.parse().unwrap())
            .collect_tuple()
            .unwrap()
    } else {
        (101, 103)
    };

    let robots = lines
        .map(|l| {
            let (px, py, vx, vy) = l
                .split(|c: char| !matches!(c, '0'..='9' | '-'))
                .filter(|n| !n.is_empty())
                .map(|n| n.parse().unwrap())
                .collect_tuple()
                .unwrap();
            Robot { px, py, vx, vy }
        })
        .collect();

    Input {
        width,
        height,
        robots,
    }
}

fn part1(input: &Input) -> usize {
    input
        .robots
        .iter()
        .flat_map(|&Robot { px, py, vx, vy }| {
            let px = (px + 100 * vx).rem_euclid(input.width);
            let py = (py + 100 * vy).rem_euclid(input.height);
            match (
                (px * 2 + 1).cmp(&input.width),
                (py * 2 + 1).cmp(&input.height),
            ) {
                (Ordering::Less, Ordering::Less) => Some(0),
                (Ordering::Less, Ordering::Greater) => Some(1),
                (Ordering::Greater, Ordering::Less) => Some(2),
                (Ordering::Greater, Ordering::Greater) => Some(3),
                _ => None,
            }
        })
        .fold([0; 4], |mut acc, q| {
            acc[q] += 1;
            acc
        })
        .into_iter()
        .product()
}

fn part2(input: &Input) -> usize {
    (0..usize::MAX)
        .into_par_iter()
        .by_exponential_blocks()
        .find_first(|&n| {
            let mut bots = vec![false; (input.width * input.height) as _];
            input.robots.iter().all(|Robot { px, py, vx, vy }| {
                let px = (px + n as i64 * vx).rem_euclid(input.width);
                let py = (py + n as i64 * vy).rem_euclid(input.height);
                let k = (py * input.width + px) as usize;
                !std::mem::replace(&mut bots[k], true)
            })
        })
        .unwrap() as _
}

aoc::main!(2024, 14, ex: 1[a]);
