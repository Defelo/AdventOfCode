#![feature(test)]

use itertools::Itertools;

type Input = Vec<Machine>;

#[derive(Debug, Clone, Copy)]
struct Machine {
    a: (i64, i64),
    b: (i64, i64),
    prize: (i64, i64),
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .split("\n\n")
        .map(|block| {
            let mut lines = block.lines().map(|l| {
                l.split(|c: char| !matches!(c, '0'..='9' | '-'))
                    .filter(|s| !s.is_empty())
                    .map(|n| n.parse().unwrap())
                    .collect_tuple()
                    .unwrap()
            });
            Machine {
                a: lines.next().unwrap(),
                b: lines.next().unwrap(),
                prize: lines.next().unwrap(),
            }
        })
        .collect()
}

fn solve(
    Machine {
        a: (ax, ay),
        b: (bx, by),
        prize: (px, py),
    }: Machine,
) -> Option<i64> {
    let a = (py * bx - px * by) / (bx * ay - ax * by);
    let b = (py * ax - px * ay) / (ax * by - bx * ay);
    (a * ax + b * bx == px && a * ay + b * by == py).then_some(a * 3 + b)
}

fn part1(input: &Input) -> i64 {
    input.iter().copied().flat_map(solve).sum()
}

fn part2(input: &Input) -> i64 {
    input
        .iter()
        .flat_map(
            |&m @ Machine {
                 prize: (px, py), ..
             }| {
                solve(Machine {
                    prize: (px + 10000000000000, py + 10000000000000),
                    ..m
                })
            },
        )
        .sum()
}

aoc::main!(2024, 13, ex: 1);
