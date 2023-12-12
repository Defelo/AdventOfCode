#![feature(test, iter_intersperse)]

use ndarray::Array2;
use rayon::prelude::*;

type Input = Vec<Report>;

#[derive(Debug)]
struct Report {
    springs: Vec<Spring>,
    groups: Vec<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Spring {
    Unknown,
    Damaged,
    Operational,
}

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            let mut split = line.split_whitespace();
            let springs = split
                .next()
                .unwrap()
                .bytes()
                .map(|b| match b {
                    b'?' => Spring::Unknown,
                    b'#' => Spring::Damaged,
                    b'.' => Spring::Operational,
                    _ => panic!(),
                })
                .collect();
            let groups = split
                .next()
                .unwrap()
                .split(',')
                .map(|group| group.parse().unwrap())
                .collect();
            Report { springs, groups }
        })
        .collect()
}

fn count(report: &Report) -> usize {
    let mut dp = Array2::zeros((report.springs.len() + 1, report.groups.len() + 1));
    dp[[report.springs.len(), report.groups.len()]] = 1;
    for i in (0..report.springs.len()).rev() {
        for j in 0..report.groups.len() + 1 {
            if matches!(report.springs[i], Spring::Damaged | Spring::Unknown)
                && j < report.groups.len()
                && i + report.groups[j] <= report.springs.len()
                && !report.springs[i..i + report.groups[j]].contains(&Spring::Operational)
                && (i + report.groups[j] >= report.springs.len()
                    || report.springs[i + report.groups[j]] != Spring::Damaged)
            {
                dp[[i, j]] += dp[[report.springs.len().min(i + report.groups[j] + 1), j + 1]];
            }

            if matches!(report.springs[i], Spring::Operational | Spring::Unknown) {
                dp[[i, j]] += dp[[report.springs.len().min(i + 1), j]];
            }
        }
    }
    dp[[0, 0]]
}

fn part1(input: &Input) -> usize {
    input.par_iter().map(count).sum()
}

fn part2(input: &Input) -> usize {
    input
        .par_iter()
        .map(|report| {
            count(&Report {
                springs: std::iter::once(&report.springs[..])
                    .cycle()
                    .take(5)
                    .intersperse(&[Spring::Unknown])
                    .flatten()
                    .copied()
                    .collect(),
                groups: std::iter::once(&report.groups)
                    .cycle()
                    .take(5)
                    .flatten()
                    .copied()
                    .collect(),
            })
        })
        .sum()
}

aoc::main!(2023, 12, ex: 1);
