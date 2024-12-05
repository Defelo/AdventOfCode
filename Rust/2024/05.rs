#![feature(test)]

use std::cmp::Ordering;

use itertools::Itertools;
use rustc_hash::FxHashSet;

struct Input {
    rules: FxHashSet<Rule>,
    updates: Vec<Vec<Page>>,
}

type Rule = (Page, Page);
type Page = i32;

fn setup(input: &str) -> Input {
    let mut blocks = input.trim().split("\n\n");
    let rules = blocks
        .next()
        .unwrap()
        .lines()
        .map(|line| {
            line.split('|')
                .map(|x| x.parse().unwrap())
                .collect_tuple()
                .unwrap()
        })
        .collect();
    let updates = blocks
        .next()
        .unwrap()
        .lines()
        .map(|line| line.split(',').map(|x| x.parse().unwrap()).collect())
        .collect();
    Input { rules, updates }
}

fn make_sort_cmp(rules: &FxHashSet<Rule>) -> impl (Fn(&Page, &Page) -> Ordering) + Copy + use<'_> {
    |&a, &b| match () {
        _ if rules.contains(&(a, b)) => Ordering::Less,
        _ if rules.contains(&(b, a)) => Ordering::Greater,
        _ => Ordering::Equal,
    }
}

fn part1(input: &Input) -> Page {
    let cmp = make_sort_cmp(&input.rules);
    input
        .updates
        .iter()
        .filter(|&u| u.is_sorted_by(|a, b| cmp(a, b).is_le()))
        .map(|u| u[u.len() >> 1])
        .sum()
}

fn part2(input: &Input) -> Page {
    let cmp = make_sort_cmp(&input.rules);
    input
        .updates
        .iter()
        .filter(|&u| !u.is_sorted_by(|a, b| cmp(a, b).is_le()))
        .cloned()
        .map(|mut u| {
            let n = u.len() >> 1;
            *u.select_nth_unstable_by(n, cmp).1
        })
        .sum()
}

aoc::main!(2024, 5, ex: 1);
