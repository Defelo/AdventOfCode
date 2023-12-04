#![feature(test)]

use std::collections::VecDeque;

use rustc_hash::FxHashSet;

type Input = Vec<Card>;

#[derive(Debug)]
struct Card {
    winning: FxHashSet<u32>,
    mine: FxHashSet<u32>,
}

impl Card {
    fn matches(&self) -> usize {
        self.winning.intersection(&self.mine).count()
    }

    fn points(&self) -> usize {
        let matches = self.matches();
        if matches >= 1 {
            2usize.pow(matches as u32 - 1)
        } else {
            0
        }
    }
}

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            let mut line = line.split(':').nth(1).unwrap().split('|');
            let mut nums = || {
                line.next()
                    .unwrap()
                    .split_whitespace()
                    .map(|n| n.parse().unwrap())
                    .collect()
            };
            Card {
                winning: nums(),
                mine: nums(),
            }
        })
        .collect()
}

fn part1(input: &Input) -> usize {
    input.iter().map(Card::points).sum()
}

fn part2(input: &Input) -> usize {
    let mut counts = VecDeque::from(vec![1; input.len()]);
    input
        .iter()
        .map(|card| {
            let n = counts.pop_front().unwrap();
            counts
                .iter_mut()
                .take(card.matches())
                .for_each(|cnt| *cnt += n);
            n
        })
        .sum()
}

aoc::main!(2023, 4, ex: 1);
