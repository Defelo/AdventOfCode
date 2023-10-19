#![feature(test)]

use rustc_hash::{FxHashMap, FxHashSet};

struct Line {
    patterns: Vec<String>,
    output: Vec<String>,
}

type Input = Vec<Line>;

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            let mut line = line.trim().split(" | ");
            let mut parse = || {
                line.next()
                    .unwrap()
                    .split(' ')
                    .map(|x| x.to_string())
                    .collect()
            };
            Line {
                patterns: parse(),
                output: parse(),
            }
        })
        .collect()
}

fn part1(input: &Input) -> String {
    input
        .iter()
        .map(|line| {
            line.output
                .iter()
                .filter(|x| [2, 3, 4, 7].contains(&x.len()))
                .count()
        })
        .sum::<usize>()
        .to_string()
}

fn parse_num(num: &str) -> u16 {
    num.chars()
        .map(|c| 1 << ((c as u16) - ('a' as u16)))
        .sum::<u16>()
}

fn is_subset(mut a: u16, mut b: u16) -> bool {
    while a > 0 {
        if 1 & a & !b == 1 {
            return false;
        }
        a >>= 1;
        b >>= 1;
    }
    true
}

fn part2(input: &Input) -> String {
    let digits = FxHashMap::from_iter([
        ((7, 2), 1),
        ((2, 5), 2),
        ((3, 5), 3),
        ((3, 4), 4),
        ((4, 5), 5),
        ((5, 3), 7),
        ((1, 7), 8),
    ]);

    input
        .iter()
        .map(|line| {
            let patterns: Vec<u16> = line.patterns.iter().map(|s| parse_num(s)).collect();
            let output: Vec<u16> = line.output.iter().map(|s| parse_num(s)).collect();
            let mut mp: FxHashMap<u16, u8> = FxHashMap::default();
            let mut rp: FxHashMap<u8, u16> = FxHashMap::default();
            let mut u: FxHashSet<u16> = FxHashSet::from_iter(patterns.iter().cloned());
            for x in &patterns {
                let sc = patterns.iter().filter(|y| is_subset(*x, **y)).count();
                match digits.get(&(sc, x.count_ones())) {
                    None => {}
                    Some(digit) => {
                        mp.insert(*x, *digit);
                        rp.insert(*digit, *x);
                        u.remove(x);
                    }
                }
            }

            let x = *u
                .iter()
                .find(|x| !is_subset(*rp.get(&5).unwrap(), **x))
                .unwrap();
            mp.insert(x, 0);
            u.remove(&x);

            let x = *u
                .iter()
                .find(|x| is_subset(*rp.get(&4).unwrap(), **x))
                .unwrap();
            mp.insert(x, 9);
            u.remove(&x);

            let x = *u.iter().next().unwrap();
            mp.insert(x, 6);

            output
                .iter()
                .map(|x| *mp.get(x).unwrap() as u32)
                .reduce(|a, b| a * 10 + b)
                .unwrap()
        })
        .sum::<u32>()
        .to_string()
}

aoc::main!(2021, 8, ex: 1[b], 2);
