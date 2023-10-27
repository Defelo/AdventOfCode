#![feature(test)]

use regex::Regex;
use rustc_hash::FxHashMap;

struct Input {
    bags: Vec<Vec<(u32, usize)>>,
    start: usize,
}

fn setup(input: &str) -> Input {
    let name_regex = Regex::new(r"^(.+?) bags").unwrap();
    let content_regex = Regex::new(r"(\d+) (.+?) bags?").unwrap();
    let names = input
        .lines()
        .enumerate()
        .map(|(i, line)| {
            let name = name_regex.captures(line).unwrap().get(1).unwrap().as_str();
            (name, i)
        })
        .collect::<FxHashMap<_, _>>();
    let bags = input
        .lines()
        .map(|line| {
            content_regex
                .captures_iter(line)
                .map(|c| {
                    let count = c.get(1).unwrap().as_str().parse().unwrap();
                    let name = c.get(2).unwrap().as_str();
                    (count, names[name])
                })
                .collect()
        })
        .collect();
    Input {
        bags,
        start: names["shiny gold"],
    }
}

fn part1(input: &Input) -> u32 {
    let mut rev = vec![vec![]; input.bags.len()];
    for (i, bag) in input.bags.iter().enumerate() {
        for &(_, j) in bag {
            rev[j].push(i);
        }
    }
    let mut out = 0;
    let mut queue = vec![input.start];
    let mut seen = vec![false; input.bags.len()];
    seen[input.start] = true;
    while let Some(p) = queue.pop() {
        for &q in &rev[p] {
            if !seen[q] {
                seen[q] = true;
                out += 1;
                queue.push(q);
            }
        }
    }
    out
}

fn count_bags(input: &Input, counts: &mut [Option<u32>], bag: usize) -> u32 {
    if let Some(out) = counts[bag] {
        return out;
    }

    let mut out = 1;
    for &(cnt, q) in &input.bags[bag] {
        out += cnt * count_bags(input, counts, q);
    }
    counts[bag] = Some(out);
    out
}

fn part2(input: &Input) -> u32 {
    let mut counts = vec![None; input.bags.len()];
    count_bags(input, &mut counts, input.start) - 1
}

aoc::main!(2020, 7, ex: 1, 2[b]);
