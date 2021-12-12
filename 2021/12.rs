#![feature(test)]

extern crate test;

use std::collections::{HashMap, HashSet};
use std::fs;
use test::Bencher;

type Input = HashMap<i32, Vec<i32>>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/12.txt").unwrap();
    let mut out = HashMap::new();
    let mut map = HashMap::from([("start", 0), ("end", 1)]);
    for line in puzzle.trim().lines() {
        let mut split = line.split("-").map(|x| {
            if !map.contains_key(x) { map.insert(x, (map.len() as i32) * if x < "a" { 1 } else { -1 }); }
            *map.get(x).unwrap()
        });

        let a = split.next().unwrap();
        let b = split.next().unwrap();

        match out.get_mut(&a) {
            None => { out.insert(a, vec![b]); }
            Some(x) => { x.push(b); }
        }
        match out.get_mut(&b) {
            None => { out.insert(b, vec![a]); }
            Some(x) => { x.push(a); }
        }
    }
    out
}

fn search(node: i32, graph: &Input, visited: &mut HashSet<i32>, small_twice: bool) -> u32 {
    if node == 1 { return 1; }
    let mut twice = false;
    if node <= 0 && visited.contains(&node) {
        if small_twice || node == 0 { return 0; }
        twice = true;
    }
    visited.insert(node);

    let out = match graph.get(&node) {
        None => 0,
        Some(edges) => edges.iter().cloned().map(|next|
            search(next, graph, visited, twice || small_twice)
        ).sum::<u32>()
    };

    if !twice { visited.remove(&node); }
    out
}

fn part1(input: &Input) -> String {
    search(0, input, &mut HashSet::new(), true).to_string()
}

fn part2(input: &Input) -> String {
    search(0, input, &mut HashSet::new(), false).to_string()
}

fn main() {
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
