#![feature(test)]

use rustc_hash::{FxHashMap, FxHashSet};

type Input = FxHashMap<i32, Vec<i32>>;

fn setup(input: &str) -> Input {
    let mut out = FxHashMap::default();
    let mut map = FxHashMap::default();
    map.insert("start", 0);
    map.insert("end", 1);
    for line in input.trim().lines() {
        let mut split = line.split('-').map(|x| {
            if !map.contains_key(x) {
                map.insert(x, (map.len() as i32) * if x < "a" { 1 } else { -1 });
            }
            *map.get(x).unwrap()
        });

        let a = split.next().unwrap();
        let b = split.next().unwrap();

        match out.get_mut(&a) {
            None => {
                out.insert(a, vec![b]);
            }
            Some(x) => {
                x.push(b);
            }
        }
        match out.get_mut(&b) {
            None => {
                out.insert(b, vec![a]);
            }
            Some(x) => {
                x.push(a);
            }
        }
    }
    out
}

fn search(node: i32, graph: &Input, visited: &mut FxHashSet<i32>, small_twice: bool) -> u32 {
    if node == 1 {
        return 1;
    }
    let mut twice = false;
    if node <= 0 && visited.contains(&node) {
        if small_twice || node == 0 {
            return 0;
        }
        twice = true;
    }
    visited.insert(node);

    let out = match graph.get(&node) {
        None => 0,
        Some(edges) => edges
            .iter()
            .cloned()
            .map(|next| search(next, graph, visited, twice || small_twice))
            .sum::<u32>(),
    };

    if !twice {
        visited.remove(&node);
    }
    out
}

fn part1(input: &Input) -> String {
    search(0, input, &mut FxHashSet::default(), true).to_string()
}

fn part2(input: &Input) -> String {
    search(0, input, &mut FxHashSet::default(), false).to_string()
}

aoc::main!(2021, 12, ex: 1, 2, 3);
