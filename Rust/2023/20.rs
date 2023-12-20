#![feature(test)]

use std::collections::VecDeque;

use aoc::{iter_ext::IterExt, tuples::TupleExt};
use itertools::Itertools;
use num::Integer;
use rustc_hash::FxHashMap;

#[derive(Debug)]
struct Input {
    modules: Vec<Module>,
    start: Vec<usize>,
    rx: usize,
}

#[derive(Debug)]
struct Module {
    ty: ModuleType,
    inputs: Vec<usize>,
    outputs: Vec<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ModuleType {
    FlipFlop,
    Conjunction,
}

fn setup(input: &str) -> Input {
    let module_ids = input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .next()
                .unwrap()
                .trim_start_matches(['%', '&'])
        })
        .filter(|&name| name != "broadcaster")
        .enumerate()
        .map(TupleExt::swap)
        .collect::<FxHashMap<_, _>>();

    let mut modules = input
        .lines()
        .filter(|line| !line.starts_with("broadcaster "))
        .map(|line| Module {
            ty: match line.bytes().next().unwrap() {
                b'%' => ModuleType::FlipFlop,
                b'&' => ModuleType::Conjunction,
                _ => panic!(),
            },
            inputs: vec![],
            outputs: line
                .split(" -> ")
                .nth(1)
                .unwrap()
                .split(", ")
                .map(|m| module_ids.get(m).copied().unwrap_or(usize::MAX))
                .collect(),
        })
        .collect_vec();

    let mut rx = usize::MAX;
    for p in 0..modules.len() {
        for q in modules[p].outputs.clone() {
            if q != usize::MAX {
                modules[q].inputs.push(p);
            } else {
                rx = p;
            }
        }
    }

    let start = input
        .lines()
        .find(|line| line.starts_with("broadcaster "))
        .unwrap()
        .split(" -> ")
        .nth(1)
        .unwrap()
        .split(", ")
        .map(|m| module_ids[m])
        .collect();

    Input { modules, start, rx }
}

fn push_button<'a>(
    input: &'a Input,
    state: &'a mut [bool],
) -> impl Iterator<Item = (usize, bool)> + 'a {
    let mut queue = input
        .start
        .iter()
        .map(|&i| (i, false))
        .collect::<VecDeque<_>>();

    std::iter::from_fn(move || {
        let (p, k) = queue.pop_front()?;

        let Some(module) = input.modules.get(p) else {
            return Some((p, k));
        };

        state[p] = match module.ty {
            ModuleType::FlipFlop if k => return Some((p, k)),
            ModuleType::FlipFlop => !state[p],
            ModuleType::Conjunction => !module.inputs.iter().all(|&i| state[i]),
        };

        for &q in &module.outputs {
            queue.push_back((q, state[p]));
        }

        Some((p, k))
    })
}

fn part1(input: &Input) -> usize {
    let mut state = vec![false; input.modules.len()];
    let (lo, hi) = (0..1000)
        .map(|_| {
            push_button(input, &mut state)
                .map(|(_, k)| k)
                .fold((1, 0), |(lo, hi), k| (lo + !k as usize, hi + k as usize))
        })
        .fold((0, 0), |acc, x| (acc.0 + x.0, acc.1 + x.1));
    lo * hi
}

fn part2(input: &Input) -> usize {
    let rx = &input.modules[input.rx];
    let mut state = vec![false; input.modules.len()];
    (1..)
        .filter(|_| {
            push_button(input, &mut state).any_consume(|(p, k)| !k && rx.inputs.contains(&p))
        })
        .take(rx.inputs.len())
        .reduce(|a, b| a.lcm(&b))
        .unwrap()
}

aoc::main!(2023, 20, ex: 1[a], 2[a]);
