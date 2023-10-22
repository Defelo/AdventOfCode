#![feature(test)]

use std::{
    cmp::{Ordering, Reverse},
    collections::BinaryHeap,
};

use aoc::iter_ext::IterExt;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug)]
struct Input {
    valves: Vec<Valve>,
    start: usize,
}

#[derive(Debug)]
struct Valve {
    rate: u32,
    tunnels: Vec<usize>,
}

fn setup(input: &str) -> Input {
    let names = input
        .lines()
        .enumerate()
        .map(|(i, line)| (line.split_whitespace().nth(1).unwrap(), i))
        .collect::<FxHashMap<_, _>>();
    let valves = input
        .lines()
        .map(|line| {
            let rate = line
                .split('=')
                .nth(1)
                .unwrap()
                .split(';')
                .next()
                .unwrap()
                .parse()
                .unwrap();
            let tunnels = line
                .split_whitespace()
                .skip(9)
                .map(|v| *names.get(v.trim_matches(',')).unwrap())
                .collect();
            Valve { rate, tunnels }
        })
        .collect();
    Input {
        valves,
        start: names["AA"],
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    human: SubState,
    elephant: SubState,
    released: u32,
    valves: ValveState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SubState {
    time: u32,
    position: usize,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        self.released.cmp(&other.released)
    }
}

impl SubState {
    fn next(
        self,
        graph: &Graph,
        valves: ValveState,
    ) -> impl Iterator<Item = (SubState, u32, ValveState)> + '_ {
        graph.closed_valves(valves).filter_map(move |(i, rate)| {
            let time = self
                .time
                .checked_sub(graph.distances[self.position][i] + 2)?
                + 1;
            Some((
                SubState { time, position: i },
                rate * time,
                valves.open_valve(i),
            ))
        })
    }
}

impl State {
    fn init(position: usize, human_time: u32, elephant_time: u32) -> Self {
        Self {
            human: SubState {
                time: human_time,
                position,
            },
            elephant: SubState {
                time: elephant_time,
                position,
            },
            released: 0,
            valves: ValveState::default(),
        }
    }

    fn update(
        self,
        human: Option<SubState>,
        elephant: Option<SubState>,
        released: u32,
        valves: ValveState,
    ) -> Self {
        Self {
            human: human.unwrap_or(self.human),
            elephant: elephant.unwrap_or(self.elephant),
            released: self.released + released,
            valves,
        }
    }

    fn next(self, graph: &Graph) -> impl Iterator<Item = State> + '_ {
        self.elephant
            .next(graph, self.valves)
            .flat_map(move |(new_elephant, elephant_released, valves)| {
                self.human
                    .next(graph, valves)
                    .map(move |(new_human, human_released, valves)| {
                        self.update(
                            Some(new_human),
                            Some(new_elephant),
                            human_released + elephant_released,
                            valves,
                        )
                    })
            })
            .chain_if_empty(self.human.next(graph, self.valves).map(
                move |(new_human, released, valves)| {
                    self.update(Some(new_human), None, released, valves)
                },
            ))
            .chain_if_empty(self.elephant.next(graph, self.valves).map(
                move |(new_elephant, released, valves)| {
                    self.update(None, Some(new_elephant), released, valves)
                },
            ))
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
struct ValveState(u64);

impl ValveState {
    fn is_valve_open(self, valve: usize) -> bool {
        self.0 & (1 << valve) != 0
    }

    fn open_valve(self, valve: usize) -> Self {
        Self(self.0 | (1 << valve))
    }
}

struct Graph {
    rates: Vec<u32>,
    distances: Vec<Vec<u32>>,
    start: usize,
}

impl Graph {
    fn from_input(input: &Input) -> Self {
        let distances = floyd_warshall(&input.valves);
        let rates = input.valves.iter().map(|v| v.rate).collect();
        Self {
            rates,
            distances,
            start: input.start,
        }
    }

    fn remove_irrelevant_nodes(mut self) -> Self {
        let relevant_valves = self
            .rates
            .iter()
            .enumerate()
            .filter(|&(i, &rate)| rate > 0 || i == self.start)
            .map(|(i, _)| i)
            .collect_vec();
        self.distances = relevant_valves
            .iter()
            .map(|&i| {
                relevant_valves
                    .iter()
                    .map(|&j| self.distances[i][j])
                    .collect()
            })
            .collect();
        self.start = relevant_valves
            .iter()
            .find_position(|&&i| i == self.start)
            .unwrap()
            .0;
        self.rates = relevant_valves.into_iter().map(|i| self.rates[i]).collect();
        self
    }

    fn sort_valves(mut self) -> Self {
        let indices = (0..self.rates.len())
            .sorted_unstable_by_key(|&i| Reverse(self.rates[i]))
            .collect_vec();
        self.distances = indices
            .iter()
            .map(|&i| indices.iter().map(|&j| self.distances[i][j]).collect())
            .collect();
        self.start = indices
            .iter()
            .find_position(|&&i| i == self.start)
            .unwrap()
            .0;
        self.rates = indices.into_iter().map(|i| self.rates[i]).collect();
        self
    }

    fn closed_valves(&self, valves: ValveState) -> impl Iterator<Item = (usize, u32)> + '_ {
        self.rates
            .iter()
            .copied()
            .enumerate()
            .filter(move |&(i, rate)| !valves.is_valve_open(i) && rate > 0)
    }
}

fn floyd_warshall(valves: &[Valve]) -> Vec<Vec<u32>> {
    let mut dist = vec![vec![u32::MAX; valves.len()]; valves.len()];
    for (i, valve) in valves.iter().enumerate() {
        for &j in &valve.tunnels {
            dist[i][j] = 1;
        }
        dist[i][i] = 0;
    }
    for k in 0..valves.len() {
        for i in 0..valves.len() {
            let di = dist[i][k];
            if di == u32::MAX {
                continue;
            }
            for j in 0..valves.len() {
                let dj = dist[k][j];
                if dj == u32::MAX {
                    continue;
                }
                dist[i][j] = dist[i][j].min(di + dj);
            }
        }
    }
    dist
}

fn solve(input: &Input, human_time: u32, elephant_time: u32) -> u32 {
    let graph = Graph::from_input(input)
        .remove_irrelevant_nodes()
        .sort_valves();

    let mut queue = BinaryHeap::from([State::init(graph.start, human_time, elephant_time)]);
    let mut seen = FxHashSet::default();
    let mut out = 0;
    while let Some(state) = queue.pop() {
        out = out.max(state.released);

        for next in state.next(&graph) {
            let closest = graph
                .closed_valves(next.valves)
                .map(|(i, _)| graph.distances[next.human.position][i])
                .min()
                .unwrap_or(0);
            let time = (next.human.time + 1).saturating_sub(closest);
            let max = next.released
                + graph
                    .closed_valves(next.valves)
                    .take(time as usize / 2)
                    .enumerate()
                    .map(|(i, (_, v))| v * (time - ((i as u32 + 1) * 2)))
                    .sum::<u32>();

            if max < out {
                continue;
            }

            if seen.insert(next) {
                queue.push(next);
            }
        }
    }

    out
}

fn part1(input: &Input) -> u32 {
    solve(input, 30, 0)
}

fn part2(input: &Input) -> u32 {
    solve(input, 26, 26)
}

aoc::main!(2022, 16, ex: 1);
