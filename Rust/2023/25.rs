#![feature(test)]

use rustc_hash::FxHashMap;
use rustworkx_core::{
    connectivity::stoer_wagner_min_cut,
    petgraph::{Graph, Undirected},
};

type Input = Graph<(), (), Undirected, u16>;

fn setup(input: &str) -> Input {
    let mut graph = Graph::default();
    let mut nodes = FxHashMap::default();
    for line in input.lines() {
        let p = *nodes
            .entry(line.split(':').next().unwrap())
            .or_insert_with(|| graph.add_node(()));
        for q in line.split_whitespace().skip(1) {
            let q = *nodes.entry(q).or_insert_with(|| graph.add_node(()));
            graph.add_edge(p, q, ());
        }
    }
    graph
}

fn part1(input: &Input) -> usize {
    let x = stoer_wagner_min_cut(input, |_| Ok::<_, ()>(1))
        .unwrap()
        .unwrap()
        .1
        .len();
    x * (input.node_count() - x)
}

aoc::main!(2023, 25, ex: 1);
