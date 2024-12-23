#![feature(test)]
#![expect(unstable_name_collisions)]

use aoc::bitset::BitSet;
use itertools::Itertools;
use rustc_hash::FxHashMap;

#[derive(Debug)]
struct Input {
    graph: Vec<BitSet>,
    names: Vec<String>,
}

fn setup(input: &str) -> Input {
    let input = input.lines().map(|l| l.split('-').collect_tuple().unwrap());

    let mut idx = FxHashMap::default();
    for name in input.clone().flat_map(|(a, b)| [a, b]) {
        let i = idx.len();
        idx.entry(name).or_insert(i);
    }

    let mut graph = vec![BitSet::new(); idx.len()];
    for (a, b) in input.map(|(a, b)| (idx[a], idx[b])) {
        graph[a].insert(b);
        graph[b].insert(a);
    }

    let mut names = vec![String::new(); idx.len()];
    for (name, i) in idx {
        names[i] = name.into();
    }

    Input { graph, names }
}

fn max_bron_kerbosch(r: BitSet, mut p: BitSet, mut x: BitSet, graph: &[BitSet]) -> Option<BitSet> {
    if p.is_empty() && x.is_empty() {
        return Some(r);
    }

    let mut out = None::<BitSet>;
    let u = p.iter().chain(&x).next().unwrap();
    for v in &(p.clone() - &graph[u]) {
        let mut r = r.clone();
        r.insert(v);
        let n = &graph[v];
        let result = max_bron_kerbosch(r, p.clone() & n, x.clone() & n, graph);
        if result.as_ref().map(|s| s.len()) > out.as_ref().map(|s| s.len()) {
            out = result;
        }
        p.remove(v);
        x.insert(v);
    }
    out
}

fn part1(input: &Input) -> usize {
    let t = &input
        .names
        .iter()
        .enumerate()
        .filter_map(|(i, name)| name.starts_with('t').then_some(i))
        .collect::<BitSet>();
    input
        .graph
        .iter()
        .enumerate()
        .flat_map(|(a, an)| {
            an.iter().take_while(move |&b| b < a).flat_map(move |b| {
                an.iter()
                    .take_while(move |&c| c < b)
                    .filter(move |&c| input.graph[b].contains(c))
                    .filter(move |&c| [a, b, c].into_iter().any(|i| t.contains(i)))
            })
        })
        .count()
}

fn part2(input: &Input) -> String {
    max_bron_kerbosch(
        Default::default(),
        (0..input.graph.len()).collect(),
        Default::default(),
        &input.graph,
    )
    .unwrap()
    .into_iter()
    .map(|i| input.names[i].as_str())
    .sorted_unstable()
    .intersperse(",")
    .collect()
}

aoc::main!(2024, 23, ex: 1);
