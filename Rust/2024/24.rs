#![feature(test)]
#![expect(unstable_name_collisions)]

use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug)]
struct Input {
    values: FxHashMap<String, bool>,
    gates: FxHashMap<String, (Gate, String, String)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Gate {
    And,
    Or,
    Xor,
}

fn setup(input: &str) -> Input {
    let mut lines = input.lines();
    let values = lines
        .by_ref()
        .take_while(|l| !l.trim().is_empty())
        .map(|l| {
            let mut s = l.split(": ");
            let name = s.next().unwrap().into();
            let value = s.next().unwrap().trim() == "1";
            (name, value)
        })
        .collect();

    let gates = lines
        .map(|l| {
            let mut s = l.split_whitespace();
            let a = s.next().unwrap().into();
            let gate = match s.next().unwrap() {
                "AND" => Gate::And,
                "OR" => Gate::Or,
                "XOR" => Gate::Xor,
                _ => panic!(),
            };
            let b = s.next().unwrap().into();
            s.next();
            let c = s.next().unwrap().into();
            (c, (gate, a, b))
        })
        .collect();

    Input { values, gates }
}

fn part1(input: &Input) -> usize {
    let mut forward = FxHashMap::<_, Vec<_>>::default();
    let mut unknown = input
        .gates
        .iter()
        .map(|(c, (_, a, b))| {
            forward.entry(a).or_default().push(c);
            forward.entry(b).or_default().push(c);
            let dependencies = [a, b]
                .into_iter()
                .filter(|&x| !input.values.contains_key(x))
                .collect::<FxHashSet<_>>();
            (c, dependencies)
        })
        .filter(|(_, v)| !v.is_empty())
        .collect::<FxHashMap<_, _>>();
    let mut values = input
        .values
        .iter()
        .map(|(p, &v)| (p, v))
        .collect::<FxHashMap<_, _>>();
    let mut stack = input
        .gates
        .keys()
        .filter(|&p| !unknown.contains_key(p))
        .collect::<Vec<_>>();
    while let Some(p) = stack.pop() {
        let (gate, ref a, ref b) = input.gates[p];
        let a = values[a];
        let b = values[b];
        let c = match gate {
            Gate::And => a & b,
            Gate::Or => a | b,
            Gate::Xor => a ^ b,
        };
        values.insert(p, c);

        for &q in forward.get(p).into_iter().flatten() {
            if unknown.contains_key(q) {
                unknown.get_mut(q).unwrap().remove(p);
                if unknown[q].is_empty() {
                    unknown.remove(q);
                    stack.push(q);
                }
            }
        }
    }

    values
        .keys()
        .filter(|k| k.starts_with('z'))
        .sorted_unstable()
        .rev()
        .fold(0, |acc, k| acc << 1 | (values[k] as usize))
}

fn part2(input: &Input) -> String {
    let last_z = input
        .gates
        .keys()
        .filter(|k| k.starts_with('z'))
        .max()
        .unwrap();
    let mut forward = FxHashMap::<_, Vec<_>>::default();
    for (c, (_, a, b)) in &input.gates {
        forward.entry(a).or_default().push(c);
        forward.entry(b).or_default().push(c);
    }
    input
        .gates
        .keys()
        .filter(|&g| {
            let (gate, ref a, ref b) = input.gates[g];
            gate != Gate::Xor && g.starts_with('z') && g != last_z
                || gate == Gate::Xor
                    && forward
                        .get(g)
                        .into_iter()
                        .flatten()
                        .any(|&x| input.gates[x].0 == Gate::Or)
                || gate == Gate::And
                    && a != "x00"
                    && b != "x00"
                    && forward
                        .get(g)
                        .into_iter()
                        .flatten()
                        .any(|&x| input.gates[x].0 != Gate::Or)
                || gate == Gate::Xor
                    && !g.starts_with('z')
                    && [a, b]
                        .into_iter()
                        .any(|x| input.gates.get(x).is_some_and(|x| x.0 == Gate::Xor))
        })
        .sorted_unstable()
        .map(|s| s.as_str())
        .intersperse(",")
        .collect()
}

aoc::main!(2024, 24, ex: 1[a], 2[a]);
