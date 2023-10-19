#![feature(test)]

use std::collections::VecDeque;

use itertools::Itertools;
use rustc_hash::FxHashSet;

type Input = Vec<Cube>;

#[derive(Clone, Hash, PartialEq, Eq)]
struct Cube(i16, i16, i16);

impl Cube {
    fn sides(&self) -> [CubeSide; 6] {
        [
            CubeSide(self.clone(), Dimension::X),
            CubeSide(self.clone(), Dimension::Y),
            CubeSide(self.clone(), Dimension::Z),
            CubeSide(Cube(self.0 - 1, self.1, self.2), Dimension::X),
            CubeSide(Cube(self.0, self.1 - 1, self.2), Dimension::Y),
            CubeSide(Cube(self.0, self.1, self.2 - 1), Dimension::Z),
        ]
    }

    fn neighbors(&self) -> [Self; 6] {
        [
            Cube(self.0 - 1, self.1, self.2),
            Cube(self.0 + 1, self.1, self.2),
            Cube(self.0, self.1 - 1, self.2),
            Cube(self.0, self.1 + 1, self.2),
            Cube(self.0, self.1, self.2 - 1),
            Cube(self.0, self.1, self.2 + 1),
        ]
    }
}

#[derive(Hash, PartialEq, Eq)]
struct CubeSide(Cube, Dimension);

#[derive(Hash, PartialEq, Eq)]
enum Dimension {
    X,
    Y,
    Z,
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| {
            let (x, y, z) = line
                .split(',')
                .map(|x| x.parse().unwrap())
                .collect_tuple()
                .unwrap();
            Cube(x, y, z)
        })
        .collect()
}

fn part1(input: &Input) -> usize {
    let mut sides = FxHashSet::with_capacity_and_hasher(input.len() * 6, Default::default());
    for cube in input {
        for side in cube.sides() {
            if sides.contains(&side) {
                sides.remove(&side);
            } else {
                sides.insert(side);
            }
        }
    }
    sides.len()
}

fn part2(input: &Input) -> usize {
    let mut out = 0;
    let (minx, maxx) = input.iter().map(|x| x.0).minmax().into_option().unwrap();
    let (miny, maxy) = input.iter().map(|x| x.1).minmax().into_option().unwrap();
    let (minz, maxz) = input.iter().map(|x| x.2).minmax().into_option().unwrap();
    let n = ((maxx - minx + 3) * (maxy - miny + 3) * (maxz - minz + 3)) as _;

    let cubes = input.iter().collect::<FxHashSet<_>>();
    let mut queue = VecDeque::with_capacity(n);
    queue.push_front(Cube(minx - 1, miny - 1, minz - 1));
    let mut visited = FxHashSet::with_capacity_and_hasher(n, Default::default());
    while let Some(p) = queue.pop_front() {
        if visited.contains(&p) {
            continue;
        }
        visited.insert(p.clone());

        for q in p.neighbors() {
            if !(minx - 1..=maxx + 1).contains(&q.0)
                || !(miny - 1..=maxy + 1).contains(&q.1)
                || !(minz - 1..=maxz + 1).contains(&q.2)
            {
                continue;
            }
            if cubes.contains(&q) {
                out += 1;
            } else if !visited.contains(&q) {
                queue.push_back(q);
            }
        }
    }

    out
}

aoc::main!(2022, 18, ex: 1);
