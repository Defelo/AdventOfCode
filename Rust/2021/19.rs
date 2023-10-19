#![feature(test)]

use std::{
    hash::Hash,
    ops::{Add, Sub},
};

use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct Pos {
    x: i32,
    y: i32,
    z: i32,
}

impl Pos {
    fn rotation(&self, i: usize) -> Pos {
        let &Pos { x, y, z } = self;
        match i {
            0 => Pos { x, y, z },
            1 => Pos { x, y: -z, z: y },
            2 => Pos { x, y: -y, z: -z },
            3 => Pos { x, y: z, z: -y },
            4 => Pos { x: -x, y: -y, z },
            5 => Pos {
                x: -x,
                y: -z,
                z: -y,
            },
            6 => Pos { x: -x, y, z: -z },
            7 => Pos { x: -x, y: z, z: y },
            8 => Pos { x: y, y: -x, z },
            9 => Pos { x: y, y: -z, z: -x },
            10 => Pos { x: y, y: x, z: -z },
            11 => Pos { x: y, y: z, z: x },
            12 => Pos { x: -y, y: x, z },
            13 => Pos { x: -y, y: -z, z: x },
            14 => Pos {
                x: -y,
                y: -x,
                z: -z,
            },
            15 => Pos { x: -y, y: z, z: -x },
            16 => Pos { x: z, y: -y, z: x },
            17 => Pos { x: z, y: -x, z: -y },
            18 => Pos { x: z, y, z: -x },
            19 => Pos { x: z, y: x, z: y },
            20 => Pos { x: -z, y, z: x },
            21 => Pos { x: -z, y: -x, z: y },
            22 => Pos {
                x: -z,
                y: -y,
                z: -x,
            },
            23 => Pos { x: -z, y: x, z: -y },
            _ => panic!(),
        }
    }

    fn distance(self, other: Pos) -> i32 {
        let diff = self - other;
        diff.x.pow(2) + diff.y.pow(2) + diff.z.pow(2)
    }

    fn manhatten_distance(self, other: Pos) -> i32 {
        let diff = self - other;
        diff.x.abs() + diff.y.abs() + diff.z.abs()
    }
}

impl Add for Pos {
    type Output = Pos;

    fn add(self, rhs: Self) -> Self::Output {
        Pos {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl Sub for Pos {
    type Output = Pos;

    fn sub(self, rhs: Self) -> Self::Output {
        Pos {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

type Scanner = (Vec<Pos>, FxHashSet<i32>);
type Input = Vec<Scanner>;

fn setup(input: &str) -> Input {
    input
        .split("\n\n")
        .map(|scanner| {
            let poss: Vec<Pos> = scanner
                .lines()
                .skip(1)
                .map(|line| {
                    let mut res = line.split(',').map(|x| x.parse().unwrap());
                    Pos {
                        x: res.next().unwrap(),
                        y: res.next().unwrap(),
                        z: res.next().unwrap(),
                    }
                })
                .collect();
            let distances: FxHashSet<i32> = poss
                .iter()
                .flat_map(|a| poss.iter().map(|b| a.distance(*b)))
                .collect();
            (poss, distances)
        })
        .collect()
}

fn match_scanner(
    beacons: &mut FxHashSet<Pos>,
    scanner_positions: &mut Vec<Pos>,
    scanner: &Scanner,
) {
    for i in 0..24 {
        let mut counter: FxHashMap<Pos, usize> = FxHashMap::default();
        for rel in &scanner.0 {
            let rel = rel.rotation(i);
            for abs in beacons.iter() {
                let k = *abs - rel;
                let cnt = counter.get(&k).cloned().unwrap_or(0) + 1;
                counter.insert(k, cnt);
                if cnt >= 12 {
                    for x in &scanner.0 {
                        let x = x.rotation(i) + k;
                        beacons.insert(x);
                    }
                    scanner_positions.push(k);
                    return;
                }
            }
        }
    }
}

fn solve(input: &Input) -> (usize, i32) {
    let mut remaining: Vec<Scanner> = input.to_vec();
    let first = remaining.remove(0);
    let mut beacons: FxHashSet<Pos> = FxHashSet::from_iter(first.0);
    let mut distances: FxHashSet<i32> = FxHashSet::from_iter(first.1);
    let mut scanners: Vec<Pos> = vec![];
    while !remaining.is_empty() {
        let i = (0..remaining.len())
            .max_by_key(|&i| remaining[i].1.intersection(&distances).count())
            .unwrap();

        let s = remaining.remove(i);
        match_scanner(&mut beacons, &mut scanners, &s);
        for x in &s.1 {
            distances.insert(*x);
        }
    }
    (
        beacons.len(),
        scanners
            .iter()
            .flat_map(|x| scanners.iter().map(|&y| x.manhatten_distance(y)))
            .max()
            .unwrap(),
    )
}

fn part1(input: &Input) -> usize {
    solve(input).0
}

fn part2(input: &Input) -> i32 {
    solve(input).1
}

aoc::main!(2021, 19, ex: 1);
