use std::collections::VecDeque;

use itertools::Itertools;
use rustc_hash::FxHashSet;

type Input = Vec<Cube>;

#[derive(Clone, Hash, PartialEq, Eq)]
struct Cube(i8, i8, i8);

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
    let mut out = part1(input);
    let (minx, maxx) = input.iter().map(|x| x.0).minmax().into_option().unwrap();
    let (miny, maxy) = input.iter().map(|x| x.1).minmax().into_option().unwrap();
    let (minz, maxz) = input.iter().map(|x| x.2).minmax().into_option().unwrap();

    let cubes = input.iter().collect::<FxHashSet<_>>();
    let mut checked = FxHashSet::default();
    let mut queue = VecDeque::new();
    let mut visited = FxHashSet::default();
    for candidate in input
        .iter()
        .flat_map(|x| x.neighbors())
        .filter(|x| !cubes.contains(x))
    {
        if checked.contains(&candidate) {
            continue;
        }
        queue.clear();
        queue.push_front(candidate);
        visited.clear();
        loop {
            let Some(p) = queue.pop_front() else {
                out -= visited.iter().flat_map(|p: &Cube| p.neighbors()).filter(|q| cubes.contains(q)).count();
                break;
            };

            if !(minx..=maxx).contains(&p.0)
                || !(miny..=maxy).contains(&p.1)
                || !(minz..=maxz).contains(&p.2)
            {
                break;
            }

            if visited.contains(&p) {
                continue;
            }
            visited.insert(p.clone());

            for q in p.neighbors() {
                if !cubes.contains(&q) && !visited.contains(&q) {
                    queue.push_back(q);
                }
            }
        }

        checked.extend(visited.drain());
    }

    out
}

aoc::main!(2022, 18);
aoc::example!(ex01, "18.1.txt", 64, 58);
aoc::test_input!("18.txt", 4244, 2460);
