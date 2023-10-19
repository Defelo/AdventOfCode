#![feature(test)]

use rustc_hash::{FxHashMap, FxHashSet};

const SHAPES: &[Shape] = &[
    Shape {
        width: 4,
        height: 1,
        rock: &[(0, 0), (0, 1), (0, 2), (0, 3)],
    },
    Shape {
        width: 3,
        height: 3,
        rock: &[(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)],
    },
    Shape {
        width: 3,
        height: 3,
        rock: &[(0, 2), (1, 2), (2, 0), (2, 1), (2, 2)],
    },
    Shape {
        width: 1,
        height: 4,
        rock: &[(0, 0), (1, 0), (2, 0), (3, 0)],
    },
    Shape {
        width: 2,
        height: 2,
        rock: &[(0, 0), (0, 1), (1, 0), (1, 1)],
    },
];

const WIDTH: u8 = 7;
const START_X: u8 = 2;
const START_Y: u64 = 3;

type Input = Vec<Direction>;

#[derive(Clone, Copy)]
enum Direction {
    Left,
    Right,
}

struct Shape<'a> {
    width: u8,
    height: u64,
    rock: &'a [(u64, u8)],
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .bytes()
        .map(|c| match c {
            b'<' => Direction::Left,
            b'>' => Direction::Right,
            _ => panic!(),
        })
        .collect()
}

struct Simulation<'a> {
    input: &'a Input,
    rock: FxHashSet<(u64, u8)>,
    height: u64,
    jet: usize,
    shape: usize,
}

impl<'a> Simulation<'a> {
    fn new(input: &'a Input) -> Self {
        Self {
            input,
            rock: FxHashSet::with_capacity_and_hasher(16384, Default::default()),
            height: 0,
            jet: 0,
            shape: 0,
        }
    }

    fn next_jet(&mut self) -> Direction {
        let out = self.input[self.jet];
        self.jet = (self.jet + 1) % self.input.len();
        out
    }

    fn next_shape(&mut self) -> &'static Shape<'static> {
        let out = &SHAPES[self.shape];
        self.shape = (self.shape + 1) % SHAPES.len();
        out
    }

    fn test(&self, shape: &Shape, x: u8, y: u64) -> bool {
        y + 1 >= shape.height
            && x + shape.width <= WIDTH
            && shape
                .rock
                .iter()
                .all(|&(i, j)| !self.rock.contains(&(y - i, x + j)))
    }

    fn add(&mut self, shape: &Shape, x: u8, y: u64) {
        for &(i, j) in shape.rock {
            self.rock.insert((y - i, x + j));
        }
    }
}

struct Step {
    height: u64,
    jet: usize,
    shape: usize,
}

impl PartialEq for Step {
    fn eq(&self, other: &Self) -> bool {
        (self.jet, self.shape) == (other.jet, other.shape)
    }
}

impl Iterator for Simulation<'_> {
    type Item = Step;

    fn next(&mut self) -> Option<Self::Item> {
        let out = Some(Step {
            height: self.height,
            jet: self.jet,
            shape: self.shape,
        });

        let shape = self.next_shape();
        let mut x = START_X;
        let mut y = self.height + START_Y + shape.height - 1;
        loop {
            if let Some(dx) = match self.next_jet() {
                Direction::Left => x.checked_sub(1),
                Direction::Right => Some(x + 1),
            }
            .and_then(|dx| self.test(shape, dx, y).then_some(dx))
            {
                x = dx;
            }

            if y > 0 && self.test(shape, x, y - 1) {
                y -= 1;
            } else {
                self.add(shape, x, y);
                self.height = self.height.max(y + 1);
                break;
            }
        }
        out
    }
}

fn part1(input: &Input) -> u64 {
    Simulation::new(input).nth(2022).unwrap().height
}

fn part2(input: &Input) -> u64 {
    let mut steps = Vec::with_capacity(4096);
    let mut idx = FxHashMap::with_capacity_and_hasher(4096, Default::default());
    Simulation::new(input)
        .enumerate()
        .find_map(
            |(
                round,
                step @ Step {
                    mut height,
                    jet,
                    shape,
                },
            )| {
                steps.push(step);
                if let Some(i) = idx.get(&(jet, shape)) {
                    let n = steps.len();
                    let l = n - i - 1;
                    if n >= l * 2 && steps[n - l..] == steps[n - 2 * l..n - l] {
                        let left = 1000000000000 - round;
                        height +=
                            (left / l) as u64 * (steps[n - 1].height - steps[n - l - 1].height);
                        height += steps[n - l - 1 + left % l].height - steps[n - l - 1].height;
                        return Some(height);
                    }
                }
                idx.insert((jet, shape), round);
                None
            },
        )
        .unwrap()
}

aoc::main!(2022, 17, ex: 1);
