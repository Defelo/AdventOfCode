#![feature(test)]

use std::ops::Add;

type Input = Vec<Position>;

fn setup(input: &str) -> Input {
    input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars().enumerate().filter_map(move |(j, c)| {
                (c == '#').then_some(Position::from_coords(j as _, i as _))
            })
        })
        .collect()
}

const SIZE: u16 = 256;
const OFFSET: u16 = 16;

type PositionType = u16;
type BucketType = usize;
const BUCKET_COUNT: usize = (1 << PositionType::BITS as usize) / BUCKET_SIZE;
const BUCKET_SIZE: usize = BucketType::BITS as usize;

const DIRECTIONS: [Direction; 4] = [
    Direction::North,
    Direction::South,
    Direction::West,
    Direction::East,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Position(PositionType);

#[derive(Debug, Clone)]
struct PositionSet([BucketType; BUCKET_COUNT]);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Add<Direction> for Position {
    type Output = Self;

    fn add(self, rhs: Direction) -> Self::Output {
        match rhs {
            Direction::North => Self(self.0 - SIZE),
            Direction::East => Self(self.0 + 1),
            Direction::South => Self(self.0 + SIZE),
            Direction::West => Self(self.0 - 1),
        }
    }
}

impl Direction {
    fn rotate_left(self) -> Self {
        match self {
            Self::North => Self::West,
            Self::East => Self::North,
            Self::South => Self::East,
            Self::West => Self::South,
        }
    }

    fn rotate_right(self) -> Self {
        match self {
            Self::North => Self::East,
            Self::East => Self::South,
            Self::South => Self::West,
            Self::West => Self::North,
        }
    }
}

impl Position {
    fn from_coords(x: PositionType, y: PositionType) -> Self {
        Self((y + OFFSET) * SIZE + x + OFFSET)
    }

    fn coords(self) -> (PositionType, PositionType) {
        (self.0 % SIZE, self.0 / SIZE)
    }

    fn neighbors(self) -> [Position; 8] {
        [
            Self(self.0 - SIZE - 1),
            Self(self.0 - SIZE),
            Self(self.0 - SIZE + 1),
            Self(self.0 - 1),
            Self(self.0 + 1),
            Self(self.0 + SIZE - 1),
            Self(self.0 + SIZE),
            Self(self.0 + SIZE + 1),
        ]
    }
}

impl FromIterator<Position> for PositionSet {
    fn from_iter<T: IntoIterator<Item = Position>>(iter: T) -> Self {
        let mut out = Self::default();
        for p in iter {
            out.insert(p);
        }
        out
    }
}

impl Default for PositionSet {
    fn default() -> Self {
        Self([0; BUCKET_COUNT])
    }
}

impl PositionSet {
    fn contains(&self, p: Position) -> bool {
        self.0[p.0 as usize / BUCKET_SIZE] & (1 << (p.0 % BUCKET_SIZE as u16)) != 0
    }

    fn remove(&mut self, p: Position) {
        self.0[p.0 as usize / BUCKET_SIZE] &= !(1 << (p.0 % BUCKET_SIZE as u16));
    }

    fn insert(&mut self, p: Position) {
        self.0[p.0 as usize / BUCKET_SIZE] |= 1 << (p.0 % BUCKET_SIZE as u16);
    }

    fn toggle(&mut self, p: Position) {
        self.0[p.0 as usize / BUCKET_SIZE] ^= 1 << (p.0 % BUCKET_SIZE as u16);
    }
}

#[derive(Debug)]
struct State {
    positions: Vec<Position>,
    proposed: Vec<Position>,
    occupied: PositionSet,
    cnt: usize,
}

impl State {
    fn from_input(input: &Input) -> Self {
        let positions = input.clone();
        let occupied = input.iter().copied().collect();
        Self {
            proposed: positions.clone(),
            positions,
            occupied,
            cnt: 0,
        }
    }

    fn size(&self) -> (usize, usize) {
        let (x, y) = self.positions[0].coords();
        let (x1, y1, x2, y2) = self
            .positions
            .iter()
            .fold((x, y, x, y), |(x1, y1, x2, y2), p| {
                let (x, y) = p.coords();
                (x1.min(x), y1.min(y), x2.max(x), y2.max(y))
            });
        ((x2 - x1 + 1) as _, (y2 - y1 + 1) as _)
    }

    fn empty_tiles(&self) -> usize {
        let (w, h) = self.size();
        w * h - self.positions.len()
    }

    fn simulate(&mut self) -> bool {
        let mut proposed_set = PositionSet::default();
        self.positions
            .iter()
            .enumerate()
            .filter_map(|(i, &p)| {
                if !p.neighbors().into_iter().any(|q| self.occupied.contains(q)) {
                    return None;
                }

                let d = (0..4)
                    .map(|di| DIRECTIONS[(di + self.cnt) % 4])
                    .find(|&d| {
                        !self.occupied.contains(p + d)
                            && !self.occupied.contains(p + d + d.rotate_left())
                            && !self.occupied.contains(p + d + d.rotate_right())
                    })?;
                Some((i, p + d))
            })
            .for_each(|(i, q)| {
                proposed_set.toggle(q);
                self.proposed[i] = q;
            });

        let mut moved = false;
        self.positions
            .iter_mut()
            .zip(self.proposed.iter_mut())
            .for_each(|(p, q)| {
                if !proposed_set.contains(*q) {
                    *q = *p;
                } else {
                    self.occupied.remove(*p);
                    self.occupied.insert(*q);
                    *p = *q;
                    moved = true;
                }
            });

        self.cnt += 1;
        moved
    }
}

fn part1(input: &Input) -> usize {
    let mut state = State::from_input(input);
    for _ in 0..10 {
        state.simulate();
    }
    state.empty_tiles()
}

fn part2(input: &Input) -> usize {
    let mut out = 1;
    let mut state = State::from_input(input);
    while state.simulate() {
        out += 1;
    }
    out
}

aoc::main!(2022, 23, ex: 1);
