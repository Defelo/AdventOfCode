#![feature(test)]

use std::{cmp::Ordering, collections::BinaryHeap};

use itertools::Itertools;
use rustc_hash::FxHashSet;
use smallvec::{smallvec, SmallVec};

#[derive(Debug)]
struct Input {
    width: usize,
    height: usize,
    blizzards: Vec<Vec<Option<Direction>>>,
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

fn setup(input: &str) -> Input {
    let blizzards = input
        .lines()
        .skip(1)
        .take_while(|l| l.as_bytes()[1] != b'#')
        .map(|line| {
            line.bytes()
                .skip(1)
                .take_while(|&c| c != b'#')
                .map(|c| match c {
                    b'.' => None,
                    b'^' => Some(Direction::North),
                    b'>' => Some(Direction::East),
                    b'v' => Some(Direction::South),
                    b'<' => Some(Direction::West),
                    _ => panic!(),
                })
                .collect_vec()
        })
        .collect_vec();
    Input {
        width: blizzards[0].len(),
        height: blizzards.len(),
        blizzards,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    time: usize,
    position: usize,
    cnt: usize,
    dist: usize,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.dist.cmp(&self.dist)
    }
}

impl State {
    fn init(input: &Input) -> Self {
        Self {
            time: 0,
            position: input.width * input.height,
            cnt: 0,
            dist: usize::MAX,
        }
    }

    fn next(self, input: &Input, target: usize) -> impl Iterator<Item = Self> + '_ {
        let &Input {
            width: w,
            height: h,
            ..
        } = input;

        let mut out: SmallVec<[_; 7]> = smallvec![self.position];

        match self.position {
            p if p == w * h => out.push(0),
            p if p == w * h + 1 => out.push(w * h - 1),
            p => {
                if p == 0 {
                    out.push(w * h);
                }

                if p == w * h - 1 {
                    out.push(w * h + 1);
                }

                if p >= w {
                    out.push(p - w);
                }
                if p + w < w * h {
                    out.push(p + w);
                }
                if p % w > 0 {
                    out.push(p - 1);
                }
                if p % w < w - 1 {
                    out.push(p + 1);
                }
            }
        }

        out.into_iter()
            .filter(move |&p| {
                let (i, j) = (p / w, p % w);
                let t = self.time + 1;
                p >= w * h
                    || !matches!(
                        input.blizzards[i][(j + w - t % w) % w],
                        Some(Direction::East)
                    ) && !matches!(input.blizzards[i][(j + t) % w], Some(Direction::West))
                        && !matches!(
                            input.blizzards[(i + h - t % h) % h][j],
                            Some(Direction::South)
                        )
                        && !matches!(input.blizzards[(i + t) % h][j], Some(Direction::North))
            })
            .map(move |p| {
                let time = self.time + 1;
                let cnt = self.cnt
                    + (self.cnt % 2) * (p == w * h) as usize
                    + ((self.cnt + 1) % 2) * (p == w * h + 1) as usize;

                let start_to_goal = w + h;
                let (start, goal) = match p {
                    p if p == w * h => (0, start_to_goal),
                    p if p == w * h + 1 => (start_to_goal, 0),
                    p => {
                        let (i, j) = (p / w, p % w);
                        (i + j + 1, start_to_goal - i - j - 2)
                    }
                };

                Self {
                    time,
                    cnt,
                    position: p,
                    dist: {
                        if cnt != target {
                            time + start * (cnt % 2)
                                + goal * ((cnt + 1) % 2)
                                + (target - cnt - 1) * start_to_goal
                        } else {
                            0
                        }
                    },
                }
            })
    }
}

fn solve(input: &Input, target: usize) -> usize {
    let mut queue = BinaryHeap::from([State::init(input)]);
    let mut seen = FxHashSet::default();
    while let Some(state) = queue.pop() {
        if state.cnt == target {
            return state.time;
        }

        for next in state.next(input, target) {
            if seen.insert(next) {
                queue.push(next);
            }
        }
    }

    panic!()
}

fn part1(input: &Input) -> usize {
    solve(input, 1)
}

fn part2(input: &Input) -> usize {
    solve(input, 3)
}

aoc::main!(2022, 24, ex: 1);
