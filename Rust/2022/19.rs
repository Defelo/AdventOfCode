#![feature(test)]

use std::{
    cmp::Ordering,
    collections::BinaryHeap,
    ops::{Add, Mul},
};

use rayon::prelude::*;
use regex::Regex;
use rustc_hash::FxHashSet;

type Input = Vec<Blueprint>;

#[derive(Debug)]
struct Blueprint {
    ore_cost: Resources,
    clay_cost: Resources,
    obsidian_cost: Resources,
    geode_cost: Resources,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
struct Resources {
    ore: u32,
    clay: u32,
    obsidian: u32,
    geode: u32,
}

impl Add for Resources {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            ore: self.ore + rhs.ore,
            clay: self.clay + rhs.clay,
            obsidian: self.obsidian + rhs.obsidian,
            geode: self.geode + rhs.geode,
        }
    }
}

impl Mul<u32> for Resources {
    type Output = Self;

    fn mul(self, rhs: u32) -> Self::Output {
        Self {
            ore: self.ore * rhs,
            clay: self.clay * rhs,
            obsidian: self.obsidian * rhs,
            geode: self.geode * rhs,
        }
    }
}

impl Resources {
    fn checked_sub(self, rhs: Self) -> Option<Self> {
        Some(Self {
            ore: self.ore.checked_sub(rhs.ore)?,
            clay: self.clay.checked_sub(rhs.clay)?,
            obsidian: self.obsidian.checked_sub(rhs.obsidian)?,
            geode: self.geode.checked_sub(rhs.geode)?,
        })
    }

    fn saturating_sub(self, rhs: Self) -> Self {
        Self {
            ore: self.ore.saturating_sub(rhs.ore),
            clay: self.clay.saturating_sub(rhs.clay),
            obsidian: self.obsidian.saturating_sub(rhs.obsidian),
            geode: self.geode.saturating_sub(rhs.geode),
        }
    }

    fn max(self, rhs: Self) -> Self {
        Self {
            ore: self.ore.max(rhs.ore),
            clay: self.clay.max(rhs.clay),
            obsidian: self.obsidian.max(rhs.obsidian),
            geode: self.geode.max(rhs.geode),
        }
    }
}

fn setup(input: &str) -> Input {
    let num_regex = Regex::new("\\d+").unwrap();
    input
        .lines()
        .map(|line| {
            let mut nums = num_regex
                .find_iter(line)
                .skip(1)
                .map(|num| num.as_str().parse().unwrap());
            let mut next = || nums.next().unwrap();
            Blueprint {
                ore_cost: Resources {
                    ore: next(),
                    ..Default::default()
                },
                clay_cost: Resources {
                    ore: next(),
                    ..Default::default()
                },
                obsidian_cost: Resources {
                    ore: next(),
                    clay: next(),
                    ..Default::default()
                },
                geode_cost: Resources {
                    ore: next(),
                    obsidian: next(),
                    ..Default::default()
                },
            }
        })
        .collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    time: u32,
    resources: Resources,
    robots: Resources,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        self.resources.geode.cmp(&other.resources.geode)
    }
}

impl State {
    fn init(time: u32) -> Self {
        Self {
            time,
            resources: Resources::default(),
            robots: Resources {
                ore: 1,
                ..Default::default()
            },
        }
    }

    fn next_noop(self) -> Self {
        Self {
            time: 0,
            resources: self.resources + self.robots * self.time,
            robots: self.robots,
        }
    }

    fn next_build_bot(self, cost: Resources, robots: Resources) -> Option<Self> {
        let missing = cost.saturating_sub(self.resources);

        if missing.clay > 0 && self.robots.clay == 0
            || missing.obsidian > 0 && self.robots.obsidian == 0
            || missing.geode > 0 && self.robots.geode == 0
        {
            return None;
        }

        let t = 1 + [
            (missing.ore > 0).then(|| missing.ore.div_ceil(self.robots.ore)),
            (missing.clay > 0).then(|| missing.clay.div_ceil(self.robots.clay)),
            (missing.obsidian > 0).then(|| missing.obsidian.div_ceil(self.robots.obsidian)),
            (missing.geode > 0).then(|| missing.geode.div_ceil(self.robots.geode)),
        ]
        .into_iter()
        .flatten()
        .max()
        .unwrap_or(0);

        Some(Self {
            time: self.time.checked_sub(t)?,
            resources: (self.resources + self.robots * t).checked_sub(cost)?,
            robots: self.robots + robots,
        })
    }

    fn next(self, blueprint: &Blueprint) -> impl Iterator<Item = State> {
        [
            Some(self.next_noop()),
            self.next_build_bot(
                blueprint.ore_cost,
                Resources {
                    ore: 1,
                    ..Default::default()
                },
            ),
            self.next_build_bot(
                blueprint.clay_cost,
                Resources {
                    clay: 1,
                    ..Default::default()
                },
            ),
            self.next_build_bot(
                blueprint.obsidian_cost,
                Resources {
                    obsidian: 1,
                    ..Default::default()
                },
            ),
            self.next_build_bot(
                blueprint.geode_cost,
                Resources {
                    geode: 1,
                    ..Default::default()
                },
            ),
        ]
        .into_iter()
        .flatten()
    }
}

fn solve(blueprint: &Blueprint, max_time: u32) -> u32 {
    let max_costs = blueprint
        .ore_cost
        .max(blueprint.clay_cost)
        .max(blueprint.obsidian_cost)
        .max(blueprint.geode_cost);

    let mut queue = BinaryHeap::from([State::init(max_time)]);
    let mut seen = FxHashSet::default();
    let mut out = 0;
    while let Some(state) = queue.pop() {
        out = out.max(state.resources.geode + state.robots.geode * state.time);

        for next in state.next(blueprint) {
            if next.robots.ore > max_costs.ore
                || next.robots.clay > max_costs.clay
                || next.robots.obsidian > max_costs.obsidian
            {
                continue;
            }

            let t = next.time;
            if next.resources.geode + t * next.robots.geode + t * t.saturating_sub(1) / 2 < out {
                continue;
            }

            if state.robots.obsidian == max_costs.obsidian && next.robots.clay > state.robots.clay {
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
    input
        .par_iter()
        .enumerate()
        .map(|(i, blueprint)| solve(blueprint, 24) * (i as u32 + 1))
        .sum()
}

fn part2(input: &Input) -> u32 {
    input
        .par_iter()
        .take(3)
        .map(|blueprint| solve(blueprint, 32))
        .product()
}

aoc::main!(2022, 19, ex: 1);
