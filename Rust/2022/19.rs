#![feature(test)]

use std::ops::Add;

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

impl Resources {
    fn checked_sub(self, rhs: Self) -> Option<Self> {
        match (
            self.ore.checked_sub(rhs.ore),
            self.clay.checked_sub(rhs.clay),
            self.obsidian.checked_sub(rhs.obsidian),
            self.geode.checked_sub(rhs.geode),
        ) {
            (Some(ore), Some(clay), Some(obsidian), Some(geode)) => Some(Resources {
                ore,
                clay,
                obsidian,
                geode,
            }),
            _ => None,
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
            Blueprint {
                ore_cost: Resources {
                    ore: nums.next().unwrap(),
                    clay: 0,
                    obsidian: 0,
                    geode: 0,
                },
                clay_cost: Resources {
                    ore: nums.next().unwrap(),
                    clay: 0,
                    obsidian: 0,
                    geode: 0,
                },
                obsidian_cost: Resources {
                    ore: nums.next().unwrap(),
                    clay: nums.next().unwrap(),
                    obsidian: 0,
                    geode: 0,
                },
                geode_cost: Resources {
                    ore: nums.next().unwrap(),
                    clay: 0,
                    obsidian: nums.next().unwrap(),
                    geode: 0,
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
    could_have_built_ore: bool,
    could_have_built_clay: bool,
    could_have_built_obsidian: bool,
    could_have_built_geode: bool,
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
            could_have_built_ore: false,
            could_have_built_clay: false,
            could_have_built_obsidian: false,
            could_have_built_geode: false,
        }
    }

    fn next(self, blueprint: &Blueprint) -> impl Iterator<Item = State> {
        let build = |could_have_built: bool, cost, add_bots| {
            (!could_have_built)
                .then(|| {
                    self.time.checked_sub(1).and_then(|time| {
                        self.resources.checked_sub(cost).map(|resources| State {
                            time,
                            resources: resources + self.robots,
                            robots: self.robots + add_bots,
                            could_have_built_ore: false,
                            could_have_built_clay: false,
                            could_have_built_obsidian: false,
                            could_have_built_geode: false,
                        })
                    })
                })
                .flatten()
        };

        let build_ore_bot = build(
            self.could_have_built_ore,
            blueprint.ore_cost,
            Resources {
                ore: 1,
                ..Default::default()
            },
        );
        let build_clay_bot = build(
            self.could_have_built_clay,
            blueprint.clay_cost,
            Resources {
                clay: 1,
                ..Default::default()
            },
        );
        let build_obsidian_bot = build(
            self.could_have_built_obsidian,
            blueprint.obsidian_cost,
            Resources {
                obsidian: 1,
                ..Default::default()
            },
        );
        let build_geode_bot = build(
            self.could_have_built_geode,
            blueprint.geode_cost,
            Resources {
                geode: 1,
                ..Default::default()
            },
        );

        let noop = self.time.checked_sub(1).map(|time| State {
            time,
            resources: self.resources + self.robots,
            robots: self.robots,
            could_have_built_ore: build_ore_bot.is_some(),
            could_have_built_clay: build_clay_bot.is_some(),
            could_have_built_obsidian: build_obsidian_bot.is_some(),
            could_have_built_geode: build_geode_bot.is_some(),
        });

        [
            noop,
            build_ore_bot,
            build_clay_bot,
            build_obsidian_bot,
            build_geode_bot,
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

    let mut queue = Vec::from([State::init(max_time)]);
    let mut seen = FxHashSet::default();
    let mut out = 0;
    let mut best_score = 0;
    while let Some(state) = queue.pop() {
        best_score = best_score.max(state.resources.geode + state.robots.geode * state.time);
        out = out.max(state.resources.geode);
        if state.time == 0 {
            continue;
        }

        for next in state.next(blueprint) {
            if next.robots.ore > max_costs.ore
                || next.robots.clay > max_costs.clay
                || next.robots.obsidian > max_costs.obsidian
            {
                continue;
            }

            let t = next.time;
            if next.resources.geode + t * next.robots.geode + t * (t + 1) / 2 < best_score {
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
