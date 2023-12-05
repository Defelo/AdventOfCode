#![feature(test)]

use std::ops::Range;

use aoc::range::{RangeExt, RangeRel};
use itertools::Itertools;

type Map = Vec<RangeMap>;

#[derive(Debug)]
struct Input {
    seeds: Vec<i64>,
    maps: Vec<Map>,
}

#[derive(Debug)]
struct RangeMap {
    source: Range<i64>,
    offset: i64,
}

fn setup(input: &str) -> Input {
    let mut blocks = input.split("\n\n");
    let seeds = blocks
        .next()
        .unwrap()
        .split_whitespace()
        .skip(1)
        .map(|x| x.parse().unwrap())
        .collect();
    let maps = blocks
        .map(|block| {
            block
                .lines()
                .skip(1)
                .map(|line| {
                    let mut nums = line.split_whitespace().map(|n| n.parse().unwrap());
                    let dest_start = nums.next().unwrap();
                    let source_start = nums.next().unwrap();
                    let len = nums.next().unwrap();
                    RangeMap {
                        source: source_start..source_start + len,
                        offset: dest_start - source_start,
                    }
                })
                .collect()
        })
        .collect();

    Input { seeds, maps }
}

fn seed_to_location(seed: i64, maps: &[Map]) -> i64 {
    maps.iter().fold(seed, |seed, map| {
        map.iter()
            .find(|range_map| range_map.source.contains(&seed))
            .map(|range_map| range_map.offset)
            .unwrap_or(0)
            + seed
    })
}

fn seed_range_to_location_range(
    seeds: Range<i64>,
    maps: &[Map],
) -> impl IntoIterator<Item = Range<i64>> {
    maps.iter().fold(vec![seeds], |mut seeds, map| {
        let mut mapped = Vec::new();
        for range_map in map {
            let mut new_seeds = Vec::new();
            for seed_range in seeds.iter().cloned() {
                match seed_range.rel(&range_map.source) {
                    RangeRel::LeftOf | RangeRel::RightOf => new_seeds.push(seed_range),
                    RangeRel::ContainedIn => mapped.push(seed_range.add(range_map.offset)),
                    RangeRel::Contains => {
                        new_seeds.push(seed_range.start..range_map.source.start);
                        mapped.push(range_map.source.add(range_map.offset));
                        new_seeds.push(range_map.source.end..seed_range.end);
                    }
                    RangeRel::ContainsStartOf => {
                        new_seeds.push(seed_range.start..range_map.source.start);
                        mapped.push((range_map.source.start..seed_range.end).add(range_map.offset));
                    }
                    RangeRel::ContainsEndOf => {
                        mapped.push((seed_range.start..range_map.source.end).add(range_map.offset));
                        new_seeds.push(range_map.source.end..seed_range.end);
                    }
                }
            }
            seeds = new_seeds;
        }
        mapped.extend(seeds);
        mapped
    })
}

fn part1(input: &Input) -> i64 {
    input
        .seeds
        .iter()
        .map(|&seed| seed_to_location(seed, &input.maps))
        .min()
        .unwrap()
}

fn part2(input: &Input) -> i64 {
    input
        .seeds
        .iter()
        .tuples()
        .flat_map(|(&start, &len)| seed_range_to_location_range(start..start + len, &input.maps))
        .map(|range| range.start)
        .min()
        .unwrap()
}

aoc::main!(2023, 5, ex: 1);
