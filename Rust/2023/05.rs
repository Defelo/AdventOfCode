#![feature(test)]

use aoc::range::RangeExt;
use itertools::Itertools;

type Range = std::ops::Range<i64>;
type Map = Vec<RangeMap>;

#[derive(Debug)]
struct Input {
    seeds: Vec<i64>,
    maps: Vec<Map>,
}

#[derive(Debug)]
struct RangeMap {
    source: Range,
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

#[derive(Debug, Clone)]
enum MaybeMappedRange {
    Mapped(Range),
    Unchanged(Range),
}

impl MaybeMappedRange {
    fn unwrap(self) -> Range {
        match self {
            Self::Mapped(range) => range,
            Self::Unchanged(range) => range,
        }
    }
}

fn map_seed_ranges(seeds: Vec<MaybeMappedRange>, range_map: &RangeMap) -> Vec<MaybeMappedRange> {
    seeds
        .iter()
        .cloned()
        .flat_map(|seed_range| {
            let MaybeMappedRange::Unchanged(seed_range) = seed_range else {
                return [Some(seed_range), None, None].into_iter().flatten();
            };

            let (left, after_start) = seed_range.split_at(range_map.source.start);
            let (before_end, right) = seed_range.split_at(range_map.source.end);
            let mid = (|| after_start?.intersect(&before_end?))();
            [
                left.map(MaybeMappedRange::Unchanged),
                mid.map(|mid| MaybeMappedRange::Mapped(mid.add(range_map.offset))),
                right.map(MaybeMappedRange::Unchanged),
            ]
            .into_iter()
            .flatten()
        })
        .collect()
}

fn seed_ranges_to_location_ranges(seeds: Vec<Range>, maps: &[Map]) -> Vec<Range> {
    maps.iter().fold(seeds, |seeds, map| {
        let seeds = seeds.into_iter().map(MaybeMappedRange::Unchanged).collect();
        map.iter()
            .fold(seeds, map_seed_ranges)
            .into_iter()
            .map(MaybeMappedRange::unwrap)
            .collect()
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
    let seeds = input
        .seeds
        .iter()
        .tuples()
        .map(|(&start, &len)| start..start + len)
        .collect();
    seed_ranges_to_location_ranges(seeds, &input.maps)
        .into_iter()
        .map(|range| range.start)
        .min()
        .unwrap()
}

aoc::main!(2023, 5, ex: 1);
