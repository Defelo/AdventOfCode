#![feature(test)]

use std::ops::RangeInclusive;

use itertools::Itertools;
use regex::Regex;
use rustc_hash::FxHashSet;

#[derive(Debug)]
struct Sensor {
    sensor_x: i64,
    sensor_y: i64,
    beacon_x: i64,
    beacon_y: i64,
}

#[derive(Debug)]
struct Input {
    sensors: Vec<Sensor>,
    part1_y: i64,
}

fn setup(input: &str) -> Input {
    let regex = Regex::new(r"-?\d+").unwrap();
    let sensors = input
        .trim()
        .lines()
        .filter(|line| !line.starts_with('#'))
        .map(|line| {
            let mut it = regex.find_iter(line);
            let mut next = || it.next().unwrap().as_str().parse().unwrap();
            Sensor {
                sensor_x: next(),
                sensor_y: next(),
                beacon_x: next(),
                beacon_y: next(),
            }
        })
        .collect();
    let fst = input.lines().next().unwrap();
    let part1_y = if fst.starts_with('#') {
        regex.find(fst).unwrap().as_str().parse().unwrap()
    } else {
        2000000
    };
    Input { sensors, part1_y }
}

fn merge_ranges(ranges: &mut Vec<RangeInclusive<i64>>) {
    ranges.sort_unstable_by(|a, b| a.start().cmp(b.start()));
    let mut out = Vec::with_capacity(ranges.len());
    let mut it = ranges.iter();
    let Some(mut current) = it.next().cloned() else {
        return;
    };
    for range in it {
        if *range.start() > current.end() + 1 {
            out.push(current);
            current = range.clone();
        } else if range.end() > current.end() {
            current = *current.start()..=*range.end();
        }
    }
    out.push(current);
    *ranges = out;
}

fn part1(input: &Input) -> i64 {
    let mut ranges = Vec::with_capacity(input.sensors.len());
    let mut beacons = FxHashSet::default();

    for &Sensor {
        sensor_x,
        sensor_y,
        beacon_x,
        beacon_y,
    } in &input.sensors
    {
        let d = (sensor_x - beacon_x).abs() + (sensor_y - beacon_y).abs();
        let a = sensor_x - (d - (sensor_y - input.part1_y).abs());
        let b = sensor_x + (d - (sensor_y - input.part1_y).abs());
        if a <= b {
            ranges.push(a..=b);
        }
        if beacon_y == input.part1_y {
            beacons.insert(beacon_x);
        }
    }

    merge_ranges(&mut ranges);
    ranges
        .iter()
        .map(|range| {
            range.end() - range.start() + 1
                - beacons.iter().filter(|b| range.contains(b)).count() as i64
        })
        .sum()
}

fn solve_dist_eqs(
    (x1, y1, d1): (i64, i64, i64),
    (x2, y2, d2): (i64, i64, i64),
    (x3, y3, d3): (i64, i64, i64),
) -> Option<(i64, i64)> {
    [1, -1]
        .iter()
        .cartesian_product([1, -1].iter().cartesian_product([1, -1].iter()))
        .map(|(&p, (&q, &r))| {
            let x = (d2 - q * r * d1 + r * (y1 - y2) + p * q * r * (x1 + x2)) / (2 * p * q * r);
            let y = y1 - q * (d1 - p * (x1 - x));
            (x, y)
        })
        .find(|(x, y)| {
            (x1 - x).abs() + (y1 - y).abs() == d1
                && (x2 - x).abs() + (y2 - y).abs() == d2
                && (x3 - x).abs() + (y3 - y).abs() == d3
        })
}

fn part2(input: &Input) -> i64 {
    let sensors = &input
        .sensors
        .iter()
        .map(
            |&Sensor {
                 sensor_x,
                 sensor_y,
                 beacon_x,
                 beacon_y,
             }| {
                let d = (sensor_x - beacon_x).abs() + (sensor_y - beacon_y).abs();
                (sensor_x, sensor_y, d + 1)
            },
        )
        .collect_vec();

    let (x, y) = sensors
        .iter()
        .enumerate()
        .flat_map(|(i, a)| {
            sensors[i + 1..]
                .iter()
                .enumerate()
                .flat_map(move |(j, b)| sensors[i + j + 2..].iter().map(move |c| (*a, *b, *c)))
        })
        .map(|(a, b, c)| {
            if (a.0 - b.0).abs() + (a.1 - b.1).abs() == a.2 + b.2 {
                (a, c, b)
            } else {
                (a, b, c)
            }
        })
        .filter_map(|(a, b, c)| solve_dist_eqs(a, b, c))
        .find(|&(x, y)| {
            sensors
                .iter()
                .all(|&(a, b, d)| (x - a).abs() + (y - b).abs() > d - 1)
        })
        .unwrap();

    x * 4000000 + y
}

aoc::main!(2022, 15, ex: 1);
