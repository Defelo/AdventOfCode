#![feature(test)]

use regex::Regex;
use rustc_hash::{FxHashMap, FxHashSet};

struct Input {
    x1: i32,
    x2: i32,
    y1: i32,
    y2: i32,
}

fn setup(input: &str) -> Input {
    let regex = Regex::new(r"^target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)$").unwrap();
    let capture = regex.captures(input.trim()).unwrap();
    let get = |i| capture.get(i).unwrap().as_str().parse().unwrap();
    Input {
        x1: get(1),
        x2: get(2),
        y1: get(3),
        y2: get(4),
    }
}

fn solve(input: &Input) -> (i32, usize) {
    let mut maxt = 0;
    let mut ok = FxHashMap::default();
    for ovy in input.y1..500 {
        let mut vy = ovy;
        let mut t = 0;
        let mut y = 0;
        let mut maxy = 0;

        while y >= input.y1 {
            if y <= input.y2 {
                ok.entry(t).or_insert_with(|| (0, FxHashSet::default()));
                let (my, vys) = ok.get_mut(&t).unwrap();
                *my = maxy.max(*my);
                vys.insert(ovy);
                maxt = maxt.max(t);
            }
            y += vy;
            maxy = maxy.max(y);
            vy -= 1;
            t += 1;
        }
    }

    let mut outa = 0;
    let mut outb = 0;

    for mut vx in 1..=input.x2 {
        let mut t = 0;
        let mut x = 0;
        let mut found = FxHashSet::default();
        while x <= input.x2 && t <= maxt {
            if input.x1 <= x {
                if let Some((a, b)) = ok.get(&t) {
                    outa = outa.max(*a);
                    for k in b {
                        found.insert(*k);
                    }
                }
            }
            x += vx;
            vx = 0.max(vx - 1);
            t += 1;
        }
        outb += found.len();
    }

    (outa, outb)
}

fn part1(input: &Input) -> i32 {
    solve(input).0
}

fn part2(input: &Input) -> usize {
    solve(input).1
}

aoc::main!(2021, 17, ex: 1);
