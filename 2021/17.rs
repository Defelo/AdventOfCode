#![feature(test)]

extern crate test;

use std::collections::{HashMap, HashSet};
use std::fs;
use test::Bencher;
use regex::Regex;

struct Input {
    x1: i32,
    x2: i32,
    y1: i32,
    y2: i32,
}

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/17.txt").unwrap();
    let regex = Regex::new(r"^target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)$").unwrap();
    let capture = regex.captures(puzzle.trim()).unwrap();
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
    let mut ok = HashMap::new();
    for ovy in input.y1..500 {
        let mut vy = ovy;
        let mut t = 0;
        let mut y = 0;
        let mut maxy = 0;

        while y >= input.y1 {
            if y <= input.y2 {
                if !ok.contains_key(&t) {ok.insert(t, (0, HashSet::new()));}
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
        let mut found = HashSet::new();
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

fn main() {
    let (part1, part2) = run();
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}

pub fn run() -> (String, String) {
    let input = get_input();
    let (part1, part2) = solve(&input);
    (part1.to_string(), part2.to_string())
}

#[bench]
fn bench(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| solve(&input))
}
