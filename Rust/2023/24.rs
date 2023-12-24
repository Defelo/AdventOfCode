#![feature(test)]

use std::ops::{Add, Mul};

use itertools::{Itertools, PeekingNext};
use num::rational::Ratio;
use rayon::prelude::*;
use z3::{
    ast::{Ast, Int, Real},
    Config, Context, Solver,
};

#[derive(Debug)]
struct Input {
    hailstones: Vec<Hailstone>,
    test_start: i64,
    test_end: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Hailstone {
    position: Vec3,
    velocity: Vec3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Vec3 {
    x: i64,
    y: i64,
    z: i64,
}

fn setup(input: &str) -> Input {
    let mut lines = input.lines().peekable();

    let (test_start, test_end) = lines
        .peeking_next(|line| line.starts_with('#'))
        .map(|line| {
            line.split(' ')
                .skip(1)
                .map(|x| x.parse().unwrap())
                .collect_tuple()
                .unwrap()
        })
        .unwrap_or((200000000000000, 400000000000000));

    let hailstones = lines
        .map(|line| {
            let (position, velocity) = line
                .split('@')
                .map(|split| {
                    let (x, y, z) = split
                        .split(',')
                        .map(|x| x.trim().parse().unwrap())
                        .collect_tuple()
                        .unwrap();
                    Vec3 { x, y, z }
                })
                .collect_tuple()
                .unwrap();
            Hailstone { position, velocity }
        })
        .collect();

    Input {
        hailstones,
        test_start,
        test_end,
    }
}

fn part1(input: &Input) -> usize {
    let test_start = Ratio::from(input.test_start as i128);
    let test_end = Ratio::from(input.test_end as i128);
    input
        .hailstones
        .iter()
        .copied()
        .enumerate()
        .flat_map(|(i, a)| {
            input
                .hailstones
                .iter()
                .copied()
                .take(i)
                .map(move |b| (a, b))
        })
        .filter(|&(a, b)| a.velocity.x * b.velocity.y != a.velocity.y * b.velocity.x)
        .par_bridge()
        .filter(|&(a, b)| {
            let x1 = Ratio::from(a.position.y as i128);
            let y1 = Ratio::from(a.position.x as i128);
            let x2 = Ratio::from(b.position.y as i128);
            let y2 = Ratio::from(b.position.x as i128);
            let vx1 = Ratio::from(a.velocity.y as i128);
            let vy1 = Ratio::from(a.velocity.x as i128);
            let vx2 = Ratio::from(b.velocity.y as i128);
            let vy2 = Ratio::from(b.velocity.x as i128);

            let t1 = ((y1 - y2) * vx2 - (x1 - x2) * vy2) / (vx1 * vy2 - vy1 * vx2);
            let t2 = ((y2 - y1) * vx1 - (x2 - x1) * vy1) / (vx2 * vy1 - vy2 * vx1);
            let x = x1 + t1 * vx1;
            let y = y1 + t1 * vy1;
            debug_assert_eq!(x, x2 + t2 * vx2);
            debug_assert_eq!(y, y2 + t2 * vy2);

            t1 >= 0.into()
                && t2 >= 0.into()
                && x >= test_start
                && x <= test_end
                && y >= test_start
                && y <= test_end
        })
        .count()
}

fn part2(input: &Input) -> i64 {
    let config = Config::new();
    let ctx = Context::new(&config);
    let solver = Solver::new(&ctx);

    let x = Real::new_const(&ctx, "x");
    let y = Real::new_const(&ctx, "y");
    let z = Real::new_const(&ctx, "z");
    let vx = Real::new_const(&ctx, "vx");
    let vy = Real::new_const(&ctx, "vy");
    let vz = Real::new_const(&ctx, "vz");
    let sum = Real::new_const(&ctx, "sum");
    solver.assert(&(&x).add(&y).add(&z)._eq(&sum));

    for (i, hailstone) in input.hailstones.iter().take(3).copied().enumerate() {
        let ti = Real::new_const(&ctx, format!("t{i}"));
        let xi = Real::from_int(&Int::from_i64(&ctx, hailstone.position.x));
        let yi = Real::from_int(&Int::from_i64(&ctx, hailstone.position.y));
        let zi = Real::from_int(&Int::from_i64(&ctx, hailstone.position.z));
        let vxi = Real::from_int(&Int::from_i64(&ctx, hailstone.velocity.x));
        let vyi = Real::from_int(&Int::from_i64(&ctx, hailstone.velocity.y));
        let vzi = Real::from_int(&Int::from_i64(&ctx, hailstone.velocity.z));
        solver.assert(&vxi.mul(&ti).add(&xi)._eq(&(&vx).mul(&ti).add(&x)));
        solver.assert(&vyi.mul(&ti).add(&yi)._eq(&(&vy).mul(&ti).add(&y)));
        solver.assert(&vzi.mul(&ti).add(&zi)._eq(&(&vz).mul(&ti).add(&z)));
    }

    solver.check();
    let model = solver.get_model().unwrap();

    let sum = model.get_const_interp(&sum).unwrap();
    sum.as_real().unwrap().0
}

aoc::main!(2023, 24, ex: 1);
