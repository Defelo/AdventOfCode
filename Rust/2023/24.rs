#![feature(test)]

use std::ops::{Add, Mul};

use itertools::{Itertools, PeekingNext};
use num::rational::Ratio;
use rayon::prelude::*;
use z3::{
    ast::{Ast, Int},
    Config, Context, Solver,
};

#[derive(Debug)]
struct Input {
    hailstones: Vec<Hailstone>,
    test_start: i64,
    test_end: i64,
}

type Hailstone = (Vec3, Vec3);
type Vec3 = (i64, i64, i64);

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
            line.split('@')
                .map(|split| {
                    split
                        .split(',')
                        .map(|x| x.trim().parse().unwrap())
                        .collect_tuple()
                        .unwrap()
                })
                .collect_tuple()
                .unwrap()
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
        .par_iter()
        .copied()
        .enumerate()
        .map(|(i, ((x1, y1, _), (vx1, vy1, _)))| {
            input
                .hailstones
                .iter()
                .copied()
                .take(i)
                .filter(|&(_, (vx2, vy2, _))| vx1 * vy2 != vy1 * vx2)
                .filter(|&((x2, y2, _), (vx2, vy2, _))| {
                    let x1 = Ratio::from(x1 as i128);
                    let y1 = Ratio::from(y1 as i128);
                    let x2 = Ratio::from(x2 as i128);
                    let y2 = Ratio::from(y2 as i128);
                    let vx1 = Ratio::from(vx1 as i128);
                    let vy1 = Ratio::from(vy1 as i128);
                    let vx2 = Ratio::from(vx2 as i128);
                    let vy2 = Ratio::from(vy2 as i128);

                    let t1 = ((y1 - y2) * vx2 - (x1 - x2) * vy2) / (vx1 * vy2 - vy1 * vx2);
                    let t2 = ((y2 - y1) * vx1 - (x2 - x1) * vy1) / (vx2 * vy1 - vy2 * vx1);
                    let x = x1 + t1 * vx1;
                    let y = y1 + t1 * vy1;
                    assert_eq!(x, x2 + t2 * vx2);
                    assert_eq!(y, y2 + t2 * vy2);

                    t1 >= 0.into()
                        && t2 >= 0.into()
                        && x >= test_start
                        && x <= test_end
                        && y >= test_start
                        && y <= test_end
                })
                .count()
        })
        .sum()
}

fn part2(input: &Input) -> i64 {
    let config = Config::new();
    let ctx = Context::new(&config);
    let solver = Solver::new(&ctx);

    let x = Int::new_const(&ctx, "x");
    let y = Int::new_const(&ctx, "y");
    let z = Int::new_const(&ctx, "z");
    let vx = Int::new_const(&ctx, "vx");
    let vy = Int::new_const(&ctx, "vy");
    let vz = Int::new_const(&ctx, "vz");

    for (i, ((xi, yi, zi), (vxi, vyi, vzi))) in input.hailstones.iter().take(3).copied().enumerate()
    {
        let ti = Int::new_const(&ctx, format!("t{i}"));
        let xi = Int::from_i64(&ctx, xi);
        let yi = Int::from_i64(&ctx, yi);
        let zi = Int::from_i64(&ctx, zi);
        let vxi = Int::from_i64(&ctx, vxi);
        let vyi = Int::from_i64(&ctx, vyi);
        let vzi = Int::from_i64(&ctx, vzi);
        solver.assert(&vxi.mul(&ti).add(&xi)._eq(&(&vx).mul(&ti).add(&x)));
        solver.assert(&vyi.mul(&ti).add(&yi)._eq(&(&vy).mul(&ti).add(&y)));
        solver.assert(&vzi.mul(&ti).add(&zi)._eq(&(&vz).mul(&ti).add(&z)));
    }

    solver.check();
    let model = solver.get_model().unwrap();

    let x = model.get_const_interp(&x).unwrap().as_i64().unwrap();
    let y = model.get_const_interp(&y).unwrap().as_i64().unwrap();
    let z = model.get_const_interp(&z).unwrap().as_i64().unwrap();
    x + y + z
}

aoc::main!(2023, 24, ex: 1);
