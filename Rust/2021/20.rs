#![feature(test)]

use rustc_hash::FxHashSet;

struct Input {
    algo: Vec<bool>,
    width: usize,
    height: usize,
    grid: FxHashSet<(i32, i32)>,
}

fn setup(input: &str) -> Input {
    let mut lines = input.lines();
    let algo = lines.next().unwrap().chars().map(|c| c == '#').collect();
    let grid = lines
        .skip(1)
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars().enumerate().filter_map(move |(j, c)| {
                if c == '#' {
                    Option::Some((j as i32, i as i32))
                } else {
                    Option::None
                }
            })
        })
        .collect();
    Input {
        algo,
        grid,
        width: input.lines().nth(2).unwrap().len(),
        height: input.lines().count() - 2,
    }
}

fn get_neighbors(x: i32, y: i32) -> Vec<(i32, i32)> {
    (-1..=1)
        .flat_map(|dy| (-1..=1).map(move |dx| (x + dx, y + dy)))
        .collect()
}

fn solve(input: &Input) -> (usize, usize) {
    let mut grid = input.grid.clone();
    let mut inf = false;
    let mut part1 = 0;
    let mut part2 = 0;
    for i in 1..=50 {
        let new_inf = inf != input.algo[0];
        grid = (-i..=input.height as i32 + i)
            .flat_map(|y| (-i..=input.width as i32 + i).map(move |x| (x, y)))
            .filter(|(x, y)| {
                let idx = get_neighbors(*x, *y).iter().fold(0, |acc, (p, q)| {
                    acc << 1 | (grid.contains(&(*p, *q)) != inf) as usize
                });
                input.algo[idx] != new_inf
            })
            .collect();
        inf = new_inf;
        if i == 2 {
            part1 = grid.len();
        }
        if i == 50 {
            part2 = grid.len();
        }
    }
    (part1, part2)
}

fn part1(input: &Input) -> usize {
    solve(input).0
}

fn part2(input: &Input) -> usize {
    solve(input).1
}

aoc::main!(2021, 20, ex: 1);
