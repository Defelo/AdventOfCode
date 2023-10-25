#![feature(test, isqrt)]

use aoc::iter_ext::IterExt;
use itertools::Itertools;
use regex::Regex;

struct Input {
    instructions: Vec<Instruction>,
    map: Map,
    first: usize,
    n: usize,
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Forward(u32),
    Left,
    Right,
}

type Map = Vec<Vec<Option<SubMap>>>;

#[derive(Debug)]
struct SubMap {
    grid: Vec<Vec<bool>>,
    face: Face,
    rotation: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Face {
    Front,
    Back,
    Up,
    Down,
    Left,
    Right,
}

fn setup(input: &str) -> Input {
    let mut lines = input.lines();
    let map = lines.take_while_ref(|l| !l.is_empty()).collect_vec();
    let instructions = lines.nth(1).unwrap();

    let n = map
        .iter()
        .flat_map(|l| l.chars().filter(|&c| c == '.' || c == '#'))
        .count();
    let n = (n / 6).isqrt();

    let h = map.len() / n;
    let w = map.iter().map(|l| l.len() / n).max().unwrap();
    let mut map = map
        .chunks(n)
        .map(|c| {
            c.iter()
                .map(|l| {
                    l.chars()
                        .chain(std::iter::repeat(' '))
                        .chunks(n)
                        .into_iter()
                        .take(w)
                        .map(|c| c.collect_vec())
                        .collect_vec()
                        .into_iter()
                })
                .transpose()
                .map(|c| {
                    (c[0][0] != ' ').then(|| SubMap {
                        grid: c
                            .into_iter()
                            .map(|i| i.into_iter().map(|c| c == '.').collect_vec())
                            .collect_vec(),
                        face: Face::Front,
                        rotation: u8::MAX,
                    })
                })
                .collect_vec()
        })
        .collect_vec();

    let first = map[0].iter().position(|x| x.is_some()).unwrap();
    map[0][first].as_mut().unwrap().rotation = 0;

    let mut queue = vec![(0, first)];
    while let Some((i, j)) = queue.pop() {
        let &SubMap { face, rotation, .. } = map[i][j].as_ref().unwrap();
        let (i, j) = (i as isize, j as isize);
        for (p, q, r) in [(i - 1, j, 0), (i + 1, j, 2), (i, j - 1, 3), (i, j + 1, 1)] {
            if p < 0 || q < 0 || p >= h as _ || q >= w as _ {
                continue;
            }
            let (p, q) = (p as usize, q as usize);
            if !map[p][q]
                .as_ref()
                .is_some_and(|sub| sub.rotation == u8::MAX)
            {
                continue;
            }

            let (face, rot) = match (face, (rotation + r) % 4) {
                (Face::Front, 0) => (Face::Up, 0),
                (Face::Front, 1) => (Face::Right, 0),
                (Face::Front, 2) => (Face::Down, 2),
                (Face::Front, 3) => (Face::Left, 0),
                (Face::Back, 0) => (Face::Up, 2),
                (Face::Back, 1) => (Face::Left, 0),
                (Face::Back, 2) => (Face::Down, 0),
                (Face::Back, 3) => (Face::Right, 0),
                (Face::Left, 0) => (Face::Up, 1),
                (Face::Left, 1) => (Face::Front, 0),
                (Face::Left, 2) => (Face::Down, 1),
                (Face::Left, 3) => (Face::Back, 0),
                (Face::Right, 0) => (Face::Up, 3),
                (Face::Right, 1) => (Face::Back, 0),
                (Face::Right, 2) => (Face::Down, 3),
                (Face::Right, 3) => (Face::Front, 0),
                (Face::Up, 0) => (Face::Back, 2),
                (Face::Up, 1) => (Face::Right, 1),
                (Face::Up, 2) => (Face::Front, 0),
                (Face::Up, 3) => (Face::Left, 3),
                (Face::Down, 0) => (Face::Back, 0),
                (Face::Down, 1) => (Face::Left, 3),
                (Face::Down, 2) => (Face::Front, 2),
                (Face::Down, 3) => (Face::Right, 1),
                _ => unreachable!(),
            };
            let sub = map[p][q].as_mut().unwrap();
            sub.face = face;
            sub.rotation = (rot + rotation) % 4;
            queue.push((p, q));
        }
    }

    let instructions = Regex::new(r"\d+|[LR]")
        .unwrap()
        .find_iter(instructions)
        .map(|x| match x.as_str() {
            "L" => Instruction::Left,
            "R" => Instruction::Right,
            x => Instruction::Forward(x.parse().unwrap()),
        })
        .collect();

    Input {
        instructions,
        map,
        first,
        n,
    }
}

type NextSubDir = (usize, usize, isize, isize);
type NextSub = Vec<Vec<(NextSubDir, NextSubDir, NextSubDir, NextSubDir)>>;

fn trace_path(input: &Input, next_sub: NextSub) -> usize {
    let mut i = 0;
    let mut j = input.first;
    let sub = input.map[i][j].as_ref().unwrap();
    let mut p = sub.grid.iter().position(|r| r.contains(&true)).unwrap() as isize;
    let mut q = sub.grid[p as usize].iter().position(|x| *x).unwrap() as isize;
    let mut dp = 0;
    let mut dq = 1;

    for &instruction in &input.instructions {
        match instruction {
            Instruction::Forward(n) => {
                for _ in 0..n {
                    let (ni, nj, ndp, ndq): (usize, usize, isize, isize) = if p + dp < 0 {
                        next_sub[i][j].0
                    } else if p + dp >= input.n as isize {
                        next_sub[i][j].1
                    } else if q + dq < 0 {
                        next_sub[i][j].2
                    } else if q + dq >= input.n as isize {
                        next_sub[i][j].3
                    } else {
                        (i, j, dp, dq)
                    };

                    let mut np = p + dp;
                    let mut nq = q + dq;

                    if ndp == -dp && ndq == -dq {
                        (np, nq) = (-np - 1, -nq - 1);
                    } else if ndp == -dq && ndq == dp {
                        (np, nq) = (-nq - 1, np);
                    } else if ndp == dq && ndq == -dp {
                        (np, nq) = (nq, -np - 1);
                    } else if ndp == dp && ndq == dq {
                    } else {
                        unreachable!();
                    }

                    let (np, nq) = (np.rem_euclid(input.n as _), nq.rem_euclid(input.n as _));

                    if !input.map[ni][nj].as_ref().unwrap().grid[np as usize][nq as usize] {
                        break;
                    }

                    (i, j, p, q, dp, dq) = (ni, nj, np, nq, ndp, ndq);
                }
            }
            Instruction::Left => {
                (dp, dq) = (-dq, dp);
            }
            Instruction::Right => {
                (dp, dq) = (dq, -dp);
            }
        }
    }

    let row = i * input.n + p as usize + 1;
    let col = j * input.n + q as usize + 1;
    let facing = match (dp, dq) {
        (0, 1) => 0,
        (1, 0) => 1,
        (0, -1) => 2,
        (-1, 0) => 3,
        _ => unreachable!(),
    };
    1000 * row + 4 * col + facing as usize
}

fn part1(input: &Input) -> usize {
    let h = input.map.len();
    let w = input.map[0].len();
    let next_sub = input
        .map
        .iter()
        .enumerate()
        .map(|(i, r)| {
            r.iter()
                .enumerate()
                .map(|(j, _)| {
                    let u = (0..i)
                        .rev()
                        .chain((i..h).rev())
                        .find(|&u| input.map[u][j].is_some())
                        .unwrap();
                    let d = (i + 1..h)
                        .chain(0..i + 1)
                        .find(|&d| input.map[d][j].is_some())
                        .unwrap();
                    let l = (0..j)
                        .rev()
                        .chain((j..w).rev())
                        .find(|&l| input.map[i][l].is_some())
                        .unwrap();
                    let r = (j + 1..w)
                        .chain(0..j + 1)
                        .find(|&r| input.map[i][r].is_some())
                        .unwrap();

                    ((u, j, -1, 0), (d, j, 1, 0), (i, l, 0, -1), (i, r, 0, 1))
                })
                .collect_vec()
        })
        .collect_vec();

    trace_path(input, next_sub)
}

fn part2(input: &Input) -> usize {
    let find = |face: Face| {
        input
            .map
            .iter()
            .enumerate()
            .flat_map(|(i, r)| {
                r.iter().enumerate().map(move |(j, r)| {
                    r.as_ref()
                        .and_then(|r| (r.face == face).then_some((i, j, r.rotation)))
                })
            })
            .find_map(|x| x)
            .unwrap()
    };

    let front = find(Face::Front);
    let back = find(Face::Back);
    let left = find(Face::Left);
    let right = find(Face::Right);
    let up = find(Face::Up);
    let down = find(Face::Down);

    let next_sub = input
        .map
        .iter()
        .map(|r| {
            r.iter()
                .map(|r| {
                    let Some(r) = r else {
                        return Default::default();
                    };

                    let mut dests = match r.face {
                        Face::Front => [(up, 2), (right, 3), (down, 2), (left, 1)],
                        Face::Back => [(up, 0), (left, 3), (down, 0), (right, 1)],
                        Face::Up => [(back, 0), (right, 0), (front, 0), (left, 0)],
                        Face::Down => [(back, 2), (left, 2), (front, 2), (right, 2)],
                        Face::Left => [(up, 3), (front, 3), (down, 1), (back, 1)],
                        Face::Right => [(up, 1), (back, 3), (down, 3), (front, 1)],
                    };

                    dests.rotate_left(r.rotation as _);

                    let dests = dests
                        .into_iter()
                        .map(|((ni, nj, fr), r)| {
                            let (dp, dq) = match (4 + r - fr) % 4 {
                                0 => (1, 0),
                                1 => (0, -1),
                                2 => (-1, 0),
                                3 => (0, 1),
                                _ => unreachable!(),
                            };
                            (ni, nj, dp, dq)
                        })
                        .collect_vec();

                    (dests[0], dests[2], dests[3], dests[1])
                })
                .collect_vec()
        })
        .collect_vec();

    trace_path(input, next_sub)
}

aoc::main!(2022, 22, ex: 1);
