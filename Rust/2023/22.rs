#![feature(test)]

use itertools::Itertools;
use rayon::prelude::*;

type Input = Vec<Brick>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Brick(Pos3, Pos3);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Rect(Pos2, Pos2);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Pos3 {
    x: usize,
    y: usize,
    z: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Pos2 {
    x: usize,
    y: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Range(usize, usize);

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            let (a, b) = line
                .split('~')
                .map(|pos| {
                    let (x, y, z) = pos
                        .split(',')
                        .map(|x| x.parse().unwrap())
                        .collect_tuple()
                        .unwrap();
                    Pos3 { x, y, z }
                })
                .collect_tuple()
                .unwrap();
            Brick(a, b)
        })
        .collect()
}

impl Brick {
    fn can_support(self, other: Self) -> bool {
        self.xy_rect().overlaps(other.xy_rect()) && self.1.z < other.0.z
    }

    fn xy_rect(self) -> Rect {
        Rect(self.0.xy(), self.1.xy())
    }

    fn height(self) -> usize {
        self.1.z - self.0.z + 1
    }
}

impl Rect {
    fn overlaps(self, other: Self) -> bool {
        self.x_range().overlaps(other.x_range()) && self.y_range().overlaps(other.y_range())
    }

    fn x_range(self) -> Range {
        Range(self.0.x, self.1.x)
    }

    fn y_range(self) -> Range {
        Range(self.0.y, self.1.y)
    }
}

impl Pos3 {
    fn xy(self) -> Pos2 {
        Pos2 {
            x: self.x,
            y: self.y,
        }
    }
}

impl Range {
    fn overlaps(self, other: Self) -> bool {
        !(self.1 < other.0 || other.1 < self.0)
    }
}

struct Graph {
    support_matrix: Vec<Vec<bool>>,
    support_lst: Vec<Vec<usize>>,
    support_rev: Vec<Vec<usize>>,
    unstable: Vec<usize>,
}

fn adj_matrix_to_lists(matrix: &[Vec<bool>]) -> (Vec<Vec<usize>>, Vec<Vec<usize>>) {
    let n = matrix.len();
    let lst = (0..n)
        .into_par_iter()
        .map(|i| (0..n).filter(|&j| matrix[i][j]).collect())
        .collect();
    let rev = (0..n)
        .into_par_iter()
        .map(|i| (0..n).filter(|&j| matrix[j][i]).collect())
        .collect();
    (lst, rev)
}

fn build_graph(bricks: &[Brick]) -> Graph {
    let n = bricks.len();

    let can_support_matrix = bricks
        .par_iter()
        .map(|&a| bricks.iter().map(|&b| a.can_support(b)).collect_vec())
        .collect::<Vec<_>>();
    let (can_support_lst, can_support_rev) = adj_matrix_to_lists(&can_support_matrix);

    let mut blocked = can_support_rev.iter().map(|x| x.len()).collect_vec();
    let mut queue = (0..n).filter(|&i| blocked[i] == 0).collect_vec();
    let mut start_z = vec![0; n];
    while let Some(p) = queue.pop() {
        start_z[p] = can_support_rev[p]
            .iter()
            .map(|&i| start_z[i] + bricks[i].height())
            .max()
            .unwrap_or(1);
        queue.extend(can_support_lst[p].iter().filter(|&&q| {
            blocked[q] -= 1;
            blocked[q] == 0
        }));
    }

    let support_matrix = can_support_matrix
        .into_par_iter()
        .zip(&start_z)
        .zip(bricks)
        .map(|((can_support, &z1), brick)| {
            can_support
                .into_iter()
                .zip(&start_z)
                .map(|(can_support, &z2)| can_support && z1 + brick.height() == z2)
                .collect_vec()
        })
        .collect::<Vec<_>>();

    let (support_lst, support_rev) = adj_matrix_to_lists(&support_matrix);

    let unstable = (0..n).filter(|&i| support_rev[i].len() == 1).collect();

    Graph {
        support_matrix,
        support_lst,
        support_rev,
        unstable,
    }
}

fn part1(input: &Input) -> usize {
    let Graph {
        support_matrix,
        unstable,
        ..
    } = build_graph(input);
    support_matrix
        .par_iter()
        .filter(|x| !unstable.iter().any(|&i| x[i]))
        .count()
}

fn part2(input: &Input) -> usize {
    let Graph {
        support_matrix,
        support_lst,
        support_rev,
        unstable,
    } = build_graph(input);

    support_matrix
        .par_iter()
        .enumerate()
        .filter(|(_, x)| unstable.iter().any(|&i| x[i]))
        .map(|(i, _)| {
            let mut out = 0;
            let mut blocked = support_rev.iter().map(|x| x.len()).collect_vec();
            let mut queue = vec![i];
            while let Some(p) = queue.pop() {
                out += 1;
                queue.extend(support_lst[p].iter().filter(|&&q| {
                    blocked[q] -= 1;
                    blocked[q] == 0
                }));
            }
            out - 1
        })
        .sum()
}

aoc::main!(2023, 22, ex: 1);
