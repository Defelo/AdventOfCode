#![feature(test)]

use aoc::grid::Direction;
use itertools::Itertools;

type Input = Vec<Vec<u8>>;

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| line.bytes().collect())
        .collect()
}

struct Regions {
    area_by_id: Vec<usize>,
    id_by_idx: Vec<usize>,
}

fn dfs(input: &Input) -> Regions {
    let rows = input.len();
    let cols = input[0].len();

    let mut stack = (0..rows)
        .flat_map(|i| (0..cols).map(move |j| (i, j, usize::MAX)))
        .collect::<Vec<_>>();
    let mut visited = vec![false; rows * cols];

    let mut regions = Regions {
        area_by_id: Vec::new(),
        id_by_idx: vec![usize::MAX; rows * cols],
    };

    while let Some((i, j, mut region_id)) = stack.pop() {
        let idx = i * cols + j;
        if visited[idx] {
            continue;
        };
        visited[idx] = true;

        if region_id == usize::MAX {
            region_id = regions.area_by_id.len();
            regions.area_by_id.push(0);
        }

        regions.id_by_idx[idx] = region_id;
        regions.area_by_id[region_id] += 1;

        stack.extend(
            Direction::iter()
                .flat_map(|d| d.step(j, i, cols, rows))
                .filter(|&(nj, ni)| input[ni][nj] == input[i][j])
                .map(|(nj, ni)| (ni, nj, region_id)),
        );
    }

    regions
}

fn part1(input: &Input) -> usize {
    let rows = input.len();
    let cols = input[0].len();

    let regions = dfs(input);

    let mut region_perimeters = regions
        .area_by_id
        .iter()
        .map(|&area| area * 4)
        .collect::<Vec<_>>();
    (0..rows)
        .flat_map(|i| (0..cols).map(move |j| (i, j)))
        .for_each(|(i, j)| {
            region_perimeters[regions.id_by_idx[i * cols + j]] -= Direction::iter()
                .flat_map(|d| d.step(j, i, cols, rows))
                .filter(|&(nj, ni)| input[ni][nj] == input[i][j])
                .count();
        });

    regions
        .area_by_id
        .into_iter()
        .zip(region_perimeters)
        .map(|(area, perimeter)| area * perimeter)
        .sum()
}

fn part2(input: &Input) -> usize {
    let rows = input.len();
    let cols = input[0].len();

    let regions = dfs(input);

    let mut corner_counts = vec![0; regions.area_by_id.len()];
    (0..=rows)
        .flat_map(|i| (0..=cols).map(move |j| (i, j)))
        .for_each(|(i, j)| {
            [(0, 0), (0, 1), (1, 1), (1, 0)]
                .map(|(ci, cj)| {
                    ((1..=cols).contains(&(i + ci)) && (1..=rows).contains(&(j + cj)))
                        .then(|| regions.id_by_idx[(i + ci - 1) * cols + j + cj - 1])
                        .unwrap_or(usize::MAX)
                })
                .into_iter()
                .circular_tuple_windows()
                .filter(|&(a, _, _, _)| a != usize::MAX)
                .filter(|&(a, b, c, d)| a != b && a != d || a == b && a == d && a != c)
                .for_each(|(a, _, _, _)| corner_counts[a] += 1);
        });

    regions
        .area_by_id
        .into_iter()
        .zip(corner_counts)
        .map(|(area, corners)| area * corners)
        .sum()
}

aoc::main!(2024, 12, ex: 1, 2, 3, 4, 5);
