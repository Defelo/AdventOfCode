#![feature(test)]

use std::{cmp::Reverse, collections::BinaryHeap};

use aoc::grid::Direction;

type Input = Vec<Vec<u8>>;

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| line.bytes().map(|b| b - b'0').collect())
        .collect()
}

fn dijkstra<const MIN: usize, const MAX: usize>(input: &Input) -> u32 {
    let h = input.len();
    let w = input[0].len();

    let mut queue = BinaryHeap::from_iter([
        (Reverse(0), (0, 0), Direction::East),
        (Reverse(0), (0, 0), Direction::South),
    ]);
    let mut visited = vec![false; h * w * 4];
    while let Some((Reverse(c), (x, y), pd)) = queue.pop() {
        if x == input[0].len() - 1 && y == input.len() - 1 {
            return c;
        }
        let i = (x + y * w) * 4 + pd as usize;
        if visited[i] {
            continue;
        }
        visited[i] = true;
        queue.extend(
            [pd.rotate_left(), pd.rotate_right()]
                .into_iter()
                .flat_map(|d| {
                    std::iter::successors(Some((c, x, y)), move |&(c, x, y)| {
                        let (x, y) = d.step(x, y, w, h)?;
                        Some((c + input[y][x] as u32, x, y))
                    })
                    .take(MAX + 1)
                    .skip(MIN)
                    .map(move |(c, x, y)| (Reverse(c), (x, y), d))
                }),
        );
    }

    panic!()
}

fn part1(input: &Input) -> u32 {
    dijkstra::<1, 3>(input)
}

fn part2(input: &Input) -> u32 {
    dijkstra::<4, 10>(input)
}

aoc::main!(2023, 17, ex: 1, 2[b]);
