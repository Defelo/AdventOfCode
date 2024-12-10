#![feature(test)]

use aoc::grid::Direction;

type Input = Vec<Vec<u8>>;

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| l.bytes().map(|b| b - b'0').collect())
        .collect()
}

fn solve<const P2: bool>(input: &Input) -> usize {
    let mut visited = vec![usize::MAX; if !P2 { input.len() * input[0].len() } else { 0 }];
    input
        .iter()
        .enumerate()
        .flat_map(|(i, row)| {
            row.iter()
                .enumerate()
                .filter(|(_, &x)| x == 0)
                .map(move |(j, _)| (i, j))
        })
        .map(|(si, sj)| {
            let mut stack = vec![(sj, si)];
            let mut out = 0;
            while let Some((j, i)) = stack.pop() {
                if !P2 {
                    let k = j * input[0].len() + i;
                    let v = sj * input[0].len() + si;
                    if visited[k] == v {
                        continue;
                    }
                    visited[k] = v;
                }

                if input[i][j] == 9 {
                    out += 1;
                    continue;
                }
                stack.extend(Direction::iter().flat_map(|d| {
                    d.step(j, i, input[0].len(), input.len())
                        .filter(|&(nj, ni)| input[ni][nj] == input[i][j] + 1)
                }));
            }
            out
        })
        .sum()
}

fn part1(input: &Input) -> usize {
    solve::<false>(input)
}

fn part2(input: &Input) -> usize {
    solve::<true>(input)
}

aoc::main!(2024, 10, ex: 1);
