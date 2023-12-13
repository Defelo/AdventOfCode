#![feature(test)]

type Input = Vec<Grid>;

type Grid = Vec<Vec<bool>>;

fn setup(input: &str) -> Input {
    input
        .split("\n\n")
        .map(|block| {
            block
                .lines()
                .map(|line| line.bytes().map(|b| b == b'#').collect())
                .collect()
        })
        .collect()
}

#[derive(Debug, Clone, Copy)]
enum Reflection {
    Vertical(usize),
    Horizontal(usize),
}

impl Reflection {
    fn generate_for(grid: &Grid) -> impl Iterator<Item = Self> {
        (1..grid.len())
            .map(Self::Horizontal)
            .chain((1..grid[0].len()).map(Self::Vertical))
    }

    fn count_mismatches(self, grid: &Grid) -> usize {
        match self {
            Reflection::Vertical(j) => (0..grid.len())
                .map(|i| {
                    let len = j.min(grid[0].len() - j);
                    (j - len..j)
                        .rev()
                        .zip(j..j + len)
                        .filter(|&(j1, j2)| grid[i][j1] != grid[i][j2])
                        .count()
                })
                .sum(),
            Reflection::Horizontal(i) => (0..grid[0].len())
                .map(|j| {
                    let len = i.min(grid.len() - i);
                    (i - len..i)
                        .rev()
                        .zip(i..i + len)
                        .filter(|&(i1, i2)| grid[i1][j] != grid[i2][j])
                        .count()
                })
                .sum(),
        }
    }

    fn summarize(self) -> usize {
        match self {
            Self::Vertical(j) => j,
            Self::Horizontal(i) => 100 * i,
        }
    }
}

fn solve<const N: usize>(input: &Input) -> usize {
    input
        .iter()
        .map(|grid| {
            Reflection::generate_for(grid)
                .find(|reflection| dbg!(reflection.count_mismatches(grid)) == N)
                .unwrap()
                .summarize()
        })
        .sum()
}

fn part1(input: &Input) -> usize {
    solve::<0>(input)
}

fn part2(input: &Input) -> usize {
    solve::<1>(input)
}

aoc::main!(2023, 13, ex: 1);
