#![feature(test)]

type Input = Vec<Vec<u8>>;

const NEIGH: [(isize, isize); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

fn setup(input: &str) -> Input {
    input.trim().lines().map(|l| l.bytes().collect()).collect()
}

fn part1(input: &Input) -> usize {
    let height = input.len() as isize;
    let width = input[0].len() as isize;
    (0..height)
        .flat_map(|i| {
            (0..width).flat_map(move |j| {
                NEIGH.iter().filter(move |&(di, dj)| {
                    "XMAS".bytes().enumerate().all(|(k, b)| {
                        let k = k as isize;
                        let i = i + di * k;
                        let j = j + dj * k;
                        (0..height).contains(&i)
                            && (0..width).contains(&j)
                            && input[i as usize][j as usize] == b
                    })
                })
            })
        })
        .count()
}

fn part2(input: &Input) -> usize {
    let height = input.len();
    let width = input[0].len();
    (1..height - 1)
        .flat_map(|i| {
            (1..width - 1).filter(move |&j| {
                const OUTER: [(u8, u8); 2] = [(b'M', b'S'), (b'S', b'M')];
                input[i][j] == b'A'
                    && OUTER.contains(&(input[i - 1][j - 1], input[i + 1][j + 1]))
                    && OUTER.contains(&(input[i - 1][j + 1], input[i + 1][j - 1]))
            })
        })
        .count()
}

aoc::main!(2024, 4, ex: 1);
