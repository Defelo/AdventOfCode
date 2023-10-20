#![feature(test)]

type Input<'a> = Vec<&'a [u8]>;

fn setup(input: &str) -> Input {
    input
        .trim()
        .split('\n')
        .map(|line| line.as_bytes())
        .collect()
}

fn to_num(x: u8) -> u8 {
    x - 38 - (x >= 97) as u8 * 58
}

fn bytes_to_set(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0, |acc, x| acc | 1 << to_num(*x))
}

fn part1(input: &Input) -> u32 {
    input
        .iter()
        .map(|line| {
            let n = line.len() >> 1;
            (bytes_to_set(&line[..n]) & bytes_to_set(&line[n..])).trailing_zeros()
        })
        .sum()
}

fn part2(input: &Input) -> u32 {
    (0..input.len())
        .step_by(3)
        .map(|i| {
            (bytes_to_set(input[i]) & bytes_to_set(input[i + 1]) & bytes_to_set(input[i + 2]))
                .trailing_zeros()
        })
        .sum()
}

aoc::main!(2022, 3, ex: 1);
