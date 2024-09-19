#![feature(test)]

type Input = Vec<i32>;

fn setup(input: &str) -> Input {
    input
        .bytes()
        .map(|b| (b == b'(') as i32 - (b == b')') as i32)
        .filter(|&b| b != 0)
        .collect()
}

fn part1(input: &Input) -> i32 {
    input.iter().sum()
}

fn part2(input: &Input) -> usize {
    input
        .iter()
        .scan(0, |acc, x| {
            *acc += *x;
            Some(*acc)
        })
        .position(|x| x == -1)
        .unwrap()
        + 1
}

aoc::main!(2015, 1);
