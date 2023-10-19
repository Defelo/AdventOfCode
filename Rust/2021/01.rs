#![feature(test)]

type Input = Vec<i32>;

fn setup(input: &str) -> Input {
    input.lines().map(|x| x.parse().unwrap()).collect()
}

fn count(input: &Input, window: usize) -> i32 {
    let mut out = 0;
    for (a, b) in input.iter().zip(&input[window..]) {
        if b > a {
            out += 1;
        }
    }
    out
}

fn part1(input: &Input) -> String {
    count(input, 1).to_string()
}

fn part2(input: &Input) -> String {
    count(input, 3).to_string()
}

aoc::main!(2021, 1, ex: 1);
