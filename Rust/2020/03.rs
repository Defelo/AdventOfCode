#![feature(test)]

type Input = Vec<String>;

fn setup(input: &str) -> Input {
    input.lines().map(|s| s.to_string()).collect()
}

fn cnt_slope(input: &Input, dx: u64, dy: u64) -> u64 {
    let mut out = 0;
    let mut x = 0;
    let mut y = 0;
    while y < input.len() {
        if input[y].chars().nth(x % input[y].len()).unwrap() == '#' {
            out += 1;
        }
        x += dx as usize;
        y += dy as usize;
    }
    out
}

fn part1(input: &Input) -> String {
    cnt_slope(input, 3, 1).to_string()
}

fn part2(input: &Input) -> String {
    [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|&(dx, dy)| cnt_slope(input, dx, dy))
        .product::<u64>()
        .to_string()
}

aoc::main!(2020, 3, ex: 1);
