#![feature(test)]

type Input = Vec<i32>;

fn setup(input: &str) -> Input {
    input
        .trim()
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect()
}

fn part1(input: &Input) -> String {
    (0..=input.len())
        .map(|pos| input.iter().map(|n| (pos as i32 - *n).abs()).sum::<i32>())
        .min()
        .unwrap()
        .to_string()
}

fn part2(input: &Input) -> String {
    (0..=input.len())
        .map(|pos| {
            input
                .iter()
                .map(|n| {
                    let x = (pos as i32 - *n).abs();
                    (x * (x + 1)) >> 1
                })
                .sum::<i32>()
        })
        .min()
        .unwrap()
        .to_string()
}

aoc::main!(2021, 7, ex: 1);
