#![feature(test)]

type Input = Vec<(u8, u8, u8, u8)>;

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            let mut it = line.split(&[',', '-']).map(|x| x.parse().unwrap());
            (|| Some((it.next()?, it.next()?, it.next()?, it.next()?)))().unwrap()
        })
        .collect()
}

fn part1(input: &Input) -> usize {
    input
        .iter()
        .filter(|(a, b, c, d)| a <= c && d <= b || c <= a && b <= d)
        .count()
}

fn part2(input: &Input) -> usize {
    input.iter().filter(|(a, b, c, d)| d >= a && c <= b).count()
}

aoc::main!(2022, 4, ex: 1);
