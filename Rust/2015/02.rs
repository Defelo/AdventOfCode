#![feature(test)]

type Input = Vec<Present>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Present {
    l: u64,
    w: u64,
    h: u64,
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| {
            let mut dims = line.split('x').map(|x| x.parse().unwrap());
            let present = Present {
                l: dims.next().unwrap(),
                w: dims.next().unwrap(),
                h: dims.next().unwrap(),
            };
            assert!(dims.next().is_none());
            present
        })
        .collect()
}

fn part1(input: &Input) -> u64 {
    input
        .iter()
        .map(|&Present { l, w, h }| {
            let surface = 2 * l * w + 2 * w * h + 2 * h * l;
            let extra = l * w * h / l.max(w).max(h);
            surface + extra
        })
        .sum()
}

fn part2(input: &Input) -> u64 {
    input
        .iter()
        .map(|&Present { l, w, h }| {
            let ribbon = 2 * (l + w + h - l.max(w).max(h));
            let bow = l * w * h;
            ribbon + bow
        })
        .sum()
}

aoc::main!(2015, 2, ex: 1);
