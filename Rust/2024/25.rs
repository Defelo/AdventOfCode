#![feature(test)]

type Input = Vec<Vec<Vec<bool>>>;

fn setup(input: &str) -> Input {
    input
        .trim()
        .split("\n\n")
        .map(|block| {
            block
                .lines()
                .map(|line| line.bytes().map(|b| b == b'#').collect())
                .collect()
        })
        .collect()
}

fn part1(input: &Input) -> usize {
    let filtered = |keys| {
        input.iter().filter(move |b| b[0][0] ^ keys).map(|b| {
            (0..b[0].len())
                .map(|i| b.iter().filter(|r| r[i]).count())
                .collect::<Vec<_>>()
        })
    };

    let locks = filtered(false).collect::<Vec<_>>();

    filtered(true)
        .flat_map(|k| {
            locks.iter().filter(move |l| {
                k.iter()
                    .zip(l.iter())
                    .all(|(&k, &l)| k + l <= input[0].len())
            })
        })
        .count()
}

aoc::main!(2024, 25, ex: 1[a]);
