type Input = Vec<(u32, u32)>;

fn get_input(input: &str) -> Input {
    input
        .trim()
        .split('\n')
        .map(|line| {
            (
                (line.as_bytes()[0] - b'A') as u32,
                (line.as_bytes()[2] - b'X') as u32,
            )
        })
        .collect()
}

fn part1(input: &Input) -> u32 {
    input.iter().map(|(a, b)| (4 + b - a) % 3 * 3 + b + 1).sum()
}

fn part2(input: &Input) -> u32 {
    input.iter().map(|(a, b)| (2 + a + b) % 3 + 1 + b * 3).sum()
}

aoc::main!(2022, 2);
