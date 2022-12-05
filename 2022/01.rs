type Input = Vec<Vec<u32>>;

fn setup(input: &str) -> Input {
    input
        .trim()
        .split("\n\n")
        .map(|elf| elf.split_whitespace().map(|x| x.parse().unwrap()).collect())
        .collect()
}

fn part1(input: &Input) -> u32 {
    input.iter().map(|elf| elf.iter().sum()).max().unwrap()
}

fn part2(input: &Input) -> u32 {
    let (a, b, c) = input
        .iter()
        .map(|elf| elf.iter().sum())
        .fold((0, 0, 0), |(a, b, c), x| {
            if x > b {
                if x > a {
                    (x, a, b)
                } else {
                    (a, x, b)
                }
            } else if x > c {
                (a, b, x)
            } else {
                (a, b, c)
            }
        });
    a + b + c
}

aoc::main!(2022, 1);
