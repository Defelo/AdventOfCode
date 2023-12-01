#![feature(test)]

type Input = Vec<String>;

fn setup(input: &str) -> Input {
    input.lines().map(ToOwned::to_owned).collect()
}

fn part1(input: &Input) -> u32 {
    input
        .iter()
        .map(|line| {
            let digits = line
                .bytes()
                .filter(|b| b.is_ascii_digit())
                .map(|b| (b - b'0') as u32);
            digits.clone().next().unwrap() * 10 + digits.last().unwrap()
        })
        .sum()
}

fn part2(input: &Input) -> u32 {
    input
        .iter()
        .map(|line| {
            let bytes = line.as_bytes();
            let digits = (0..line.len()).filter_map(|i| {
                let test = |s: &str| &bytes[i..bytes.len().min(i + s.len())] == s.as_bytes();
                match bytes[i] {
                    b @ b'0'..=b'9' => Some((b - b'0') as u32),
                    _ if test("one") => Some(1),
                    _ if test("two") => Some(2),
                    _ if test("three") => Some(3),
                    _ if test("four") => Some(4),
                    _ if test("five") => Some(5),
                    _ if test("six") => Some(6),
                    _ if test("seven") => Some(7),
                    _ if test("eight") => Some(8),
                    _ if test("nine") => Some(9),
                    _ => None,
                }
            });
            digits.clone().next().unwrap() * 10 + digits.last().unwrap()
        })
        .sum()
}

aoc::main!(2023, 1, ex: 1[a], 2[b]);
