#![feature(test)]

type Input = Vec<Game>;
type Game = Vec<CubeCounts>;

#[derive(Debug, Clone, Copy, Default)]
struct CubeCounts {
    red: u32,
    green: u32,
    blue: u32,
}

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            line.split(": ")
                .nth(1)
                .unwrap()
                .split("; ")
                .map(|round| {
                    round
                        .split(", ")
                        .fold(CubeCounts::default(), |mut acc, round| {
                            let mut round = round.split(' ');
                            let count = round.next().unwrap().parse().unwrap();
                            let color = round.next().unwrap();
                            match color {
                                "red" => acc.red = count,
                                "green" => acc.green = count,
                                "blue" => acc.blue = count,
                                _ => panic!(),
                            }
                            acc
                        })
                })
                .collect()
        })
        .collect()
}

fn min_config(game: &Game) -> CubeCounts {
    game.iter()
        .fold(CubeCounts::default(), |acc, &round| CubeCounts {
            red: acc.red.max(round.red),
            green: acc.green.max(round.green),
            blue: acc.blue.max(round.blue),
        })
}

fn part1(input: &Input) -> usize {
    input
        .iter()
        .enumerate()
        .filter(|(_, game)| {
            let min = min_config(game);
            min.red <= 12 && min.green <= 13 && min.blue <= 14
        })
        .map(|(i, _)| i + 1)
        .sum()
}

fn part2(input: &Input) -> u32 {
    input
        .iter()
        .map(|game| {
            let min = min_config(game);
            min.red * min.green * min.blue
        })
        .sum()
}

aoc::main!(2023, 2, ex: 1);
