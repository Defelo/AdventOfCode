#[derive(Clone, Copy)]
enum Shape {
    Rock = 0,
    Paper = 1,
    Scissors = 2,
}

impl Shape {
    fn outcome(&self, opponent: &Self) -> Outcome {
        match (3 + *opponent as u8 - *self as u8) % 3 {
            0 => Outcome::Draw,
            1 => Outcome::Win,
            2 => Outcome::Lose,
            _ => unreachable!(),
        }
    }

    fn choose_by_outcome(&self, outcome: &Outcome) -> Self {
        match (*self as u8 + *outcome as u8) % 3 {
            0 => Self::Rock,
            1 => Self::Paper,
            2 => Self::Scissors,
            _ => unreachable!(),
        }
    }

    fn score(&self) -> u32 {
        *self as u32 + 1
    }
}

#[derive(Clone, Copy)]
enum Outcome {
    Draw = 0,
    Win = 1,
    Lose = 2,
}

impl Outcome {
    fn score(&self) -> u32 {
        match self {
            Self::Lose => 0,
            Self::Draw => 3,
            Self::Win => 6,
        }
    }
}

enum ShapeOrOutcome {
    X,
    Y,
    Z,
}

impl From<&ShapeOrOutcome> for Shape {
    fn from(soo: &ShapeOrOutcome) -> Self {
        match soo {
            ShapeOrOutcome::X => Self::Rock,
            ShapeOrOutcome::Y => Self::Paper,
            ShapeOrOutcome::Z => Self::Scissors,
        }
    }
}

impl From<&ShapeOrOutcome> for Outcome {
    fn from(soo: &ShapeOrOutcome) -> Self {
        match soo {
            ShapeOrOutcome::X => Self::Lose,
            ShapeOrOutcome::Y => Self::Draw,
            ShapeOrOutcome::Z => Self::Win,
        }
    }
}

type Input = Vec<(Shape, ShapeOrOutcome)>;

fn get_input(input: &str) -> Input {
    input
        .trim()
        .split('\n')
        .map(|line| {
            let a = match line.chars().next().unwrap() {
                'A' => Shape::Rock,
                'B' => Shape::Paper,
                'C' => Shape::Scissors,
                _ => panic!(),
            };
            let b = match line.chars().nth(2).unwrap() {
                'X' => ShapeOrOutcome::X,
                'Y' => ShapeOrOutcome::Y,
                'Z' => ShapeOrOutcome::Z,
                _ => panic!(),
            };
            (a, b)
        })
        .collect()
}

fn part1(input: &Input) -> u32 {
    input
        .iter()
        .map(|(a, b)| {
            let b = b.into();
            a.outcome(&b).score() + b.score()
        })
        .sum()
}

fn part2(input: &Input) -> u32 {
    input
        .iter()
        .map(|(a, b)| {
            let b = b.into();
            a.choose_by_outcome(&b).score() + b.score()
        })
        .sum()
}

aoc::main!(2022, 2);
