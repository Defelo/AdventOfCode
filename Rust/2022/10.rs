#![feature(test)]

use aoc::parsing::parse_ascii;

type Input = Vec<Instruction>;

enum Instruction {
    Noop,
    Addx(i32),
}

struct Cpu<'a, I> {
    instructions: I,
    current: Option<&'a Instruction>,
    cycle: usize,
    x: i32,
}

impl<'a, I> Cpu<'a, I>
where
    I: Iterator<Item = &'a Instruction>,
{
    fn new(instructions: I) -> Self {
        Self {
            instructions,
            current: None,
            cycle: 0,
            x: 1,
        }
    }
}

struct Cycle {
    cycle: usize,
    x: i32,
}

impl<'a, I> Iterator for Cpu<'a, I>
where
    I: Iterator<Item = &'a Instruction>,
{
    type Item = Cycle;

    fn next(&mut self) -> Option<Self::Item> {
        let current = Cycle {
            cycle: self.cycle,
            x: self.x,
        };
        self.cycle += 1;

        if let Some(Instruction::Addx(x)) = self.current.take() {
            self.x += x;
        } else {
            self.current = Some(self.instructions.next()?);
        }
        Some(current)
    }
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| {
            if line == "noop" {
                Instruction::Noop
            } else {
                Instruction::Addx(line[5..].parse().unwrap())
            }
        })
        .collect()
}

fn part1(input: &Input) -> i32 {
    Cpu::new(input.iter())
        .filter_map(|Cycle { cycle, x }| {
            if cycle % 40 == 19 {
                Some((cycle as i32 + 1) * x)
            } else {
                None
            }
        })
        .sum()
}

fn part2(input: &Input) -> String {
    let mut dots = [[false; 40]; 6];
    for Cycle { cycle, x } in Cpu::new(input.iter()) {
        if x.abs_diff(cycle as i32 % 40) <= 1 {
            dots[cycle / 40][cycle % 40] = true;
        }
    }
    parse_ascii(&dots.iter().map(|l| &l[..]).collect::<Vec<_>>())
}

#[cfg(test)]
mod test_cpu {
    #[allow(unused_imports)]
    use super::{Instruction::*, *};

    macro_rules! test {
        ($name:ident, $instructions:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let cpu = Cpu::new($instructions.iter());
                let cycles = cpu.map(|Cycle { cycle, x }| (cycle, x)).collect::<Vec<_>>();
                let expected = $expected.into_iter().enumerate().collect::<Vec<_>>();
                assert_eq!(cycles, expected);
            }
        };
    }

    test!(empty, [], []);
    test!(noop, [Noop], [1]);
    test!(noop3, [Noop, Noop, Noop], [1, 1, 1]);

    test!(add, [Addx(2)], [1, 1]);
    test!(add_noop, [Addx(2), Noop], [1, 1, 3]);
    test!(noop_add_noop, [Noop, Addx(2), Noop], [1, 1, 1, 3]);
    test!(
        noop_add_add_noop,
        [Noop, Addx(2), Addx(3), Noop],
        [1, 1, 1, 3, 3, 6]
    );
}

aoc::main!(2022, 10, ex: 1[a]);
