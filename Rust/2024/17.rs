#![feature(test)]

use std::fmt::Write;

struct Input {
    registers: [u64; 3],
    program: Vec<u64>,
}

fn setup(input: &str) -> Input {
    let mut blocks = input.trim().split("\n\n");
    let registers = blocks
        .next()
        .unwrap()
        .lines()
        .map(|l| l.rsplit_once(' ').unwrap().1.parse().unwrap())
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

    let program = blocks
        .next()
        .unwrap()
        .split(' ')
        .nth(1)
        .unwrap()
        .split(',')
        .map(|n| n.parse().unwrap())
        .collect();

    Input { registers, program }
}

fn run(mut reg: [u64; 3], program: &[u64]) -> Vec<u64> {
    let mut out = Vec::new();
    let mut pc = 0;
    while pc < program.len() {
        let inst = program[pc];
        let arg = program[pc + 1];
        let combo = || match arg {
            0..=3 => arg,
            4..=6 => reg[arg as usize - 4],
            _ => panic!("invalid combo arg: {arg}"),
        };
        match inst {
            0 => reg[0] >>= combo(),
            1 => reg[1] ^= arg,
            2 => reg[1] = combo() & 0b111,
            3 if reg[0] == 0 => {}
            3 => {
                pc = arg as usize;
                continue;
            }
            4 => reg[1] ^= reg[2],
            5 => out.push(combo() & 0b111),
            6 => reg[1] = reg[0] >> combo(),
            7 => reg[2] = reg[0] >> combo(),
            8.. => panic!("Invalid instruction: {inst}"),
        }
        pc += 2;
    }
    out
}

fn part1(input: &Input) -> String {
    let mut out = String::new();
    for n in run(input.registers, &input.program) {
        write!(&mut out, "{n},").unwrap();
    }
    out.truncate(out.len() - (out.ends_with(',') as usize));
    out
}

fn part2(input: &Input) -> u64 {
    let mut stack = vec![(0, 0)];
    let mut result = u64::MAX;
    while let Some((a, i)) = stack.pop() {
        if i == input.program.len() {
            result = result.min(a);
            continue;
        }
        for j in 0..1 << 3 {
            let a = a << 3 | j;
            let mut registers = input.registers;
            registers[0] = a;
            if run(registers, &input.program) == input.program[input.program.len() - i - 1..] {
                stack.push((a, i + 1));
            }
        }
    }
    result
}

aoc::main!(2024, 17, ex: 1, 2);
