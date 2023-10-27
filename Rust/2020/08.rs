#![feature(test)]

type Input = Vec<Instruction>;

#[derive(Debug, Clone, Copy)]
struct Instruction {
    operation: Operation,
    argument: i32,
}

#[derive(Debug, Copy, Clone)]
enum Operation {
    Nop,
    Acc,
    Jmp,
}

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            let mut line = line.split_whitespace();
            let operation = match line.next().unwrap() {
                "nop" => Operation::Nop,
                "acc" => Operation::Acc,
                "jmp" => Operation::Jmp,
                _ => panic!(),
            };
            let argument = line.next().unwrap().parse().unwrap();
            Instruction {
                operation,
                argument,
            }
        })
        .collect()
}

fn part1(input: &Input) -> i32 {
    let mut exec = vec![false; input.len()];
    let mut i = 0;
    let mut acc = 0;
    while !exec[i] {
        exec[i] = true;
        match input[i].operation {
            Operation::Nop => i += 1,
            Operation::Acc => {
                acc += input[i].argument;
                i += 1;
            }
            Operation::Jmp => i = (i as i32 + input[i].argument) as _,
        }
    }
    acc
}

fn simulate(input: &Input) -> Option<i32> {
    let mut exec = vec![false; input.len()];
    let mut i = 0;
    let mut acc = 0;
    loop {
        if i == input.len() {
            return Some(acc);
        }
        if i > input.len() || exec[i] {
            return None;
        }

        exec[i] = true;
        match input[i].operation {
            Operation::Nop => i += 1,
            Operation::Acc => {
                acc += input[i].argument;
                i += 1;
            }
            Operation::Jmp => i = (i as i32 + input[i].argument).try_into().ok()?,
        }
    }
}

fn part2(input: &Input) -> i32 {
    let mut prog = input.clone();
    for i in 0..prog.len() {
        prog[i].operation = match prog[i].operation {
            Operation::Nop => Operation::Jmp,
            Operation::Acc => continue,
            Operation::Jmp => Operation::Nop,
        };
        if let Some(acc) = simulate(&prog) {
            return acc;
        }
        prog[i].operation = input[i].operation;
    }
    panic!()
}

aoc::main!(2020, 8, ex: 1);
