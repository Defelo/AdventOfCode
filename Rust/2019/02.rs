#![feature(test)]

use aoc::intcode::{Int, Vm};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

type Input = Vec<Int>;

fn setup(input: &str) -> Input {
    input
        .split(',')
        .map(|x| x.trim().parse().unwrap())
        .collect()
}

fn run(input: &Input, noun: Int, verb: Int) -> Int {
    let mut vm = Vm::new(input.iter().copied());
    vm.write(1, noun);
    vm.write(2, verb);
    vm.run().unwrap();
    vm.read(0)
}

fn part1(input: &Input) -> Int {
    run(input, 12, 2)
}

fn part2(input: &Input) -> Int {
    (0..100)
        .into_par_iter()
        .find_map_any(|noun| {
            (0..100)
                .find(|&verb| run(input, noun, verb) == 19690720)
                .map(|verb| 100 * noun + verb)
        })
        .unwrap()
}

aoc::main!(2019, 2);
