#![feature(test)]

type Input = Vec<i32>;

fn setup(input: &str) -> Input {
    input
        .trim()
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect()
}

fn solve(input: &Input, n: u32) -> String {
    let mut counter = vec![0; 9];
    for num in input {
        counter[*num as usize] += 1;
    }
    for _ in 0..n {
        let x = counter.remove(0);
        counter.push(x);
        counter[6] += x;
    }
    counter.iter().sum::<u64>().to_string()
}

fn part1(input: &Input) -> String {
    solve(input, 80)
}

fn part2(input: &Input) -> String {
    solve(input, 256)
}

aoc::main!(2021, 6, ex: 1);
