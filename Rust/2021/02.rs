#![feature(test)]

enum Command {
    Forward(i32),
    Down(i32),
    Up(i32),
}

type Input = Vec<Command>;

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            let mut x = line.split(' ');
            let cmd = x.next().unwrap();
            let n: i32 = x.next().unwrap().parse().unwrap();
            match cmd {
                "forward" => Command::Forward(n),
                "up" => Command::Up(n),
                "down" => Command::Down(n),
                _ => panic!(),
            }
        })
        .collect()
}

fn part1(input: &Input) -> String {
    let mut d = 0;
    let mut h = 0;
    for cmd in input {
        match cmd {
            Command::Forward(n) => h += n,
            Command::Down(n) => d += n,
            Command::Up(n) => d -= n,
        }
    }
    (d * h).to_string()
}

fn part2(input: &Input) -> String {
    let mut d = 0;
    let mut h = 0;
    let mut a = 0;
    for cmd in input {
        match cmd {
            Command::Forward(n) => {
                h += n;
                d += a * n;
            }
            Command::Down(n) => a += n,
            Command::Up(n) => a -= n,
        }
    }
    (d * h).to_string()
}

aoc::main!(2021, 2, ex: 1);
