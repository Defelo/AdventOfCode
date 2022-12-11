use itertools::Itertools;
use num::Integer;

type Input = Vec<Monkey>;

#[derive(Debug)]
struct Monkey {
    starting: Vec<u64>,
    operation: Operation,
    test: u64,
    true_idx: usize,
    false_idx: usize,
}

#[derive(Debug)]
enum Operation {
    Add(Arg),
    Mul(Arg),
}

impl Operation {
    fn apply(&self, old: u64) -> u64 {
        match self {
            Operation::Add(x) => old + x.get(old),
            Operation::Mul(x) => old * x.get(old),
        }
    }
}

#[derive(Debug)]
enum Arg {
    Old,
    Lit(u64),
}

impl Arg {
    fn get(&self, old: u64) -> u64 {
        match self {
            Arg::Old => old,
            &Arg::Lit(x) => x,
        }
    }
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .split("\n\n")
        .map(|monkey| -> Option<Monkey> {
            let mut lines = monkey.lines().skip(1);
            let starting = lines
                .next()?
                .split(':')
                .nth(1)?
                .trim()
                .split(',')
                .map(|i| i.trim().parse().ok())
                .collect::<Option<_>>()?;
            let (arg, op) = lines.next()?.rsplit(' ').take(2).collect_tuple()?;
            let arg = match arg.parse() {
                Ok(n) => Arg::Lit(n),
                Err(_) => Arg::Old,
            };
            let operation = match op {
                "+" => Operation::Add(arg),
                "*" => Operation::Mul(arg),
                _ => panic!(),
            };
            let test = lines.next()?.rsplit(' ').next()?.parse().ok()?;
            let true_idx = lines.next()?.rsplit(' ').next()?.parse().ok()?;
            let false_idx = lines.next()?.rsplit(' ').next()?.parse().ok()?;
            Some(Monkey {
                starting,
                operation,
                test,
                true_idx,
                false_idx,
            })
        })
        .collect::<Option<_>>()
        .unwrap()
}

struct Solver<'a> {
    monkeys: &'a Input,
    rounds: usize,
    div3: bool,
    cnt: Vec<u64>,
    modulus: u64,
}

impl<'a> Solver<'a> {
    fn new(monkeys: &'a Input, rounds: usize, div3: bool) -> Self {
        Self {
            monkeys,
            rounds,
            div3,
            cnt: vec![0; monkeys.len()],
            modulus: monkeys.iter().fold(1, |acc, monkey| acc.lcm(&monkey.test)),
        }
    }

    fn simulate_item(&mut self, mut monkey: usize, mut item: u64) {
        for _ in 0..self.rounds {
            let mut last = monkey;
            while last <= monkey {
                last = monkey;
                self.cnt[monkey] += 1;
                item = self.monkeys[monkey].operation.apply(item);
                if self.div3 {
                    item /= 3;
                }
                item %= self.modulus;
                monkey = if item % self.monkeys[monkey].test == 0 {
                    self.monkeys[monkey].true_idx
                } else {
                    self.monkeys[monkey].false_idx
                };
            }
        }
    }

    fn solve(mut self) -> u64 {
        for (m, monkey) in self.monkeys.iter().enumerate() {
            for &item in &monkey.starting {
                self.simulate_item(m, item);
            }
        }

        let (a, b) = self.cnt.iter().fold((0, 0), |(a, b), &x| {
            if x > a {
                (x, a)
            } else if x > b {
                (a, x)
            } else {
                (a, b)
            }
        });
        a * b
    }
}

fn part1(input: &Input) -> u64 {
    Solver::new(input, 20, true).solve()
}

fn part2(input: &Input) -> u64 {
    Solver::new(input, 10000, false).solve()
}

aoc::main!(2022, 11);
aoc::example!(ex01, "11.1.txt", 10605, 2713310158);
aoc::test_input!("11.txt", 54054, 14314925001);
