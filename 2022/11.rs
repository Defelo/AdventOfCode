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

fn simulate(monkeys: &Input, rounds: usize, div3: bool) -> u64 {
    let lcm = monkeys.iter().fold(1, |acc, monkey| acc.lcm(&monkey.test));
    let mut cnt = vec![0; monkeys.len()];
    let mut items: Vec<Vec<u64>> = monkeys
        .iter()
        .map(|monkey| monkey.starting.clone())
        .collect();
    for _ in 0..rounds {
        for i in 0..monkeys.len() {
            items.push(Vec::new());
            for item in items.swap_remove(i) {
                cnt[i] += 1;
                let mut j = monkeys[i].operation.apply(item);
                if div3 {
                    j /= 3;
                }
                j %= lcm;
                let new = if j % monkeys[i].test == 0 {
                    monkeys[i].true_idx
                } else {
                    monkeys[i].false_idx
                };
                items[new].push(j);
            }
        }
    }
    cnt.sort_unstable();
    cnt[monkeys.len() - 1] * cnt[monkeys.len() - 2]
}

fn part1(input: &Input) -> u64 {
    simulate(input, 20, true)
}

fn part2(input: &Input) -> u64 {
    simulate(input, 10000, false)
}

aoc::main!(2022, 11);
aoc::example!(ex01, "11.1.txt", 10605, 2713310158);
aoc::test_input!("11.txt", 54054, 14314925001);
