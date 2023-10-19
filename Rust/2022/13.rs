#![feature(test)]

use std::cmp::Ordering;

type Input = Vec<(Value, Value)>;

#[derive(PartialEq, Eq)]
enum Value {
    Int(u32),
    List(Vec<Value>),
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::Int(a), Value::List(_)) => Value::List(vec![Value::Int(*a)]).cmp(other),
            (Value::List(_), Value::Int(b)) => self.cmp(&Value::List(vec![Value::Int(*b)])),
            (Value::List(a), Value::List(b)) => a
                .iter()
                .zip(b)
                .map(|(x, y)| x.cmp(y))
                .find(|x| x != &Ordering::Equal)
                .unwrap_or_else(|| a.len().cmp(&b.len())),
        }
    }
}

fn parse_recursive(data: &str) -> Value {
    let mut current_number = None;
    let mut stack: Vec<Vec<Value>> = vec![vec![]];
    for c in data.chars() {
        match c {
            '[' => stack.push(vec![]),
            ']' => {
                let mut current = stack.pop().unwrap();
                if let Some(num) = current_number.take() {
                    current.push(Value::Int(num));
                }
                stack.last_mut().unwrap().push(Value::List(current));
            }
            '0'..='9' => {
                current_number = Some(current_number.unwrap_or(0) * 10 + (c as u8 - b'0') as u32)
            }
            ',' => {
                if let Some(num) = current_number.take() {
                    stack.last_mut().unwrap().push(Value::Int(num));
                }
            }
            _ => {}
        }
    }
    stack.pop().unwrap().pop().unwrap()
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .split("\n\n")
        .map(|pair| {
            let mut it = pair.lines();
            (
                parse_recursive(it.next().unwrap()),
                parse_recursive(it.next().unwrap()),
            )
        })
        .collect()
}

fn part1(input: &Input) -> usize {
    input
        .iter()
        .enumerate()
        .map(|(i, (a, b))| if a < b { i + 1 } else { 0 })
        .sum()
}

fn part2(input: &Input) -> usize {
    let fst = Value::List(vec![Value::List(vec![Value::Int(2)])]);
    let snd = Value::List(vec![Value::List(vec![Value::Int(6)])]);
    let idx = |value| {
        input
            .iter()
            .flat_map(|(a, b)| vec![a, b])
            .chain(std::iter::once(&fst))
            .chain(std::iter::once(&snd))
            .filter(|v| *v < value)
            .count()
            + 1
    };
    idx(&fst) * idx(&snd)
}

aoc::main!(2022, 13, ex: 1);
