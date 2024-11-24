#![feature(test)]

use rustc_hash::FxHashMap;

#[derive(Debug)]
struct Input {
    wires: FxHashMap<String, Expr>,
}

#[derive(Debug)]
enum Expr {
    Const(NameOrLiteral),
    Unary(Unary, NameOrLiteral),
    Binary(Binary, NameOrLiteral, NameOrLiteral),
}

#[derive(Debug)]
enum NameOrLiteral {
    Name(String),
    Literal(u16),
}

#[derive(Debug, Clone, Copy)]
enum Unary {
    Not,
}

#[derive(Debug, Clone, Copy)]
enum Binary {
    And,
    Or,
    Lshift,
    Rshift,
}

impl Expr {
    fn from(s: &str) -> Option<Self> {
        let mut expr = s.split_whitespace();
        let expr0 = expr.next()?;
        let expr1 = expr.next();
        let expr2 = expr.next();
        match (expr0, expr1, expr2) {
            (c, None, None) => Some(Self::Const(NameOrLiteral::from(c))),
            (op, Some(arg), None) => Some(Self::Unary(Unary::from(op)?, NameOrLiteral::from(arg))),
            (arg1, Some(op), Some(arg2)) => Some(Self::Binary(
                Binary::from(op)?,
                NameOrLiteral::from(arg1),
                NameOrLiteral::from(arg2),
            )),
            (_, None, Some(_)) => unreachable!(),
        }
    }
}

impl NameOrLiteral {
    fn from(s: &str) -> Self {
        match s.parse() {
            Ok(lit) => Self::Literal(lit),
            Err(_) => Self::Name(s.into()),
        }
    }
}

impl Unary {
    fn from(s: &str) -> Option<Self> {
        match s {
            "NOT" => Some(Self::Not),
            _ => None,
        }
    }

    fn eval(self, arg: u16) -> u16 {
        match self {
            Self::Not => !arg,
        }
    }
}

impl Binary {
    fn from(s: &str) -> Option<Self> {
        match s {
            "AND" => Some(Self::And),
            "OR" => Some(Self::Or),
            "LSHIFT" => Some(Self::Lshift),
            "RSHIFT" => Some(Self::Rshift),
            _ => None,
        }
    }

    fn eval(self, arg1: u16, arg2: u16) -> u16 {
        match self {
            Self::And => arg1 & arg2,
            Self::Or => arg1 | arg2,
            Self::Lshift => arg1 << arg2,
            Self::Rshift => arg1 >> arg2,
        }
    }
}

fn setup(input: &str) -> Input {
    let wires = input
        .trim()
        .lines()
        .map(|line| {
            let mut split = line.split(" -> ");
            let expr = split.next().unwrap();
            let name = split.next().unwrap();
            (name.into(), Expr::from(expr).unwrap())
        })
        .collect();

    Input { wires }
}

fn simulate<'a>(input: &'a Input, mut values: FxHashMap<&'a str, u16>) -> u16 {
    fn simulate_inner<'a>(
        name: &'a str,
        values: &mut FxHashMap<&'a str, u16>,
        input: &'a Input,
    ) -> u16 {
        if let Some(&value) = values.get(name) {
            return value;
        }

        let mut resolve = |x: &'a NameOrLiteral| match x {
            NameOrLiteral::Name(name) => simulate_inner(name, values, input),
            &NameOrLiteral::Literal(value) => value,
        };

        let value = match input.wires.get(name).unwrap() {
            Expr::Const(c) => resolve(c),
            Expr::Unary(op, arg) => op.eval(resolve(arg)),
            Expr::Binary(op, arg1, arg2) => op.eval(resolve(arg1), resolve(arg2)),
        };

        values.insert(name, value);

        value
    }

    simulate_inner("a", &mut values, input)
}

fn part1(input: &Input) -> u16 {
    simulate(input, Default::default())
}

fn part2(input: &Input) -> u16 {
    let a = simulate(input, Default::default());
    simulate(input, [("b", a)].into_iter().collect())
}

aoc::main!(2015, 7);
