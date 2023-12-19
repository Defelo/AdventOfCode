#![feature(test)]

use aoc::{
    arrays::ArrayExt,
    tuples::{HomogeneousTupleExt, TupleExt},
};
use itertools::Itertools;
use regex::Regex;
use rustc_hash::FxHashMap;

type Range = aoc::range::Range<u64>;
type Part<T> = [T; 4]; // x, m, a, s
type Variable = usize; // 0..4

#[derive(Debug)]
struct Input {
    workflows: Vec<Workflow>,
    in_workflow: usize,
    parts: Vec<Part<u64>>,
}

#[derive(Debug)]
struct Workflow {
    rules: Vec<Rule>,
}

#[derive(Debug, Clone, Copy)]
struct Rule {
    ty: RuleType,
    action: Action,
}

#[derive(Debug, Clone, Copy)]
enum RuleType {
    Comparison {
        variable: Variable,
        condition: Condition,
        num: u64,
    },
    Default,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Condition {
    LessThan,
    GreaterThan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Action {
    Accept,
    Reject,
    Goto(usize),
}

impl Action {
    fn from(workflow_ids: &FxHashMap<&str, usize>, action: &str) -> Self {
        match action {
            "A" => Self::Accept,
            "R" => Self::Reject,
            workflow => Self::Goto(workflow_ids[workflow]),
        }
    }
}

fn setup(input: &str) -> Input {
    let mut input = input.split("\n\n").peekable();

    let workflow_ids = input
        .peek()
        .unwrap()
        .lines()
        .enumerate()
        .map(|(i, line)| (line.split('{').next().unwrap(), i))
        .collect::<FxHashMap<_, _>>();

    let regex = Regex::new(r"^[a-z]+\{(.+,)([a-zAR]+)\}$").unwrap();
    let rules_regex = Regex::new(r"([xmas])(<|>)(\d+):([a-zAR]+),").unwrap();
    let workflows = input
        .next()
        .unwrap()
        .lines()
        .map(|line| {
            let cap = regex.captures(line).unwrap();
            let rules = rules_regex
                .captures_iter(&cap[1])
                .map(|cap| {
                    let variable = match &cap[1] {
                        "x" => 0,
                        "m" => 1,
                        "a" => 2,
                        "s" => 3,
                        _ => unreachable!(),
                    };
                    let num = cap[3].parse().unwrap();
                    let condition = match &cap[2] {
                        "<" => Condition::LessThan,
                        ">" => Condition::GreaterThan,
                        _ => unreachable!(),
                    };
                    let action = Action::from(&workflow_ids, &cap[4]);

                    Rule {
                        ty: RuleType::Comparison {
                            variable,
                            condition,
                            num,
                        },
                        action,
                    }
                })
                .chain([Rule {
                    ty: RuleType::Default,
                    action: Action::from(&workflow_ids, &cap[2]),
                }])
                .collect();

            Workflow { rules }
        })
        .collect();

    let regex = Regex::new(r"\d+").unwrap();
    let parts = input
        .next()
        .unwrap()
        .lines()
        .map(|line| {
            regex
                .find_iter(line)
                .map(|n| n.as_str().parse().unwrap())
                .collect_vec()
                .try_into()
                .unwrap()
        })
        .collect();

    Input {
        workflows,
        in_workflow: workflow_ids["in"],
        parts,
    }
}

fn split_part_range(
    workflow: &Workflow,
    part_range: Part<Range>,
) -> impl Iterator<Item = (Action, Part<Range>)> + '_ {
    workflow
        .rules
        .iter()
        .scan(Some(part_range), |part_range, rule| {
            let unmatched = *part_range.as_ref()?;

            let (matching, remaining) = match rule.ty {
                RuleType::Comparison {
                    variable,
                    condition,
                    num,
                } => match condition {
                    Condition::LessThan => unmatched[variable].split_at(num),
                    Condition::GreaterThan => unmatched[variable].split_at(num + 1).swap(),
                }
                .bimap(|opt_split| opt_split.map(|split| unmatched.update(variable, split))),
                RuleType::Default => (Some(unmatched), None),
            };

            *part_range = remaining;

            Some(matching.map(|split| (rule.action, split)))
        })
        .flatten()
}

fn filter(
    workflows: &[Workflow],
    in_workflow: usize,
    ranges: impl IntoIterator<Item = Part<Range>>,
) -> Vec<Part<Range>> {
    let mut out = vec![];
    let mut queue = ranges
        .into_iter()
        .map(|range| (Action::Goto(in_workflow), range))
        .collect_vec();
    while let Some((action, range)) = queue.pop() {
        match action {
            Action::Accept => out.push(range),
            Action::Reject => {}
            Action::Goto(id) => {
                queue.extend(split_part_range(&workflows[id], range));
            }
        }
    }
    out
}

fn part1(input: &Input) -> u64 {
    filter(
        &input.workflows,
        input.in_workflow,
        input.parts.iter().map(|&r| r.map(|a| (a..a + 1).into())),
    )
    .into_iter()
    .flat_map(|r| r.into_iter().map(|r| r.start))
    .sum()
}

fn part2(input: &Input) -> u64 {
    filter(
        &input.workflows,
        input.in_workflow,
        [[Range::from(1..4001); 4]],
    )
    .into_iter()
    .map(|r| r.into_iter().map(|a| a.end - a.start).product::<u64>())
    .sum()
}

aoc::main!(2023, 19, ex: 1);
