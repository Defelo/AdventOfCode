#![feature(test)]

#[derive(Debug)]
struct Input {
    races: Vec<Race>,
    part2: Race,
}

#[derive(Debug, Clone, Copy)]
struct Race {
    time: u64,
    distance: u64,
}

fn setup(input: &str) -> Input {
    let mut lines = input.lines();
    let mut parse_line = || {
        lines
            .next()
            .unwrap()
            .split_whitespace()
            .skip(1)
            .map(|x| x.parse().unwrap())
    };
    let races = parse_line()
        .zip(parse_line())
        .map(|(t, d)| Race {
            time: t,
            distance: d,
        })
        .collect();

    let mut lines = input.lines();
    let mut parse_line = || {
        lines
            .next()
            .unwrap()
            .split(':')
            .nth(1)
            .unwrap()
            .replace(' ', "")
            .parse()
            .unwrap()
    };
    let part2 = Race {
        time: parse_line(),
        distance: parse_line(),
    };

    Input { races, part2 }
}

fn solve(Race { time, distance }: Race) -> u64 {
    let Some(x) = (time * time).checked_sub(distance * 4) else {
        return 0;
    };
    let root = x.isqrt();
    let root_is_int = (root * root == x) as u64;
    root + (time + root + 1 + root_is_int) % 2 - root_is_int
}

fn part1(input: &Input) -> u64 {
    input
        .races
        .iter()
        .map(|&race| solve(race))
        .filter(|&x| x > 0)
        .product()
}

fn part2(input: &Input) -> u64 {
    solve(input.part2)
}

aoc::main!(2023, 6, ex: 1);

#[cfg(test)]
mod tests {
    use proptest::{prop_assert_eq, proptest};

    use super::*;

    proptest! {
        #[test]
        fn proptest_solve(t in (0u64..=200000), d in (0u64..=1000000)) {
            let race = Race { time: t, distance: d };
            let expected = (0..=t).filter(|i| i * (t - i) > d).count() as u64;

            let result = solve(race);

            prop_assert_eq!(result, expected);
        }
    }
}
