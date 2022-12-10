use aoc::grid::Direction;
use rustc_hash::FxHashSet;

type Input = Vec<Motion>;

#[derive(Debug)]
struct Motion {
    direction: Direction,
    count: u32,
}

struct Solver<'a> {
    motions: &'a [Motion],
    knots: Vec<(isize, isize)>,
    visited: FxHashSet<(isize, isize)>,
}

impl<'a> Solver<'a> {
    fn new(input: &'a Input, n: usize) -> Self {
        Self {
            motions: input,
            knots: vec![(0, 0); n],
            visited: FxHashSet::default(),
        }
    }

    fn follow(&mut self, i: usize) {
        let p = self.knots[i - 1].0 - self.knots[i].0;
        let q = self.knots[i - 1].1 - self.knots[i].1;
        if p.abs() > 1 || q.abs() > 1 {
            self.knots[i] = (
                self.knots[i].0 + p.max(-1).min(1),
                self.knots[i].1 + q.max(-1).min(1),
            );
        }
    }

    fn solve(mut self) -> usize {
        for Motion { direction, count } in self.motions {
            for _ in 0..*count {
                self.knots[0] = direction.step_signed(self.knots[0]);
                (1..self.knots.len()).for_each(|i| self.follow(i));
                self.visited.insert(*self.knots.last().unwrap());
            }
        }
        self.visited.len()
    }
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| Motion {
            direction: line.chars().next().unwrap().into(),
            count: line[2..].parse().unwrap(),
        })
        .collect()
}

fn part1(input: &Input) -> usize {
    Solver::new(input, 2).solve()
}

fn part2(input: &Input) -> usize {
    Solver::new(input, 10).solve()
}

aoc::main!(2022, 9);
aoc::example!(ex01, "09.1.txt", 13, 1);
aoc::example!(ex02, "09.2.txt", 88, 36);
aoc::test_input!("09.txt", 6030, 2545);
