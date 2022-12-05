type Input = (Vec<String>, Vec<(usize, usize, usize)>);

struct Solver<'input> {
    stacks: &'input Vec<String>,
    instructions: &'input Vec<(usize, usize, usize)>,
    get_next: fn(cnt: usize, h: usize) -> usize,
}

impl<'input> Solver<'input> {
    pub fn new(
        (stacks, instructions): &'input Input,
        get_next: fn(cnt: usize, h: usize) -> usize,
    ) -> Self {
        Self {
            stacks,
            instructions,
            get_next,
        }
    }

    fn get_crate(&self, ins: usize, st: usize, h: usize) -> char {
        if ins == 0 {
            return self.stacks[st].chars().nth_back(h).unwrap();
        }
        let (cnt, i, j) = self.instructions[ins - 1];
        if st == j {
            if h < cnt {
                self.get_crate(ins - 1, i, (self.get_next)(cnt, h))
            } else {
                self.get_crate(ins - 1, st, h - cnt)
            }
        } else if st == i {
            self.get_crate(ins - 1, st, h + cnt)
        } else {
            self.get_crate(ins - 1, st, h)
        }
    }

    pub fn solve(self) -> String {
        let n = self.instructions.len();
        (0..self.stacks.len())
            .map(|i| self.get_crate(n, i, 0))
            .collect()
    }
}

fn setup(input: &str) -> Input {
    let (initial, instructions) = input.split_once("\n\n").unwrap();
    let mut it = initial.lines().rev();
    let mut stacks: Vec<_> = it
        .next()
        .unwrap()
        .split_whitespace()
        .map(|_| String::new())
        .collect();
    for line in it {
        for (i, c) in line
            .chars()
            .skip(1)
            .step_by(4)
            .enumerate()
            .filter(|(_, x)| *x != ' ')
        {
            stacks[i].push(c);
        }
    }
    let instructions = instructions
        .lines()
        .map(|line| {
            let mut it = line
                .split_whitespace()
                .skip(1)
                .step_by(2)
                .map(|x| x.parse().unwrap());
            (|| Some((it.next()?, it.next()? - 1, it.next()? - 1)))().unwrap()
        })
        .collect();
    (stacks, instructions)
}

fn part1(input: &Input) -> String {
    Solver::new(input, |cnt, h| cnt - h - 1).solve()
}

fn part2(input: &Input) -> String {
    Solver::new(input, |_, h| h).solve()
}

aoc::main!(2022, 5);
aoc::example!(ex01, "05.1.txt", "CMZ", "MCD");
