type Input = Vec<usize>;

fn setup(input: &str) -> Input {
    input.trim().bytes().map(|x| (x - b'a') as _).collect()
}

#[derive(Default)]
struct Counter {
    counter: [usize; 26],
    dup: usize,
}

impl Counter {
    pub fn inc(&mut self, i: usize) {
        self.counter[i] += 1;
        if self.counter[i] == 2 {
            self.dup += 1;
        }
    }

    pub fn dec(&mut self, i: usize) {
        self.counter[i] -= 1;
        if self.counter[i] == 1 {
            self.dup -= 1;
        }
    }

    pub fn is_distinct(&self) -> bool {
        self.dup == 0
    }
}

fn solve(input: &Input, n: usize) -> usize {
    let mut counter = Counter::default();
    (0..n).for_each(|i| counter.inc(input[i]));
    for i in n..input.len() {
        if counter.is_distinct() {
            return i;
        }
        counter.inc(input[i]);
        counter.dec(input[i - n]);
    }
    panic!()
}

fn part1(input: &Input) -> usize {
    solve(input, 4)
}

fn part2(input: &Input) -> usize {
    solve(input, 14)
}

aoc::main!(2022, 6);
aoc::example!(ex01, "06.1.txt", 7, 19);
aoc::example!(ex02, "06.2.txt", 5, 23);
aoc::example!(ex03, "06.3.txt", 6, 23);
aoc::example!(ex04, "06.4.txt", 10, 29);
aoc::example!(ex05, "06.5.txt", 11, 26);
aoc::test_input!("06.txt", 1802, 3551);
