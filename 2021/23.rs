#![feature(test)]
#![feature(int_abs_diff)]

extern crate test;

use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::fs;
use test::Bencher;

use rustc_hash::FxHashSet;

type Input = [Vec<char>; 4];

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/23.txt").unwrap();
    let mut out = [vec![], vec![], vec![], vec![]];
    let lines: Vec<(char, char, char, char)> = puzzle.lines().skip(2).take(2).map(|line| {
        let get = |i| line.chars().nth(3 + 2 * i as usize).unwrap();
        (get(0), get(1), get(2), get(3))
    }).collect();
    for line in lines.iter().rev() {
        out[0].push(line.0);
        out[1].push(line.1);
        out[2].push(line.2);
        out[3].push(line.3);
    }
    out
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone)]
struct State {
    energy: u32,
    rooms: [Vec<char>; 4],
    hallway: [char; 11],
}

impl State {
    fn make_key(&self) -> ([Vec<char>; 4], [char; 11]) {
        (self.rooms.clone(), self.hallway.clone())
    }

    fn check_room(&self, idx: usize) -> bool {
        let c = "ABCD".chars().nth(idx).unwrap();
        self.rooms[idx].iter().all(|&x| x == c)
    }

    fn done(&self) -> bool {
        self.hallway.iter().all(|&x| x == '.') && (0..4).all(|i| { self.check_room(i) })
    }

    fn check_hallway(&self, start: usize, end: usize) -> bool {
        (start.min(end)..=end.max(start)).all(|i| { self.hallway[i] == '.' || i == start })
    }

    fn add_cost(mut self, cost: u32) -> Self {
        self.energy += cost;
        self
    }

    fn push_room(mut self, idx: usize, c: char) -> Self {
        self.rooms[idx].push(c);
        self
    }

    fn pop_room(mut self, idx: usize) -> Self {
        self.rooms[idx].pop();
        self
    }

    fn set_hallway(mut self, idx: usize, c: char) -> Self {
        self.hallway[idx] = c;
        self
    }

    fn generate_moves(&self, n: usize) -> Vec<State> {
        for (i, &c) in self.hallway.iter().enumerate() {
            if c == '.' { continue; }

            let dst = "ABCD".find(c).unwrap();
            if self.rooms[dst].iter().any(|&x| x != c) { continue; }
            if !self.check_hallway(i, 2 + 2 * dst) { continue; }

            let dist = i.abs_diff(2 + 2 * dst) + n - self.rooms[dst].len();
            return vec![self.clone()
                .add_cost(dist as u32 * 10u32.pow(dst as u32))
                .push_room(dst, c)
                .set_hallway(i, '.')];
        }

        let mut out = vec![];
        for i in 0..4 {
            if self.check_room(i) { continue; }

            let c = *self.rooms[i].last().unwrap();
            let src = 2 + 2 * i;
            let dst = "ABCD".find(c).unwrap();
            for j in [0, 1, 3, 5, 7, 9, 10] {
                if !self.check_hallway(src, j) {continue;}
                let dist = (1 + n - self.rooms[i].len()) + src.abs_diff(j);
                out.push(self.clone()
                    .add_cost(dist as u32 * 10u32.pow(dst as u32))
                    .pop_room(i)
                    .set_hallway(j, c))
            }
        }
        out
    }
}

fn solve(input: &Input, part2: bool) -> u32 {
    let mut input = input.clone();
    if part2 {
        for i in 0..4 {
            input[i].insert(1, "DBAC".chars().nth(i).unwrap());
            input[i].insert(2, "DCBA".chars().nth(i).unwrap());
        }
    }
    let n = input[0].len();
    let mut queue = BinaryHeap::new();
    queue.push(Reverse(State {
        energy: 0,
        rooms: input,
        hallway: ['.'; 11],
    }));
    let mut visited = FxHashSet::default();
    while !queue.is_empty() {
        let state = queue.pop().unwrap().0;

        let key = state.make_key();
        if visited.contains(&key) { continue; }
        visited.insert(key);

        if state.done() { return state.energy; }

        for next in state.generate_moves(n) {
            queue.push(Reverse(next));
        }
    }
    panic!();
}

fn part1(input: &Input) -> String {
    solve(input, false).to_string()
}

fn part2(input: &Input) -> String {
    solve(input, true).to_string()
}

fn main() {
    let (part1, part2) = run();
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}

pub fn run() -> (String, String) {
    let input = get_input();
    (part1(&input), part2(&input))
}

#[bench]
fn bench_part1(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| part1(&input))
}

#[bench]
fn bench_part2(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| part2(&input))
}
