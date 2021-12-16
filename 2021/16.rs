#![feature(test)]

extern crate test;

use std::fs;
use test::Bencher;

type Input = Vec<bool>;

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/16.txt").unwrap();
    puzzle.trim().chars().flat_map(|c| {
        let n = "0123456789ABCDEF".find(c).unwrap();
        (0..4).rev().map(move |i| n & (1 << i) > 0)
    }).collect()
}

struct Reader {
    data: Vec<bool>,
    pos: usize,
}

impl Reader {
    fn new(data: Vec<bool>) -> Reader {
        Reader { data, pos: 0 }
    }

    fn read(&mut self, n: usize) -> u64 {
        let mut out = 0;
        for _ in 0..n {
            if self.pos >= self.data.len() { break; }
            out <<= 1;
            if self.data[self.pos] { out |= 1; }
            self.pos += 1;
        }
        out
    }
}

struct Result {
    version_sum: u64,
    value: u64,
}

fn solve(io: &mut Reader) -> Result {
    let mut version = io.read(3);
    let tid = io.read(3);
    if tid == 4 {
        let mut lit = 0;
        loop {
            let c = io.read(1);
            lit <<= 4;
            lit |= io.read(4);
            if c == 0 { break; }
        }
        return Result { version_sum: version, value: lit };
    }

    let mut values = vec![];
    if io.read(1) == 0 {
        let c = io.read(15) as usize + io.pos;
        while io.pos < c {
            let result = solve(io);
            version += result.version_sum;
            values.push(result.value);
        }
    } else {
        for _ in 0..io.read(11) {
            let result = solve(io);
            version += result.version_sum;
            values.push(result.value);
        }
    }

    Result {
        version_sum: version,
        value: match tid {
            0 => values.iter().cloned().sum(),
            1 => values.iter().cloned().product(),
            2 => values.iter().cloned().min().unwrap(),
            3 => values.iter().cloned().max().unwrap(),
            5 => (values[0] > values[1]) as u64,
            6 => (values[0] < values[1]) as u64,
            7 => (values[0] == values[1]) as u64,
            _ => panic!()
        },
    }
}

pub fn main() {
    let input = get_input();
    let result = solve(&mut Reader::new(input.clone()));
    println!("Part 1: {}", result.version_sum);
    println!("Part 2: {}", result.value);
}

#[bench]
fn bench_solution(b: &mut Bencher) {
    let input = get_input();
    b.iter(|| solve(&mut Reader::new(input.clone())))
}
