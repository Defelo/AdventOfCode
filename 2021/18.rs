#![feature(test)]

extern crate test;

use std::fs;
use test::Bencher;

type Number = [Option<u32>; 64];
type Input = Vec<Number>;

fn parent(n: usize) -> usize { n >> 1 }

fn left(n: usize) -> usize { n << 1 }

fn right(n: usize) -> usize { n << 1 | 1 }

fn get_input() -> Input {
    let puzzle = fs::read_to_string("2021/18.txt").unwrap();
    puzzle.trim().lines().map(|line| {
        let mut out = [Option::None; 64];
        let mut acc = Option::None;
        let mut n = 1;
        for c in line.trim().chars() {
            let digit = (c as i32) - 0x30;
            if digit >= 0 && digit <= 9 {
                acc = Option::Some(acc.unwrap_or(0) * 10 + (digit as u32));
                continue;
            }
            let mut write = |x| {
                if let Some(_) = acc { out[n] = acc; }
                acc = Option::None;
                x
            };
            n = match c {
                '[' => left(n),
                ',' => write(n + 1),
                ']' => write(parent(n)),
                _ => panic!()
            }
        }
        out
    }).collect()
}

fn move_up(lst: &mut Number, i: usize, x: u32) {
    match lst[i] {
        None => { move_up(lst, parent(i), x); }
        Some(y) => { lst[i] = Some(x + y); }
    }
}

fn explode(lst: &mut Number) -> bool {
    for n in (1 << 4)..(1 << 5) {
        if let Option::Some(_) = lst[n] { continue; }

        let a = left(n);
        let b = right(n);
        if let (Option::Some(l), Option::Some(r)) = (lst[a], lst[b]) {
            lst[a] = Option::None;
            lst[b] = Option::None;
            lst[n] = Option::Some(0);
            if a - 1 >= 1 << 5 {
                move_up(lst, a - 1, l);
            }
            if b + 1 < 1 << 6 {
                move_up(lst, b + 1, r);
            }
            return true;
        }
    }

    false
}

fn split(lst: &mut Number, n: usize) -> bool {
    if let Option::Some(x) = lst[n] {
        if x < 10 { return false; }
        let k = x >> 1;
        lst[n] = Option::None;
        lst[left(n)] = Option::Some(k);
        lst[right(n)] = Option::Some(x - k);
        return true;
    }
    split(lst, left(n)) || split(lst, right(n))
}

fn add(a: &Number, b: &Number) -> Number {
    let mut out = [Option::None; 64];
    let mut j = 2;
    for i in 0..=4 {
        for k in (1 << i)..(1 << i + 1) {
            out[j] = a[k];
            j += 1;
        }
        for k in (1 << i)..(1 << i + 1) {
            out[j] = b[k];
            j += 1;
        }
    }

    while explode(&mut out) || split(&mut out, 1) {};

    out
}

fn magnitude(lst: &Number, n: usize) -> u32 {
    if let Some(x) = lst[n] { x }
    else { 3 * magnitude(lst, left(n)) + 2 * magnitude(lst, right(n)) }
}

fn part1(input: &Input) -> String {
    magnitude(&input.iter().cloned().reduce(|a, b| { add(&a, &b) }).unwrap(), 1).to_string()
}

fn part2(input: &Input) -> String {
    input.iter().flat_map(|a| {
        input.iter().filter_map(move |b| {
            if a == b { Option::None } else { Option::Some(magnitude(&add(a, b), 1)) }
        })
    }).max().unwrap().to_string()
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
