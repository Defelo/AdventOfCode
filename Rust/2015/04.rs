#![feature(test)]

use std::{
    sync::{
        atomic::{self, AtomicUsize},
        Arc,
    },
    thread::{self, available_parallelism},
};

use md5::{Digest, Md5};

type Input = String;

fn setup(input: &str) -> Input {
    input.trim().into()
}

fn solve(input: &Input, n: usize) -> usize {
    let parallelism = usize::from(available_parallelism().unwrap());

    thread::scope(|s| {
        let result = Arc::new(AtomicUsize::new(usize::MAX));
        (0..parallelism)
            .map(|i| {
                let result = Arc::clone(&result);
                s.spawn(move || {
                    (1..)
                        .skip(i)
                        .step_by(parallelism)
                        .take_while(|&j| j <= result.load(atomic::Ordering::SeqCst))
                        .find(|&j| test(input, j, n))
                        .inspect(|&j| {
                            result.fetch_min(j, atomic::Ordering::SeqCst);
                        });
                })
            })
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|t| t.join().unwrap());

        Arc::into_inner(result).unwrap().into_inner()
    })
}

fn test(input: &Input, i: usize, n: usize) -> bool {
    let hash = Md5::new().chain_update(format!("{input}{i}")).finalize();
    hash.iter().take(n >> 1).all(|&x| x == 0) && (n & 1 == 0 || hash[n >> 1] >> 4 == 0)
}

fn part1(input: &Input) -> usize {
    solve(input, 5)
}

fn part2(input: &Input) -> usize {
    solve(input, 6)
}

aoc::main!(2015, 4, ex: 1[a], 2[a]);
