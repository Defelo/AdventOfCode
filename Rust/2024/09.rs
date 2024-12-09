#![feature(test)]

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, VecDeque},
};

type Input = Vec<u8>;

fn setup(input: &str) -> Input {
    input.trim().bytes().map(|b| b - b'0').collect()
}

fn part1(input: &Input) -> u64 {
    let mut files = Vec::new();
    let mut free = VecDeque::new();
    let mut pos = 0;
    for (i, len) in input.iter().map(|&x| x as u64).enumerate() {
        if i & 1 == 0 {
            files.push((pos, len, i as u64 >> 1));
        } else if len > 0 {
            free.push_back((pos, len));
        }
        pos += len;
    }

    files
        .iter()
        .flat_map(|&(file_pos, file_len, file_id)| {
            (file_pos..file_pos + file_len).map(move |pos| (pos, file_id))
        })
        .rev()
        .map(|(mut pos, file_id)| {
            if let Some((free_pos, free_len)) = free.front_mut().filter(|free| free.0 < pos) {
                pos = *free_pos;
                *free_pos += 1;
                *free_len -= 1;
                if *free_len == 0 {
                    free.pop_front();
                }
            }
            pos * file_id
        })
        .sum()
}

fn part2(input: &Input) -> u64 {
    let mut files = Vec::new();
    let mut free_by_len = [const { BinaryHeap::new() }; 10];
    let mut pos = 0;
    for (i, len) in input.iter().map(|&x| x as u64).enumerate() {
        if i & 1 == 0 {
            files.push((pos, len, i as u64 >> 1));
        } else if len > 0 {
            free_by_len[len as usize].push(Reverse(pos));
        }
        pos += len;
    }

    files
        .iter()
        .rev()
        .map(|&(file_pos, file_len, file_id)| {
            let pos = (file_len as _..free_by_len.len())
                .filter_map(|len| free_by_len[len].peek().map(|&free_pos| (free_pos.0, len)))
                .min()
                .filter(|&(free_pos, _)| free_pos < file_pos)
                .map(|(free_pos, free_len)| {
                    free_by_len[free_len].pop();
                    if free_len > file_len as usize {
                        free_by_len[free_len - file_len as usize]
                            .push(Reverse(free_pos + file_len));
                    }
                    free_pos
                })
                .unwrap_or(file_pos);
            file_len * (pos * 2 + file_len - 1) / 2 * file_id
        })
        .sum()
}

aoc::main!(2024, 9, ex: 1);
