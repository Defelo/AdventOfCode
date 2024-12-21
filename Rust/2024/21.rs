#![feature(test)]

use std::iter;

use aoc::matrix;
use itertools::Itertools;
use rustc_hash::FxHashSet;

type Input = Vec<Vec<u8>>;

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            l.bytes()
                .map(|b| match b {
                    b'0'..=b'9' => b - b'0',
                    b'A' => 10,
                    _ => panic!(),
                })
                .collect()
        })
        .collect()
}

type Sequence = Vec<u8>;

/// For each keypad button pair compute all optimal sequences to activate the
/// second button, starting at the first one.
fn compute_sequences<const N: usize>(keypad: [(u8, u8); N]) -> [[Vec<Sequence>; N]; N] {
    keypad.map(|(i1, j1)| {
        keypad.map(|(i2, j2)| {
            iter::empty()
                .chain(iter::repeat_n(0, i1.saturating_sub(i2) as _))
                .chain(iter::repeat_n(1, j1.saturating_sub(j2) as _))
                .chain(iter::repeat_n(2, i2.saturating_sub(i1) as _))
                .chain(iter::repeat_n(3, j2.saturating_sub(j1) as _))
                .permutations((i1.abs_diff(i2) + j1.abs_diff(j2)) as _)
                .filter(|seq| {
                    let (mut i, mut j) = (i1, j1);
                    seq.iter().all(|&d| {
                        match d {
                            0 => i -= 1,
                            1 => j -= 1,
                            2 => i += 1,
                            3 => j += 1,
                            _ => panic!(),
                        }
                        keypad.contains(&(i, j))
                    })
                })
                .collect::<FxHashSet<_>>()
                .into_iter()
                .map(|mut s| {
                    s.push(DIR_A);
                    s
                })
                .collect()
        })
    })
}

const NUM_A: u8 = 10;
const DIR_A: u8 = 4;

type NumSeqs = [[Vec<Sequence>; 11]; 11];
type DirSeqs = [[Vec<Sequence>; 5]; 5];

const NUM_KEYPAD: [(u8, u8); 11] = [
    (3, 1), // 0
    (2, 0), // 1
    (2, 1), // 2
    (2, 2), // 3
    (1, 0), // 4
    (1, 1), // 5
    (1, 2), // 6
    (0, 0), // 7
    (0, 1), // 8
    (0, 2), // 9
    (3, 2), // A
];

const DIR_KEYPAD: [(u8, u8); 5] = [
    (0, 1), // ^
    (1, 0), // <
    (1, 1), // v
    (1, 2), // >
    (0, 2), // A
];

fn steps(init: u8, seq: &[u8]) -> impl Iterator<Item = (usize, usize)> + use<'_> {
    iter::once(init as _)
        .chain(seq.iter().map(|&x| x as _))
        .tuple_windows()
}

type DirMatrix = [[usize; 5]; 5];

/// For each button pair on the directional keypad closest to the door compute
/// the minimum number of button presses needed on the manually controlled
/// directional keypad to activate the second button starting at the first one.
fn compute_dir_matrix<const N: usize>(dir_seqs: &DirSeqs) -> DirMatrix {
    (1..N).fold(
        // if there is only one robot controlled directional keypad, use one of the pre-computed
        // sequences on the human controlled keypad. doesn't matter which one, they should all have
        // the same (optimal) length.
        matrix(|x, y| dir_seqs[x][y][0].len()),
        // if there are more robot controlled directional keypads, compute a new matrix based on
        // the previous one: try all sequences from x to y on the directional keypad, sum up all
        // the required button presses for each sequence and find the minimum of those.
        |prev, _| {
            matrix(|x, y| {
                dir_seqs[x][y]
                    .iter()
                    .map(|seq| steps(DIR_A, seq).map(|(x, y)| prev[x][y]).sum())
                    .min()
                    .unwrap()
            })
        },
    )
}

type NumMatrix = [[usize; 11]; 11];

/// Similarly to [`compute_dir_matrix`], for each button pair on the numeric
/// keypad at the door compute the minimum number of button presses needed on
/// the manually controlled directional keypad to activate the second button
/// starting at the first one.
fn compute_num_matrix(num_seqs: &NumSeqs, dir_matrix: &DirMatrix) -> NumMatrix {
    matrix(|x, y| {
        // try all sequences from x to y on the numeric keypad, sum up all the required
        // button presses for each sequence and find the minimum of those.
        num_seqs[x][y]
            .iter()
            .map(|seq| steps(DIR_A, seq).map(|(x, y)| dir_matrix[x][y]).sum())
            .min()
            .unwrap()
    })
}

fn min_len(seq: &[u8], num_matrix: &NumMatrix) -> usize {
    steps(NUM_A, seq).map(|(x, y)| num_matrix[x][y]).sum()
}

fn numeric_part(seq: &[u8]) -> usize {
    seq.iter()
        .filter(|&&b| b < 10)
        .fold(0, |acc, &b| acc * 10 + b as usize)
}

fn solve<const N: usize>(input: &Input) -> usize {
    let num_seqs = compute_sequences(NUM_KEYPAD);
    let dir_seqs = compute_sequences(DIR_KEYPAD);
    let dir_matrix = compute_dir_matrix::<N>(&dir_seqs);
    let num_matrix = compute_num_matrix(&num_seqs, &dir_matrix);

    input
        .iter()
        .map(|seq| min_len(seq, &num_matrix) * numeric_part(seq))
        .sum()
}

fn part1(input: &Input) -> usize {
    solve::<2>(input)
}

fn part2(input: &Input) -> usize {
    solve::<25>(input)
}

aoc::main!(2024, 21, ex: 1);
