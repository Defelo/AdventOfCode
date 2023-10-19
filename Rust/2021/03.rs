#![feature(test, extract_if)]

type Input = Vec<String>;

fn setup(input: &str) -> Input {
    input.lines().map(|s| s.to_string()).collect()
}

fn count(input: &Input, i: usize, chr: &char) -> usize {
    input
        .iter()
        .filter(|s| s.chars().nth(i).unwrap() == *chr)
        .count()
}

fn most_common(input: &Input, i: usize) -> char {
    "01".chars().max_by_key(|c| count(input, i, c)).unwrap()
}

fn least_common(input: &Input, i: usize) -> char {
    "01".chars().min_by_key(|c| count(input, i, c)).unwrap()
}

fn part1(input: &Input) -> String {
    let mut most = 0;
    let mut least = 0;
    for i in 0..input[0].len() {
        most = most << 1 | (most_common(input, i) as u32 - '0' as u32);
        least = least << 1 | (least_common(input, i) as u32 - '0' as u32);
    }
    (most * least).to_string()
}

fn find(input: &Input, x: bool) -> isize {
    let mut out = input.clone();
    for i in 0..input[0].len() {
        let mx = if x {
            least_common(&out, i)
        } else {
            most_common(&out, i)
        };
        out.extract_if(|x| x.chars().nth(i).unwrap() != mx).count();
        if out.len() == 1 {
            return isize::from_str_radix(out[0].as_str(), 2).unwrap();
        }
    }
    panic!();
}

fn part2(input: &Input) -> String {
    (find(input, true) * find(input, false)).to_string()
}

aoc::main!(2021, 3, ex: 1);
