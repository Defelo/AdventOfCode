#![feature(test)]

type Input = Vec<(i8, i8)>;

fn setup(input: &str) -> Input {
    let lines: Vec<String> = input.lines().map(|line| line.to_string()).collect();
    (0..14)
        .map(|i| {
            let get = |j: usize| {
                lines[i * 18 + j]
                    .split(' ')
                    .nth(2)
                    .unwrap()
                    .parse()
                    .unwrap()
            };
            (get(5), get(15))
        })
        .collect()
}

fn part1(input: &Input) -> String {
    let mut out = [9u8; 14];
    let mut stack = vec![];
    for (i, (mut x, y)) in input.iter().cloned().enumerate() {
        if x < 0 {
            let (j, y) = stack.pop().unwrap();
            x += y;
            if x < 0 {
                out[i] -= -x as u8;
            } else {
                out[j] -= x as u8;
            }
        } else {
            stack.push((i, y));
        }
    }
    out.iter().map(|&x| (x + 0x30) as char).collect()
}

fn part2(input: &Input) -> String {
    let mut out = [1u8; 14];
    let mut stack = vec![];
    for (i, (mut x, y)) in input.iter().cloned().enumerate() {
        if x < 0 {
            let (j, y) = stack.pop().unwrap();
            x += y;
            if x < 0 {
                out[j] += -x as u8;
            } else {
                out[i] += x as u8;
            }
        } else {
            stack.push((i, y));
        }
    }
    out.iter().map(|&x| (x + 0x30) as char).collect()
}

aoc::main!(2021, 24);
