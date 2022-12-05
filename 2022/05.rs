type Input = (Vec<String>, Vec<(usize, usize, usize)>);

pub fn solve((stacks, instructions): &Input, get_next: fn(usize, usize) -> usize) -> String {
    (0..stacks.len())
        .map(|mut st| {
            let mut h = 0;
            for &(cnt, i, j) in instructions.iter().rev() {
                if st == j {
                    if h < cnt {
                        st = i;
                        h = (get_next)(cnt, h);
                    } else {
                        h -= cnt;
                    }
                } else if st == i {
                    h += cnt;
                }
            }
            stacks[st].chars().nth_back(h).unwrap()
        })
        .collect()
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
    solve(input, |cnt, h| cnt - h - 1)
}

fn part2(input: &Input) -> String {
    solve(input, |_, h| h)
}

aoc::main!(2022, 5);
aoc::example!(ex01, "05.1.txt", "CMZ", "MCD");
