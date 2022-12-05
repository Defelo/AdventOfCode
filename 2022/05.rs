type Input = (Vec<String>, Vec<(usize, usize, usize)>);

fn get_input(input: &str) -> Input {
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

fn part1((stacks, instructions): &Input) -> String {
    let mut stacks = stacks.clone();
    for (cnt, i, j) in instructions {
        for _ in 0..*cnt {
            let c = stacks[*i].pop().unwrap();
            stacks[*j].push(c);
        }
    }
    stacks.iter().map(|s| s.chars().last().unwrap()).collect()
}

fn part2((stacks, instructions): &Input) -> String {
    let mut stacks = stacks.clone();
    for (cnt, i, j) in instructions {
        let n = stacks[*i].len() - cnt;
        let s = &stacks[*i][n..].to_owned();
        stacks[*j].push_str(s);
        stacks[*i].truncate(n);
    }
    stacks.iter().map(|s| s.chars().last().unwrap()).collect()
}

aoc::main!(2022, 5);
aoc::example!(ex01, "05.1.txt", "CMZ", "MCD");
