use std::collections::{HashMap, HashSet};

type Input = (u64, Vec<u64>);

fn setup(input: &str) -> Input {
    let mut out = HashMap::new();
    let mut seen = HashSet::new();
    let mut pwd = vec![];
    for line in input.trim().lines() {
        let mut split = line.split(' ');
        match (split.next(), split.next(), split.next()) {
            (Some("$"), Some("cd"), Some(dir)) => {
                if dir == "/" {
                    pwd.clear();
                } else if dir == ".." {
                    pwd.pop();
                } else {
                    pwd.push(dir);
                }
            }
            (Some(size), Some(name), None) => {
                if let Ok(size) = size.parse::<u64>() {
                    let key = (pwd.join("/"), name);
                    if seen.contains(&key) {
                        continue;
                    }
                    seen.insert(key);

                    for i in 0..=pwd.len() {
                        out.entry(pwd[..i].join("/"))
                            .and_modify(|x| *x += size)
                            .or_insert(size);
                    }
                }
            }
            _ => {}
        }
    }
    (out[""], out.values().copied().collect())
}

fn part1((_, sizes): &Input) -> u64 {
    sizes.iter().filter(|x| **x <= 100000).sum()
}

fn part2((root_size, sizes): &Input) -> u64 {
    let free = 70000000 - root_size;
    *sizes
        .iter()
        .filter(|x| **x >= 30000000 - free)
        .min()
        .unwrap()
}

aoc::main!(2022, 7);
aoc::example!(ex01, "07.1.txt", 95437, 24933642);
