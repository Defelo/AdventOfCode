use std::collections::HashSet;

fn get_input() -> Vec<i32> {
    let puzzle = include_str!("01.txt");
    puzzle.lines().map(|i| i.parse().unwrap()).collect()
}

fn part1(input: &Vec<i32>) -> i32 {
    let mut seen: HashSet<i32> = HashSet::new();
    for &x in input {
        let y = 2020 - x;
        if seen.contains(&y) {
            return x * y;
        }
        seen.insert(x);
    }
    panic!();
}

fn part2(input: &Vec<i32>) -> i32 {
    for (i, &x) in input.iter().enumerate() {
        let mut seen: HashSet<i32> = HashSet::new();
        for &y in &input[i+1..] {
            let z = 2020 - x - y;
            if seen.contains(&z) {
                return x * y * z;
            }
            seen.insert(y);
        }
    }
    panic!();
}

fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}