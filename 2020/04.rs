use std::collections::HashMap;

use regex::Regex;

type Input = Vec<HashMap<String, String>>;

fn get_input() -> Input {
    let puzzle = include_str!("04.txt");
    let mut out = Vec::new();
    for block in puzzle.split("\n\n") {
        let mut cur = HashMap::new();
        for line in block.trim().lines() {
            for (key, value) in line.split(" ").map(|a| {
                let mut x = a.trim().split(":");
                (x.next().unwrap(), x.next().unwrap())
            }) {
                cur.insert(key.to_string(), value.to_string());
            }
        }
        out.push(cur);
    }
    out
}

fn part1(input: &Input) -> String {
    let mut out = 0;
    for passport in input {
        if ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"].iter().all(|x| passport.contains_key(&x.to_string())) {
            out += 1;
        }
    }
    out.to_string()
}

const RULES: [(&str, fn(&String) -> bool); 7] = [
    ("byr", |x: &String| {
        let num: i32 = x.parse().unwrap();
        num >= 1920 && num <= 2002
    }),
    ("iyr", |x: &String| {
        let num: i32 = x.parse().unwrap();
        num >= 2010 && num <= 2020
    }),
    ("eyr", |x: &String| {
        let num: i32 = x.parse().unwrap();
        num >= 2020 && num <= 2030
    }),
    ("hgt", |x: &String| {
        let regex = Regex::new(r"^(\d+)(cm|in)$").unwrap();
        match regex.captures(x) {
            None => false,
            Some(result) => {
                let num: i32 = result[1].parse().unwrap();
                match &result[2] {
                    "cm" => num >= 150 && num <= 193,
                    "in" => num >= 59 && num <= 76,
                    _ => false
                }
            }
        }
    }),
    ("hcl", |x: &String| {
        let regex = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
        regex.captures(x).is_some()
    }),
    ("ecl", |x: &String| {
        ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(&x.as_str())
    }),
    ("pid", |x: &String| {
        let regex = Regex::new(r"^\d{9}$").unwrap();
        regex.captures(x).is_some()
    })
];

fn part2(input: &Input) -> String {
    let mut out = 0;
    for passport in input {
        if RULES.iter().all(|(key, validator)| {
            match passport.get(&key.to_string()) {
                None => false,
                Some(value) => validator(value)
            }
        }) {
            out += 1;
        }
    }
    out.to_string()
}

fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
