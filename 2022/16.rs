use regex::Regex;
use rustc_hash::FxHashMap;

type Input = Vec<Valve>;

struct Valve {
    rate: u32,
    tunnels: Vec<usize>,
}

fn setup(input: &str) -> Input {
    let regex = Regex::new(r"^Valve ([A-Z]+) .* rate=(\d+); .* valves? ([A-Z, ]+)$").unwrap();
    let mut names = FxHashMap::default();
    names.insert("AA", 0);
    let mut name = |n| {
        let cnt = names.len();
        *names.entry(n).or_insert(cnt)
    };

    let mut valves: FxHashMap<usize, Valve> = input
        .trim()
        .lines()
        .map(|line| {
            let caps = regex.captures(line).unwrap();
            let v = name(caps.get(1).unwrap().as_str());
            let rate = caps[2].parse().unwrap();
            let tunnels = caps
                .get(3)
                .unwrap()
                .as_str()
                .split(", ")
                .map(&mut name)
                .collect();
            (v, Valve { rate, tunnels })
        })
        .collect();

    (0..valves.len())
        .map(|x| valves.remove(&x).unwrap())
        .collect()
}

struct Solver<'a> {
    valves: &'a Input,
    dist: Vec<Vec<u32>>,
    dp: FxHashMap<(usize, u32, u64), u32>,
}

impl<'a> Solver<'a> {
    fn new(valves: &'a Input) -> Self {
        let mut dist = vec![vec![u32::MAX; valves.len()]; valves.len()];
        for (i, v) in valves.iter().enumerate() {
            dist[i][i] = 0;
            v.tunnels.iter().for_each(|&j| dist[i][j] = 1);
        }
        for k in 0..valves.len() {
            for i in 0..valves.len() {
                for j in 0..valves.len() {
                    if let Some(d) = dist[i][k].checked_add(dist[k][j]) {
                        dist[i][j] = dist[i][j].min(d);
                    }
                }
            }
        }

        Self {
            valves,
            dist,
            dp: FxHashMap::default(),
        }
    }

    fn solve(&mut self, p: usize, time: u32, closed: u64) -> u32 {
        let key = (p, time, closed);
        if let Some(&result) = self.dp.get(&key) {
            return result;
        }

        let result = (0..self.valves.len())
            .filter_map(|q| {
                if closed & 1 << q == 0 {
                    return None;
                }
                if let Some(t) = time.checked_sub(self.dist[p][q] + 1) {
                    Some(self.solve(q, t, closed & !(1 << q)) + self.valves[q].rate * t)
                } else {
                    None
                }
            })
            .max()
            .unwrap_or(0);
        self.dp.insert(key, result);
        result
    }
}

fn part1(input: &Input) -> u32 {
    Solver::new(input).solve(
        0,
        30,
        input
            .iter()
            .enumerate()
            .filter(|(_, x)| x.rate > 0)
            .fold(0, |acc, (i, _)| acc | 1 << i),
    )
}

fn part2(input: &Input) -> u32 {
    let mut solver = Solver::new(input);
    let valves = input
        .iter()
        .enumerate()
        .filter(|(_, x)| x.rate > 0)
        .map(|(i, _)| i)
        .collect::<Vec<_>>();
    (0u64..1 << (valves.len() - 1))
        .filter(|&s| valves.len().abs_diff(s.count_ones() as usize * 2) <= 1)
        .map(|s| {
            let a = solver.solve(
                0,
                26,
                valves
                    .iter()
                    .enumerate()
                    .filter(|&(i, _)| s & 1 << i != 0)
                    .fold(0, |acc, (_, j)| acc | 1 << j),
            );
            let b = solver.solve(
                0,
                26,
                valves
                    .iter()
                    .enumerate()
                    .filter(|&(i, _)| s & 1 << i == 0)
                    .fold(0, |acc, (_, j)| acc | 1 << j),
            );
            a + b
        })
        .max()
        .unwrap()
}

aoc::main!(2022, 16);
aoc::example!(ex01, "16.1.txt", 1651, 1707);
aoc::test_input!("16.txt", 1991, 2705);
