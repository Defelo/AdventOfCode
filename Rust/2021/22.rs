#![feature(test)]

use regex::Regex;

struct Cuboid {
    x1: i64,
    x2: i64,
    y1: i64,
    y2: i64,
    z1: i64,
    z2: i64,
    off: Vec<Cuboid>,
}

fn line_intersection(a1: i64, a2: i64, b1: i64, b2: i64) -> Option<(i64, i64)> {
    if a2 < b1 || b2 < a1 {
        Option::None
    } else {
        Option::Some((a1.max(b1), a2.min(b2)))
    }
}

impl Cuboid {
    fn copy(&self) -> Cuboid {
        Cuboid {
            x1: self.x1,
            x2: self.x2,
            y1: self.y1,
            y2: self.y2,
            z1: self.z1,
            z2: self.z2,
            off: vec![],
        }
    }

    fn get_intersection(&self, c: &Cuboid) -> Option<Cuboid> {
        let x = line_intersection(self.x1, self.x2, c.x1, c.x2);
        let y = line_intersection(self.y1, self.y2, c.y1, c.y2);
        let z = line_intersection(self.z1, self.z2, c.z1, c.z2);
        if let (Option::Some(x), Option::Some(y), Option::Some(z)) = (x, y, z) {
            Option::Some(Cuboid {
                x1: x.0,
                x2: x.1,
                y1: y.0,
                y2: y.1,
                z1: z.0,
                z2: z.1,
                off: vec![],
            })
        } else {
            Option::None
        }
    }

    fn subtract(&mut self, c: &Cuboid) {
        if let Option::Some(intersection) = self.get_intersection(c) {
            for o in self.off.iter_mut() {
                o.subtract(c);
            }
            self.off.push(intersection);
        }
    }

    fn volume(&self) -> i64 {
        (self.x2 - self.x1 + 1) * (self.y2 - self.y1 + 1) * (self.z2 - self.z1 + 1)
            - self.off.iter().map(|c| c.volume()).sum::<i64>()
    }
}

struct Step {
    on: bool,
    cuboid: Cuboid,
}

type Input = Vec<Step>;

fn setup(input: &str) -> Input {
    let regex =
        Regex::new(r"^(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)$").unwrap();
    input
        .lines()
        .map(|line| {
            let capture = regex.captures(line).unwrap();
            let get_num = |i| capture.get(i).unwrap().as_str().parse().unwrap();
            Step {
                on: capture.get(1).unwrap().as_str() == "on",
                cuboid: Cuboid {
                    x1: get_num(2),
                    x2: get_num(3),
                    y1: get_num(4),
                    y2: get_num(5),
                    z1: get_num(6),
                    z2: get_num(7),
                    off: vec![],
                },
            }
        })
        .collect()
}

fn part1(input: &Input) -> String {
    let mut cuboids: Vec<Cuboid> = vec![];
    for Step { on, cuboid } in input {
        if cuboid.x1 < -50
            || cuboid.x2 > 50
            || cuboid.y1 < -50
            || cuboid.y2 > 50
            || cuboid.z1 < -50
            || cuboid.z2 > 50
        {
            continue;
        }
        for c in cuboids.iter_mut() {
            c.subtract(cuboid);
        }
        if *on {
            cuboids.push(cuboid.copy());
        }
    }
    cuboids.iter().map(|c| c.volume()).sum::<i64>().to_string()
}

fn part2(input: &Input) -> String {
    let mut cuboids: Vec<Cuboid> = vec![];
    for Step { on, cuboid } in input {
        for c in cuboids.iter_mut() {
            c.subtract(cuboid);
        }
        if *on {
            cuboids.push(cuboid.copy());
        }
    }
    cuboids.iter().map(|c| c.volume()).sum::<i64>().to_string()
}

aoc::main!(2021, 22, ex: "1a"[a], "2a"[a], "1b"[b]);
