use std::fmt::Display;

use criterion::black_box;

pub struct Day<T, U: Display, V: Display> {
    year: u16,
    day: u8,
    get_input: fn(&str) -> T,
    part1: fn(&T) -> U,
    part2: fn(&T) -> V,
}

impl<T, U: Display, V: Display> Day<T, U, V> {
    pub fn new(
        year: u16,
        day: u8,
        get_input: fn(&str) -> T,
        part1: fn(&T) -> U,
        part2: fn(&T) -> V,
    ) -> Self {
        Self {
            year,
            day,
            get_input,
            part1,
            part2,
        }
    }

    fn get_raw_input(&self) -> String {
        std::fs::read_to_string(format!("{}/{:02}.txt", self.year, self.day)).unwrap()
    }

    pub fn run(&self) {
        println!("=== {}/{:02} ===", self.year, self.day);
        let input = (self.get_input)(&self.get_raw_input());
        println!("part 1: {}", (self.part1)(&input));
        println!("part 2: {}", (self.part2)(&input));
    }

    pub fn bench(&self, c: &mut criterion::Criterion) {
        let raw_input = self.get_raw_input();
        c.bench_function(&format!("{}/{:02}/input", self.year, self.day), |b| {
            b.iter(|| (self.get_input)(black_box(&raw_input)))
        });
        let input = (self.get_input)(&raw_input);
        c.bench_function(&format!("{}/{:02}/1", self.year, self.day), |b| {
            b.iter(|| (self.part1)(black_box(&input)))
        });
        c.bench_function(&format!("{}/{:02}/2", self.year, self.day), |b| {
            b.iter(|| (self.part2)(black_box(&input)))
        });
    }
}

#[macro_export]
macro_rules! day {
    ($year:expr, $day:expr) => {
        aoc::Day::new($year, $day, get_input, part1, part2)
    };
}

#[macro_export]
macro_rules! main {
    ($year:expr, $day:expr) => {
        #[cfg(not(test))]
        fn main() {
            aoc::day!($year, $day).run();
        }

        #[cfg(test)]
        fn criterion_benchmark(c: &mut criterion::Criterion) {
            aoc::day!($year, $day).bench(c)
        }

        #[cfg(test)]
        criterion::criterion_group!(benches, criterion_benchmark);
        #[cfg(test)]
        criterion::criterion_main!(benches);
    };
}

#[macro_export]
macro_rules! example {
    ($name:ident, $path:expr, $part1:expr, $part2:expr) => {
        #[cfg(test)]
        mod $name {
            #[test]
            fn part1() {
                let input = super::get_input(&include_str!($path));
                assert_eq!(super::part1(&input), $part1);
            }

            #[test]
            fn part2() {
                let input = super::get_input(&include_str!($path));
                assert_eq!(super::part2(&input), $part2);
            }
        }
    };
}
