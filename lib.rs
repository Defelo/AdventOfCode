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
        let ans1 = (self.part1)(&input);
        let ans2 = (self.part2)(&input);
        println!("part 1: {ans1}");
        println!("part 2: {ans2}");
    }

    pub fn bench(&self, c: &mut criterion::Criterion) {
        let raw_input = self.get_raw_input();
        c.bench_function(&format!("{}/{}/input", self.year, self.day), |b| {
            b.iter(|| (self.get_input)(black_box(&raw_input)))
        });
        let input = (self.get_input)(&raw_input);
        c.bench_function(&format!("{}/{}/1", self.year, self.day), |b| {
            b.iter(|| (self.part1)(black_box(&input)))
        });
        c.bench_function(&format!("{}/{}/2", self.year, self.day), |b| {
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
