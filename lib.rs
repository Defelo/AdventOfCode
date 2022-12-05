#[macro_export]
macro_rules! main {
    ($year:expr, $day:expr) => {
        #[cfg(not(test))]
        fn main() {
            let input = std::fs::read_to_string(&format!("{}/{:02}.txt", $year, $day)).unwrap();
            let data = setup(&input);
            println!("{}", part1(&data));
            println!("{}", part2(&data));
        }

        #[cfg(test)]
        fn criterion_benchmark(c: &mut criterion::Criterion) {
            let input = std::fs::read_to_string(&format!("{}/{:02}.txt", $year, $day)).unwrap();
            let data = setup(&input);
            c.bench_function(&format!("{}/{:02}/setup", $year, $day), |b| {
                b.iter(|| setup(criterion::black_box(&input)))
            });
            c.bench_function(&format!("{}/{:02}/1", $year, $day), |b| {
                b.iter(|| part1(criterion::black_box(&data)))
            });
            c.bench_function(&format!("{}/{:02}/2", $year, $day), |b| {
                b.iter(|| part2(criterion::black_box(&data)))
            });
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
                let data = super::setup(&include_str!($path));
                assert_eq!(super::part1(&data), $part1);
            }

            #[test]
            fn part2() {
                let data = super::setup(&include_str!($path));
                assert_eq!(super::part2(&data), $part2);
            }
        }
    };
}
