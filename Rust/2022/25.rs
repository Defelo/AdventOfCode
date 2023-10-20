#![feature(test)]

type Input = Vec<String>;

fn setup(input: &str) -> Input {
    input.lines().map(Into::into).collect()
}

fn part1(input: &Input) -> String {
    to_snafu(input.iter().map(|snafu| from_snafu(snafu)).sum())
}

fn from_snafu(snafu: &str) -> u64 {
    snafu.bytes().fold(0, |acc, c| {
        let digit = match c {
            b'=' => 0,
            b'-' => 1,
            b'0' => 2,
            b'1' => 3,
            b'2' => 4,
            _ => panic!("invalid snafu number"),
        };
        acc * 5 + digit - 2
    })
}

fn to_snafu(mut num: u64) -> String {
    let mut out = Vec::with_capacity(32);
    while num > 0 {
        num += 2;
        out.push(match num % 5 {
            0 => '=',
            1 => '-',
            2 => '0',
            3 => '1',
            4 => '2',
            _ => unreachable!(),
        });
        num /= 5;
    }
    out.into_iter().rev().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! tests {
        ($($dec:literal, $snafu:literal,)*) => {
            #[test]
            fn test_from_snafu() {
                $(assert_eq!(from_snafu($snafu), $dec);)*
            }

            #[test]
            fn test_to_snafu() {
                $(assert_eq!(to_snafu($dec), $snafu, concat!($dec));)*
            }
        };
    }

    tests! {
        1, "1",
        2, "2",
        3, "1=",
        4, "1-",
        5, "10",
        6, "11",
        7, "12",
        8, "2=",
        9, "2-",
        10, "20",
        15, "1=0",
        20, "1-0",
        2022, "1=11-2",
        12345, "1-0---0",
        314159265, "1121-1110-1=0",
    }
}

aoc::main!(2022, 25, ex: 1);
