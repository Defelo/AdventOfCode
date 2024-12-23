#![feature(test)]

#[doc(hidden)]
pub use test::Bencher;

pub mod arrays;
pub mod bitset;
pub mod grid;
pub mod intcode;
pub mod iter_ext;
pub mod math;
pub mod parsing;
pub mod range;
pub mod trie;
pub mod tuples;

extern crate test;

pub fn matrix<const N: usize, const M: usize>(
    mut f: impl FnMut(usize, usize) -> usize,
) -> [[usize; M]; N] {
    std::array::from_fn(|x| std::array::from_fn(|y| f(x, y)))
}

#[macro_export]
macro_rules! main {
    ($year:literal, $day:tt $(, ex: $($example:literal $([$expart:ident])?),*)?) => {
        ::aoc::__main!($year, $day);
        ::aoc::__setup!(_input, ::std::concat!("../../.cache/", $year, "/", $day));
        ::aoc::__inp!($day, _input, ::std::concat!("../../.cache/", $year, "/", $day));
        $($(
            ::paste::paste! {
                ::aoc::__setup!([< _ex $example >], ::std::concat!("../../examples/", $year, "/", $day, "/", $example));
                ::aoc::__inp!($day, [< _ex $example >], ::std::concat!("../../examples/", $year, "/", $day, "/", $example) $(, $expart)?);
            }
        )*)?
    };
}

#[macro_export]
macro_rules! year {
    ($($path:tt),*$(,)?) => {
        #[cfg(not(test))]
        fn main() {
            $({
                #[path = $path]
                #[allow(unused_attributes)]
                mod day;
                day::_main();
            })*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __ifnot25 {
    (25, $($tt:tt)*) => {};
    ($x:tt, $($tt:tt)*) => {
        $($tt)*
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __main {
    ($year:literal, $day:tt) => {
        fn main() {
            let path = ::std::env::args()
                .nth(1)
                .unwrap_or_else(|| ::std::concat!("../.cache/", $year, "/", $day).into());
            let input = ::std::fs::read_to_string(path).unwrap();
            let input = setup(&input);
            ::std::println!("{}", part1(&input));
            ::aoc::__ifnot25! { $day,
                ::std::println!("{}", part2(&input));
            }
        }

        pub fn _main() {
            let path = ::std::concat!("../.cache/", $year, "/", $day);
            let input = ::std::fs::read_to_string(path).unwrap();
            let input = setup(&input);
            ::std::print!("[{}/{:02}/1] {:<20}", $year, $day, part1(&input));
            ::aoc::__ifnot25! { $day,
                ::std::print!("[{}/{:02}/2] {}", $year, $day, part2(&input));
            }
            ::std::println!();
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __setup {
    ($ident:expr, $in:expr) => {
        ::paste::paste! {
            #[cfg(test)]
            #[bench]
            fn [< bench $ident _setup >](b: &mut ::aoc::Bencher) {
                let input = ::std::include_str!($in);
                b.iter(|| setup(input));
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __inp {
    ($day:tt, $ident:expr, $in:expr) => {
        ::aoc::__inp!($day, $ident, $in, 1);
        ::aoc::__ifnot25! { $day,
            ::aoc::__inp!($day, $ident, $in, 2);
        }
    };
    ($day:tt, $ident:expr, $in:expr) => {
        ::aoc::__inp!($day, $ident, $in, 1);
        ::aoc::__inp!($day, $ident, $in, 2);
    };
    ($day:tt, $ident:expr, $in:expr, a) => {
        ::aoc::__inp!($day, $ident, $in, 1);
    };
    ($day:tt, $ident:expr, $in:expr, b) => {
        ::aoc::__inp!($day, $ident, $in, 2);
    };
    ($day:tt, $ident:expr, $in:expr, $part:expr) => {
        ::paste::paste! {
            #[cfg(test)]
            #[test]
            fn [< test $ident _part $part >]() {
                let input = setup(::std::include_str!($in));
                let out = [< part $part >](&input);
                assert_eq!(
                    out.to_string().trim(),
                    include_str!(::std::concat!($in, ".", $part)).trim()
                );
            }


            #[cfg(test)]
            #[bench]
            fn [< bench $ident _part $part >](b: &mut ::aoc::Bencher) {
                let input = setup(::std::include_str!($in));
                b.iter(|| [< part $part >](&input));
            }
        }
    };
}
