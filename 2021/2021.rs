#![feature(test)]
#![feature(drain_filter)]
#![feature(binary_heap_into_iter_sorted)]

#![feature(div_duration)]

use std::ops::AddAssign;
use std::path::Path;
use std::time::{Duration, Instant};


#[path = "01.rs"]
mod d01;

#[path = "02.rs"]
mod d02;

#[path = "03.rs"]
mod d03;

#[path = "04.rs"]
mod d04;

#[path = "05.rs"]
mod d05;

#[path = "06.rs"]
mod d06;

#[path = "07.rs"]
mod d07;

#[path = "08.rs"]
mod d08;

#[path = "09.rs"]
mod d09;

#[path = "10.rs"]
mod d10;

#[path = "11.rs"]
mod d11;

#[path = "12.rs"]
mod d12;

#[path = "13.rs"]
mod d13;

#[path = "14.rs"]
mod d14;

#[path = "15.rs"]
mod d15;

#[path = "16.rs"]
mod d16;

#[path = "17.rs"]
mod d17;

#[path = "18.rs"]
mod d18;

#[path = "19.rs"]
mod d19;

#[path = "20.rs"]
mod d20;

#[path = "21.rs"]
mod d21;

#[path = "22.rs"]
mod d22;

#[path = "23.rs"]
mod d23;

#[path = "24.rs"]
mod d24;

#[path = "25.rs"]
mod d25;


struct Day {
    day: i32,
    func: fn() -> (String, String),
    part1: &'static str,
    part2: &'static str,
}

struct DayResult<'a> {
    day: &'a Day,
    part1: String,
    part2: String,
    duration: Duration,
}

fn print_result(part: u8, actual: &String, expected: &str, total: &mut usize, failed: &mut usize) {
    if expected.is_empty() {
        println!("\x1b[33m skipped\x1b[0m | Part {}: \x1b[33m{}\x1b[0m", part, actual);
    } else if actual == expected {
        total.add_assign(1);
        println!("\x1b[32m\x1b[1m✔ passed\x1b[0m | Part {}: \x1b[32m{}\x1b[0m", part, actual);
    } else {
        total.add_assign(1);
        failed.add_assign(1);
        println!("\x1b[31m\x1b[1m✘ failed\x1b[0m | Part {}: \x1b[31m{}\x1b[0m -> \x1b[32m{}\x1b[0m", part, actual, expected);
    }
}

fn main() {
    println!("##### Advent of Code 2021 #####");
    let mut results = vec![];
    let mut total = 0;
    let mut failed = 0;
    for day in &[
        Day { day: 01, func: d01::run, part1: "1266", part2: "1217" },
        Day { day: 02, func: d02::run, part1: "1648020", part2: "1759818555" },
        Day { day: 03, func: d03::run, part1: "3895776", part2: "7928162" },
        Day { day: 04, func: d04::run, part1: "49860", part2: "24628" },
        Day { day: 05, func: d05::run, part1: "5698", part2: "15463" },
        Day { day: 06, func: d06::run, part1: "345793", part2: "1572643095893" },
        Day { day: 07, func: d07::run, part1: "340052", part2: "92948968" },
        Day { day: 08, func: d08::run, part1: "392", part2: "1004688" },
        Day { day: 09, func: d09::run, part1: "425", part2: "1135260" },
        Day { day: 10, func: d10::run, part1: "278475", part2: "3015539998" },
        Day { day: 11, func: d11::run, part1: "1659", part2: "227" },
        Day { day: 12, func: d12::run, part1: "3679", part2: "107395" },
        Day { day: 13, func: d13::run, part1: "747", part2: "ARHZPCUH" },
        Day { day: 14, func: d14::run, part1: "3247", part2: "4110568157153" },
        Day { day: 15, func: d15::run, part1: "366", part2: "2829" },
        Day { day: 16, func: d16::run, part1: "906", part2: "819324480368" },
        Day { day: 17, func: d17::run, part1: "3916", part2: "2986" },
        Day { day: 18, func: d18::run, part1: "4469", part2: "4770" },
        Day { day: 19, func: d19::run, part1: "403", part2: "10569" },
        Day { day: 20, func: d20::run, part1: "5419", part2: "17325" },
        Day { day: 21, func: d21::run, part1: "1067724", part2: "630947104784464" },
        Day { day: 22, func: d22::run, part1: "623748", part2: "1227345351869476" },
        Day { day: 23, func: d23::run, part1: "", part2: "" },
        Day { day: 24, func: d24::run, part1: "", part2: "" },
        Day { day: 25, func: d25::run, part1: "", part2: "" },
        
    ] {
        if !Path::new(format!("2021/{:02}.txt", day.day).as_str()).exists() {continue;}
        let s = Instant::now();
        let (part1, part2) = (day.func)() as (String, String);
        results.push(DayResult {day, part1, part2, duration: s.elapsed()});
    }

    let total_duration = results.iter().map(|DayResult {duration, .. }| {
        duration
    }).sum::<Duration>();

    for DayResult {day, part1, part2, duration} in results {
        println!();
        println!("=== Day {} ===", day.day);
        print_result(1, &part1, day.part1, &mut total, &mut failed);
        print_result(2, &part2, day.part2, &mut total, &mut failed);
        println!("  => {:.6} ms ({:.2}%)", duration.as_secs_f64() * 1000f64, duration.div_duration_f64(total_duration) * 100f64);
    }
    println!();
    if failed == 0 {
        println!("\x1b[32m\x1b[1m✔ {}/{} passed\x1b[0m", total, total);
    } else {
        println!("\x1b[31m\x1b[1m✘ {}/{} failed\x1b[0m", failed, total);
    }
    println!("=> {:.6} ms", total_duration.as_secs_f64() * 1000f64);
}
