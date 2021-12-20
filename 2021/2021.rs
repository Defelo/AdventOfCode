#![feature(test)]
#![feature(drain_filter)]
#![feature(binary_heap_into_iter_sorted)]

#![feature(div_duration)]

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


fn main() {
    println!("##### Advent of Code 2021 #####");
    let mut results = vec![];
    for (day, func) in &[
        (01, d01::run as fn() -> (String, String)),
        (02, d02::run as fn() -> (String, String)),
        (03, d03::run as fn() -> (String, String)),
        (04, d04::run as fn() -> (String, String)),
        (05, d05::run as fn() -> (String, String)),
        (06, d06::run as fn() -> (String, String)),
        (07, d07::run as fn() -> (String, String)),
        (08, d08::run as fn() -> (String, String)),
        (09, d09::run as fn() -> (String, String)),
        (10, d10::run as fn() -> (String, String)),
        (11, d11::run as fn() -> (String, String)),
        (12, d12::run as fn() -> (String, String)),
        (13, d13::run as fn() -> (String, String)),
        (14, d14::run as fn() -> (String, String)),
        (15, d15::run as fn() -> (String, String)),
        (16, d16::run as fn() -> (String, String)),
        (17, d17::run as fn() -> (String, String)),
        (18, d18::run as fn() -> (String, String)),
        (19, d19::run as fn() -> (String, String)),
        (20, d20::run as fn() -> (String, String)),
        (21, d21::run as fn() -> (String, String)),
        (22, d22::run as fn() -> (String, String)),
        (23, d23::run as fn() -> (String, String)),
        (24, d24::run as fn() -> (String, String)),
        (25, d25::run as fn() -> (String, String)),
        
    ] {
        if !Path::new(format!("2021/{:02}.txt", day).as_str()).exists() {continue;}
        let s = Instant::now();
        let (part1, part2) = func() as (String, String);
        results.push((day, part1, part2, s.elapsed()));
    }

    let total_duration = results.iter().map(|(_, _, _, duration)| {
        duration
    }).sum::<Duration>();

    for (day, part1, part2, duration) in results {
        println!();
        println!("=== Day {} ===", day);
        println!("Part 1: {}", part1);
        println!("Part 2: {}", part2);
        println!("  => {:.6} ms ({:.2}%)", duration.as_secs_f64() * 1000f64, duration.div_duration_f64(total_duration) * 100f64);
    }
    println!();
    println!("=> {:.6} ms", total_duration.as_secs_f64() * 1000f64);
}
