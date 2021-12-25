#![allow(unused)]
{% for feature in features -%}
#![feature({{ feature }})]
{% endfor %}
#![feature(div_duration)]

use std::ops::AddAssign;
use std::path::Path;
use std::time::{Duration, Instant};

{% for n in range(1, 26) %}
#[path = "{{ '%02d' % n }}.rs"]
mod d{{ '%02d' % n }};
{% endfor %}

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
    println!("##### Advent of Code {{ year }} #####");
    let mut results = vec![];
    let mut total = 0;
    let mut failed = 0;
    for day in &[
        {% for n in range(1, 26) -%}
            Day { day: {{ '%02d' % n }}, func: d{{ '%02d' % n }}::run, part1: "", part2: "" },
        {% endfor %}
    ] {
        if !Path::new(format!("{{ year }}/{:02}.txt", day.day).as_str()).exists() {continue;}
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
        if day.day != 25 {
            print_result(2, &part2, day.part2, &mut total, &mut failed);
        }
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
