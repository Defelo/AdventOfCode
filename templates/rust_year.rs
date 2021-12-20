{% for feature in features -%}
#![feature({{ feature }})]
{% endfor %}
#![feature(div_duration)]

use std::path::Path;
use std::time::{Duration, Instant};

{% for n in range(1, 26) %}
#[path = "{{ '%02d' % n }}.rs"]
mod d{{ '%02d' % n }};
{% endfor %}

fn main() {
    println!("##### Advent of Code {{ year }} #####");
    let mut results = vec![];
    for (day, func) in &[
        {% for n in range(1, 26) -%}
            ({{ '%02d' % n }}, d{{ '%02d' % n }}::run as fn() -> (String, String)),
        {% endfor %}
    ] {
        if !Path::new(format!("{{ year }}/{:02}.txt", day).as_str()).exists() {continue;}
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
