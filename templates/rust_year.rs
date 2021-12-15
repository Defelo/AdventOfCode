{% for feature in features -%}
#![feature({{ feature }})]
{% endfor %}

use std::path::Path;

{% for n in range(1, 26) %}
#[path = "{{ '%02d' % n }}.rs"]
mod d{{ '%02d' % n }};
{% endfor %}

fn main() {
    println!("##### Advent of Code {{ year }} #####");
    for (day, func) in &[
        {% for n in range(1, 26) -%}
            ({{ '%02d' % n }}, d{{ '%02d' % n }}::main as fn()),
        {% endfor %}
    ] {
        if !Path::new(format!("{{ year }}/{:02}.txt", day).as_str()).exists() {continue;}
        println!();
        println!("=== Day {} ===", day);
        func();
    }
}
