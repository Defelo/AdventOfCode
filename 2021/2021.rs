#![feature(test)]
#![feature(drain_filter)]
#![feature(binary_heap_into_iter_sorted)]


use std::path::Path;


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
    for (day, func) in &[
        (01, d01::main as fn()),
        (02, d02::main as fn()),
        (03, d03::main as fn()),
        (04, d04::main as fn()),
        (05, d05::main as fn()),
        (06, d06::main as fn()),
        (07, d07::main as fn()),
        (08, d08::main as fn()),
        (09, d09::main as fn()),
        (10, d10::main as fn()),
        (11, d11::main as fn()),
        (12, d12::main as fn()),
        (13, d13::main as fn()),
        (14, d14::main as fn()),
        (15, d15::main as fn()),
        (16, d16::main as fn()),
        (17, d17::main as fn()),
        (18, d18::main as fn()),
        (19, d19::main as fn()),
        (20, d20::main as fn()),
        (21, d21::main as fn()),
        (22, d22::main as fn()),
        (23, d23::main as fn()),
        (24, d24::main as fn()),
        (25, d25::main as fn()),
        
    ] {
        if !Path::new(format!("2021/{:02}.txt", day).as_str()).exists() {continue;}
        println!();
        println!("=== Day {} ===", day);
        func();
    }
}
