type Input = Vec<i32>;

fn get_input() -> Input {
    let puzzle = include_str!("01.txt");
    return puzzle.lines().map(|x|x.parse().unwrap()).collect();
}

fn part1(input: &Input) -> String {
    let mut out = 0;
    for (a, b) in input.iter().zip(&input[1..]) {
        if b > a {
            out += 1;
        }
    }
    out.to_string()
}

fn part2(input: &Input) -> String {
    let mut out = 0;
    for (a, b) in input.iter().zip(&input[3..]) {
        if b > a {
            out += 1;
        }
    }
    out.to_string()
}

fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
