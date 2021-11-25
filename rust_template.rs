type Input = String;

fn get_input() -> Input {
    let puzzle = include_str!("XX.txt");
    return puzzle.to_string();
}

fn part1(input: &Input) -> String {
    panic!();
}

fn part2(input: &Input) -> String {
    panic!();
}

fn main() {
    let input = get_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
