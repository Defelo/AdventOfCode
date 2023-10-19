#![feature(test)]

#[derive(Copy, Clone, Eq, PartialEq)]
enum Type {
    Empty,
    Right,
    Down,
}

type Input = Vec<Vec<Type>>;

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    'v' => Type::Down,
                    '>' => Type::Right,
                    _ => Type::Empty,
                })
                .collect()
        })
        .collect()
}

fn part1(input: &Input) -> String {
    let mut grid = input.clone();
    let mut cnt = 0;
    loop {
        let mut moved = false;
        let mut new_grid = grid.clone();
        for (i, line) in grid.iter().enumerate() {
            for (j, &c) in line.iter().enumerate() {
                if c == Type::Right {
                    let k = (j + 1) % line.len();
                    if grid[i][k] == Type::Empty {
                        new_grid[i][k] = Type::Right;
                        new_grid[i][j] = Type::Empty;
                        moved = true;
                    }
                }
            }
        }
        grid = new_grid;
        let mut new_grid = grid.clone();
        for (i, line) in grid.iter().enumerate() {
            for (j, &c) in line.iter().enumerate() {
                if c == Type::Down {
                    let k = (i + 1) % grid.len();
                    if grid[k][j] == Type::Empty {
                        new_grid[k][j] = Type::Down;
                        new_grid[i][j] = Type::Empty;
                        moved = true;
                    }
                }
            }
        }
        grid = new_grid;
        cnt += 1;
        if !moved {
            break;
        }
    }
    cnt.to_string()
}

aoc::main!(2021, 25, ex: 1);
