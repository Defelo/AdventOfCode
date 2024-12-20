#![feature(test)]

use std::collections::VecDeque;

use aoc::grid::Direction;

#[derive(Debug)]
struct Input {
    end: (usize, usize),
    grid: Vec<Vec<bool>>,
}

fn setup(input: &str) -> Input {
    let mut end = None;
    let grid = input
        .trim()
        .lines()
        .enumerate()
        .map(|(y, l)| {
            l.bytes()
                .enumerate()
                .map(|(x, b)| match b {
                    b'.' => true,
                    b'#' => false,
                    b'S' => true,
                    b'E' => {
                        end = Some((x, y));
                        true
                    }
                    _ => panic!(),
                })
                .collect()
        })
        .collect();

    Input {
        end: end.unwrap(),
        grid,
    }
}

fn solve<const N: usize>(input: &Input) -> usize {
    let n = N as isize;
    let width = input.grid[0].len();
    let height = input.grid.len();

    let mut shortest = vec![usize::MAX; width * height];
    let mut queue = VecDeque::from([(0, input.end)]);
    let mut visited = vec![false; width * height];
    while let Some((d, (x, y))) = queue.pop_front() {
        let idx = y * width + x;
        if std::mem::replace(&mut visited[idx], true) {
            continue;
        }

        shortest[idx] = d;

        queue.extend(
            Direction::iter()
                .flat_map(|d| d.step(x, y, width, height))
                .filter(|&(nx, ny)| input.grid[ny][nx])
                .map(|q| (d + 1, q)),
        );
    }

    let mut out = 0;
    for csy in 0..height {
        for csx in 0..width {
            if !input.grid[csy][csx] {
                continue;
            }

            for dy in -n..=n {
                let n = n - dy.abs();
                for dx in -n..=n {
                    let cex = csx as isize + dx;
                    let cey = csy as isize + dy;
                    if !(0..width as isize).contains(&cex) || !(0..height as isize).contains(&cey) {
                        continue;
                    }

                    let cex = cex as usize;
                    let cey = cey as usize;
                    if !input.grid[cey][cex] {
                        continue;
                    }

                    let d = (dx.abs() + dy.abs()) as usize;
                    if shortest[cey * width + cex] + d + 100 <= shortest[csy * width + csx] {
                        out += 1;
                    }
                }
            }
        }
    }

    out
}

fn part1(input: &Input) -> usize {
    solve::<2>(input)
}

fn part2(input: &Input) -> usize {
    solve::<20>(input)
}

aoc::main!(2024, 20);
