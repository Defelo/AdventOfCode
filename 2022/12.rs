use std::collections::VecDeque;

#[derive(Clone, PartialEq, Eq)]
struct Position {
    x: usize,
    y: usize,
}

impl Position {
    fn neighbors(&self) -> Vec<Position> {
        [(1, 2), (1, 0), (2, 1), (0, 1)]
            .into_iter()
            .filter(|&(dx, dy)| self.x + dx > 0 && self.y + dy > 0)
            .map(|(dx, dy)| Position {
                x: self.x + dx - 1,
                y: self.y + dy - 1,
            })
            .collect()
    }
}

struct Input {
    start: Position,
    end: Position,
    grid: Vec<Vec<u8>>,
}

fn setup(input: &str) -> Input {
    let mut grid = Vec::new();
    let mut start = None;
    let mut end = None;
    for (i, line) in input.trim().lines().enumerate() {
        let mut row = Vec::new();
        for (j, c) in line.bytes().enumerate() {
            let c = match c {
                b'S' => {
                    start = Some(Position { x: j, y: i });
                    0
                }
                b'E' => {
                    end = Some(Position { x: j, y: i });
                    25
                }
                _ => c - b'a',
            };
            row.push(c);
        }
        grid.push(row);
    }
    Input {
        start: start.unwrap(),
        end: end.unwrap(),
        grid,
    }
}

fn bfs(
    start: Position,
    grid: &[&[u8]],
    target: impl Fn(&Position) -> bool,
    step: impl Fn(&Position, &Position) -> bool,
) -> u32 {
    let mut queue = VecDeque::from([(0, start)]);
    let mut visited = vec![vec![false; grid[0].len()]; grid.len()];
    while let Some((d, p)) = queue.pop_front() {
        if target(&p) {
            return d;
        }
        if visited[p.y][p.x] {
            continue;
        }
        visited[p.y][p.x] = true;
        for q in p.neighbors() {
            if grid.get(q.y).and_then(|r| r.get(q.x)).is_some()
                && step(&p, &q)
                && !visited[q.y][q.x]
            {
                queue.push_back((d + 1, q));
            }
        }
    }
    panic!()
}

fn part1(Input { start, end, grid }: &Input) -> u32 {
    bfs(
        start.clone(),
        &grid.iter().map(|x| &x[..]).collect::<Vec<_>>()[..],
        |pos| pos == end,
        |from, to| grid[to.y][to.x] <= grid[from.y][from.x] + 1,
    )
}

fn part2(Input { end, grid, .. }: &Input) -> u32 {
    bfs(
        end.clone(),
        &grid.iter().map(|x| &x[..]).collect::<Vec<_>>()[..],
        |pos| grid[pos.y][pos.x] == 0,
        |from, to| grid[from.y][from.x] <= grid[to.y][to.x] + 1,
    )
}

aoc::main!(2022, 12);
aoc::example!(ex01, "12.1.txt", 31, 29);
aoc::test_input!("12.txt", 380, 375);
