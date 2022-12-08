use aoc::iter_ext::IterExt;

type Input = Vec<Vec<u8>>;

enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    fn step(&self, x: usize, y: usize, width: usize, height: usize) -> Option<(usize, usize)> {
        Some(match self {
            Direction::North if y > 0 => (x, y - 1),
            Direction::East if x < width - 1 => (x + 1, y),
            Direction::South if y < height - 1 => (x, y + 1),
            Direction::West if x > 0 => (x - 1, y),
            _ => return None,
        })
    }

    fn iter() -> std::slice::Iter<'static, Direction> {
        [Self::North, Self::East, Self::South, Self::West].iter()
    }
}

struct CoordIterator<'a> {
    x: usize,
    y: usize,
    width: usize,
    height: usize,
    direction: &'a Direction,
}

impl Iterator for CoordIterator<'_> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        (self.x, self.y) = self
            .direction
            .step(self.x, self.y, self.width, self.height)?;
        Some((self.x, self.y))
    }
}

fn setup(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|line| line.bytes().map(|x| x - b'0').collect())
        .collect()
}

fn part1(grid: &Input) -> usize {
    let (height, width) = (grid.len(), grid[0].len());

    (0..height)
        .flat_map(|y| {
            (0..width).filter(move |&x| {
                Direction::iter().any(|direction| {
                    CoordIterator {
                        x,
                        y,
                        width,
                        height,
                        direction,
                    }
                    .all(|(j, i)| grid[y][x] > grid[i][j])
                })
            })
        })
        .count()
}

fn part2(grid: &Input) -> usize {
    let (height, width) = (grid.len(), grid[0].len());

    (0..height)
        .flat_map(|y| {
            (0..width).map(move |x| {
                Direction::iter()
                    .map(|direction| {
                        CoordIterator {
                            x,
                            y,
                            width,
                            height,
                            direction,
                        }
                        .take_while_inclusive(|&(j, i)| grid[y][x] > grid[i][j])
                        .count()
                    })
                    .product::<usize>()
            })
        })
        .max()
        .unwrap()
}

aoc::main!(2022, 8);
aoc::example!(ex01, "08.1.txt", 21, 8);
