#![feature(test)]

use aoc::grid::Direction;

struct Input {
    grid: Grid,
    directions: Vec<Direction>,
}

#[derive(Debug, Clone)]
struct Grid {
    width: usize,
    height: usize,
    cells: Vec<Cell>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cell {
    Empty,
    Wall,
    Robot,
    Box,
    BoxLeft,
    BoxRight,
}

impl Grid {
    fn is_pushable(&self, row: usize, col: usize, direction: Direction) -> bool {
        let Some((dst_col, dst_row)) = direction.step(col, row, self.width, self.height) else {
            return self.get(row, col) == Cell::Empty;
        };

        match (self.get(row, col), direction) {
            (Cell::Empty, _) => true,
            (Cell::Wall, _) => false,
            (Cell::Robot | Cell::Box, _) => self.is_pushable(dst_row, dst_col, direction),
            (Cell::BoxLeft | Cell::BoxRight, Direction::East | Direction::West) => {
                self.is_pushable(dst_row, dst_col, direction)
            }
            (Cell::BoxLeft, Direction::North | Direction::South) => {
                self.is_pushable(dst_row, dst_col, direction)
                    && self.is_pushable(dst_row, dst_col + 1, direction)
            }
            (Cell::BoxRight, Direction::North | Direction::South) => {
                self.is_pushable(dst_row, dst_col, direction)
                    && self.is_pushable(dst_row, dst_col - 1, direction)
            }
        }
    }

    fn push(&mut self, row: usize, col: usize, direction: Direction) {
        debug_assert!(self.is_pushable(row, col, direction));

        let (dst_col, dst_row) = direction.step(col, row, self.width, self.height).unwrap();

        match (self.get(row, col), direction) {
            (Cell::Empty, _) => {}
            (Cell::Wall, _) => panic!("Cannot move walls"),
            (cell @ (Cell::Robot | Cell::Box), _) => {
                self.push(dst_row, dst_col, direction);
                self.set(dst_row, dst_col, cell);
                self.set(row, col, Cell::Empty);
            }
            (cell @ (Cell::BoxLeft | Cell::BoxRight), Direction::East | Direction::West) => {
                self.push(dst_row, dst_col, direction);
                self.set(dst_row, dst_col, cell);
                self.set(row, col, Cell::Empty);
            }
            (cell @ Cell::BoxLeft, Direction::North | Direction::South) => {
                debug_assert_eq!(self.get(row, col + 1), Cell::BoxRight);
                self.push(dst_row, dst_col, direction);
                self.push(dst_row, dst_col + 1, direction);
                self.set(dst_row, dst_col, cell);
                self.set(dst_row, dst_col + 1, Cell::BoxRight);
                self.set(row, col, Cell::Empty);
                self.set(row, col + 1, Cell::Empty);
            }
            (Cell::BoxRight, Direction::North | Direction::South) => {
                debug_assert_eq!(self.get(row, col - 1), Cell::BoxLeft);
                self.push(row, col - 1, direction);
            }
        }
    }

    fn execute(&mut self, directions: impl Iterator<Item = Direction>) {
        let Some((mut i, mut j)) = self.coords().find(|&(i, j)| self.get(i, j) == Cell::Robot)
        else {
            return;
        };

        for direction in directions {
            debug_assert_eq!(self.get(i, j), Cell::Robot);
            if self.is_pushable(i, j, direction) {
                self.push(i, j, direction);
                debug_assert_eq!(self.get(i, j), Cell::Empty);
                (j, i) = direction.step(j, i, self.width, self.height).unwrap();
            }
        }
    }

    fn expand(&self) -> Grid {
        let width = self.width * 2;
        let height = self.height;
        let cells = self
            .cells
            .iter()
            .flat_map(|&c| match c {
                Cell::Empty => [Cell::Empty, Cell::Empty],
                Cell::Wall => [Cell::Wall, Cell::Wall],
                Cell::Robot => [Cell::Robot, Cell::Empty],
                Cell::Box | Cell::BoxLeft | Cell::BoxRight => [Cell::BoxLeft, Cell::BoxRight],
            })
            .collect::<Vec<_>>();
        debug_assert_eq!(cells.len(), width * height);
        Grid {
            width,
            height,
            cells,
        }
    }

    fn score(&self) -> usize {
        self.coords()
            .filter(move |&(i, j)| matches!(self.get(i, j), Cell::Box | Cell::BoxLeft))
            .map(move |(i, j)| i * 100 + j)
            .sum()
    }

    fn coords(&self) -> impl Iterator<Item = (usize, usize)> {
        let width = self.width;
        (0..self.height).flat_map(move |i| (0..width).map(move |j| (i, j)))
    }

    fn get(&self, row: usize, col: usize) -> Cell {
        self.cells[self.idx(row, col)]
    }

    fn set(&mut self, row: usize, col: usize, cell: Cell) -> Cell {
        let idx = self.idx(row, col);
        std::mem::replace(&mut self.cells[idx], cell)
    }

    fn idx(&self, row: usize, col: usize) -> usize {
        debug_assert!((0..self.height).contains(&row));
        debug_assert!((0..self.width).contains(&col));
        row * self.width + col
    }
}

impl<I> FromIterator<I> for Grid
where
    I: IntoIterator<Item = Cell>,
{
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let Some(fst) = iter.next() else {
            return Self {
                width: 0,
                height: 0,
                cells: Vec::new(),
            };
        };

        let mut width = 0;
        let mut height = 1;
        let mut cells = Vec::new();
        for c in fst.into_iter() {
            cells.push(c);
            width += 1;
        }

        for row in iter {
            let mut row_width = 0;
            height += 1;
            for c in row.into_iter() {
                cells.push(c);
                row_width += 1;
            }
            debug_assert_eq!(row_width, width);
        }

        Self {
            width,
            height,
            cells,
        }
    }
}

fn setup(input: &str) -> Input {
    let mut lines = input.trim().lines();

    let grid = lines
        .by_ref()
        .take_while(|l| !l.is_empty())
        .map(|l| {
            l.trim().bytes().map(|b| match b {
                b'.' => Cell::Empty,
                b'#' => Cell::Wall,
                b'@' => Cell::Robot,
                b'O' => Cell::Box,
                b'[' => Cell::BoxLeft,
                b']' => Cell::BoxRight,
                _ => panic!(),
            })
        })
        .collect();

    let directions = lines.flat_map(|l| l.chars()).map(Into::into).collect();

    Input { grid, directions }
}

fn part1(input: &Input) -> usize {
    let mut grid = input.grid.clone();
    grid.execute(input.directions.iter().copied());
    grid.score()
}

fn part2(input: &Input) -> usize {
    let mut grid = input.grid.expand();
    grid.execute(input.directions.iter().copied());
    grid.score()
}

aoc::main!(2024, 15, ex: 1, 2);
