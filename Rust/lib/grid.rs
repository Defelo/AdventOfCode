#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    pub fn step(&self, x: usize, y: usize, width: usize, height: usize) -> Option<(usize, usize)> {
        Some(match self {
            Direction::North if y > 0 => (x, y - 1),
            Direction::East if x < width - 1 => (x + 1, y),
            Direction::South if y < height - 1 => (x, y + 1),
            Direction::West if x > 0 => (x - 1, y),
            _ => return None,
        })
    }

    pub fn step_signed(&self, (x, y): (isize, isize)) -> (isize, isize) {
        match self {
            Direction::North => (x, y - 1),
            Direction::East => (x + 1, y),
            Direction::South => (x, y + 1),
            Direction::West => (x - 1, y),
        }
    }

    pub fn iter() -> std::slice::Iter<'static, Direction> {
        [Self::North, Self::East, Self::South, Self::West].iter()
    }
}

impl From<char> for Direction {
    fn from(c: char) -> Self {
        match c {
            'U' | 'N' => Self::North,
            'R' | 'E' => Self::East,
            'D' | 'S' => Self::South,
            'L' | 'W' => Self::West,
            _ => panic!(),
        }
    }
}
