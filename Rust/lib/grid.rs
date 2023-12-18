#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    pub fn step(self, x: usize, y: usize, width: usize, height: usize) -> Option<(usize, usize)> {
        Some(match self {
            Direction::North if y > 0 => (x, y - 1),
            Direction::East if x < width - 1 => (x + 1, y),
            Direction::South if y < height - 1 => (x, y + 1),
            Direction::West if x > 0 => (x - 1, y),
            _ => return None,
        })
    }

    pub fn step_signed(self, (x, y): (isize, isize)) -> (isize, isize) {
        match self {
            Direction::North => (x, y - 1),
            Direction::East => (x + 1, y),
            Direction::South => (x, y + 1),
            Direction::West => (x - 1, y),
        }
    }

    pub fn step_signed_n(self, (x, y): (isize, isize), n: isize) -> (isize, isize) {
        match self {
            Direction::North => (x, y - n),
            Direction::East => (x + n, y),
            Direction::South => (x, y + n),
            Direction::West => (x - n, y),
        }
    }

    pub fn invert(self) -> Self {
        match self {
            Direction::North => Direction::South,
            Direction::East => Direction::West,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
        }
    }

    pub fn rotate_left(self) -> Self {
        match self {
            Direction::North => Direction::West,
            Direction::East => Direction::North,
            Direction::South => Direction::East,
            Direction::West => Direction::South,
        }
    }

    pub fn rotate_right(self) -> Self {
        match self {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        }
    }

    pub fn iter() -> impl Iterator<Item = Self> {
        [Self::North, Self::East, Self::South, Self::West].into_iter()
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
