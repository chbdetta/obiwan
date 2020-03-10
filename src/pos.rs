use std::ops::Add;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Default)]
pub struct Position {
    pub row: usize,
    pub col: usize,
}

impl Position {
    pub fn new(row: usize, col: usize) -> Self {
        Position { row: row, col: col }
    }
}

impl Add<Self> for Position {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        pos!(self.row + other.row, self.col + other.col)
    }
}
