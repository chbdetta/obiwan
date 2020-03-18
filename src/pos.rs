use std::ops::Add;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Default)]
pub struct Position {
    pub index: usize,
}

impl Position {
    pub fn new(index: usize) -> Self {
        Position { index: index }
    }
}

impl Add<Self> for Position {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self::new(self.index + other.index)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Default)]
pub struct Range(pub Position, pub Position);

impl Add<Self> for Range {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Range(
            Position::new(std::cmp::min(self.0.index, self.0.index)),
            Position::new(std::cmp::max(self.1.index, self.1.index)),
        )
    }
}
