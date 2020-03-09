#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct Position {
    row: usize,
    col: usize,
}

impl Position {
    pub fn new(row: usize, col: usize) -> Self {
        Position { row: row, col: col }
    }

    pub fn set_row(&mut self, row: usize) {
        self.row = row;
    }

    pub fn set_col(&mut self, col: usize) {
        self.col = col;
    }
}
