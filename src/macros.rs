macro_rules! pos {
    ($row:expr, $col:expr) => {
        crate::pos::Position::new($row, $col)
    };
}

macro_rules! token {
    ($name:literal) => {
        crate::token::Token::from_str($name).unwrap()
    };
    ($name:ident) => {
        crate::token::Token::from_str($name).unwrap()
    };
}
