macro_rules! range {
    ($start:expr,$end:expr) => {
        crate::pos::Range(
            crate::pos::Position::new($start),
            crate::pos::Position::new($end),
        )
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
