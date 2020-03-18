use crate::ast::Program;
use crate::codegen::Codegen;
use crate::error::Error;
use crate::pos::Range;

mod arg;
mod expr;
mod stmt;
mod util;

use stmt::stmt_list;
use util::*;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ParserError {
    pos: Range,
    msg: String,
}

impl ParserError {
    fn new(pos: Range, msg: &str) -> Self {
        Self {
            pos: pos,
            msg: String::from(msg),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Program, Error> {
    match stmt_list(tokens) {
        Ok((left_input, statements)) => {
            if left_input.is_empty() {
                Ok(Program::Script(statements))
            } else {
                Err(Error::ParserError(ParserError {
                    pos: left_input[0].pos,
                    msg: format!("Unfinished: \"{}\"", left_input.to_code()),
                }))
            }
        }
        Err(err) => {
            eprintln!("{:#?}", err);
            Err(Error::ParserError(ParserError {
                pos: Default::default(),
                msg: format!("{:?}", err),
            }))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_match_lit() {
        let mut lexer = Lexer::new(
            r"
        switch (a) {
            let b = 12;
        }
        ",
        );

        assert_eq!(
            t("switch")(&lexer.parse().unwrap()),
            Ok((
                &Lexer::new(r"(a) {let b = 12;}").parse().unwrap()[..],
                &token!("switch")
            ))
        );
    }

    fn test_match_pair() {
        let mut lexer = Lexer::new(
            r"
        switch (a) {
            let b = 12;
        }
        ",
        );

        assert_eq!(
            pair(t("switch"), t("{"))(&lexer.parse().unwrap()),
            Ok((
                &Lexer::new(r"a) {let b = 12;}").parse().unwrap()[..],
                (&token!("switch"), &token!("("))
            ))
        );
    }
}
