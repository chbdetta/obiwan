use crate::lexer::LexerError;
use crate::parser::ParserError;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    LexerError(LexerError),
    TokenConvertError,
    // Parser error
    ParserError(ParserError),
}
