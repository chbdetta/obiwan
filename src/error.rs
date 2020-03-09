use crate::lexer::LexerError;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    LexerError(LexerError),
    TokenConvertError,
}
