use crate::lexer::LexerError;

#[derive(Debug, PartialEq, Clone)]
pub enum Error<'a> {
    LexerError(LexerError<'a>),
    TokenConvertError,
}
