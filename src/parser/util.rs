pub use crate::error::Error;
pub use crate::pos::Position;
pub use crate::token::{Token, TokenType};
pub use nom::{
    branch::alt,
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

pub type Input<'a> = &'a [Token];
pub type ParseResult<'a, T> = IResult<Input<'a>, T>;

pub fn any_token(input: Input) -> ParseResult<&Token> {
    if input.is_empty() {
        Err(nom::Err::Error((input, nom::error::ErrorKind::Alpha)))
    } else {
        Ok((&input[1..], &input[0]))
    }
}

// For reserved token
pub fn t(name: &'static str) -> impl Fn(Input) -> ParseResult<&Token> {
    let tk = token!(name);
    move |input| {
        if input.is_empty() {
            Err(nom::Err::Error((input, nom::error::ErrorKind::Alpha)))
        } else {
            let token = &input[0];
            if token == &tk {
                Ok((&input[1..], token))
            } else {
                Err(nom::Err::Error((input, nom::error::ErrorKind::Alpha)))
            }
        }
    }
}

pub fn tt(tt: TokenType) -> impl Fn(Input) -> ParseResult<&Token> {
    t_is(move |t| t.tt == tt)
}

pub fn t_is<F>(f: F) -> impl Fn(Input) -> ParseResult<&Token>
where
    F: Fn(&Token) -> bool,
{
    move |input| {
        if input.is_empty() {
            Err(nom::Err::Error((input, nom::error::ErrorKind::Alpha)))
        } else {
            let token = &input[0];
            if f(token) {
                Ok((&input[1..], &input[0]))
            } else {
                Err(nom::Err::Error((input, nom::error::ErrorKind::Alpha)))
            }
        }
    }
}

pub fn t_elision(input: Input) -> ParseResult<&Token> {
    unimplemented!();
}
