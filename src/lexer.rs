use crate::error::Error;
use crate::pos::Position;
use crate::token::{Token, TokenType};
use std::str;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LexerError {
    src: String,
    pos: Position,
    msg: &'static str,
}

pub struct Lexer<'a> {
    src: &'a [u8],
    start: usize,
    cur: usize,
    pos: Position,
    tokens: Vec<Token<'a>>,
    error: Option<Error>,
}

fn is_whitespace(c: &u8) -> bool {
    c.is_ascii_whitespace()
}

fn is_line_feed(c: &u8) -> bool {
    *c == b'\n' || *c == b'\r'
}

fn is_digit(c: &u8) -> bool {
    c.is_ascii_digit()
}

fn is_alpha(c: &u8) -> bool {
    c.is_ascii_alphabetic()
}

fn is_alphanumeric(c: &u8) -> bool {
    c.is_ascii_alphanumeric()
}

fn is_identifier(c: &u8) -> bool {
    is_alphanumeric(c) || *c == b'_' || *c == b'$'
}

fn is_identifier_start(c: &u8) -> bool {
    is_alpha(c) || *c == b'_' || *c == b'$'
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let bytes = src.as_bytes();

        Lexer {
            src: bytes,
            cur: 0,
            start: 0,
            pos: Position::new(0, 0),
            tokens: vec![],
            error: None,
        }
    }

    fn next(&mut self) -> Option<&'a u8> {
        let el = self.src.get(self.cur);

        if let Some(_) = el {
            self.cur += 1;
        }

        el
    }

    fn peek(&self) -> Option<&'a u8> {
        self.src.get(self.cur)
    }

    fn consume(&mut self) {
        self.start = self.cur;
    }

    fn is_ended(&self) -> bool {
        self.cur >= self.src.len()
    }

    fn next_is<F>(&mut self, pred: F) -> bool
    where
        F: FnOnce(&u8) -> bool,
    {
        if let Some(a) = self.peek() {
            if pred(a) {
                self.next();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn next_match(&mut self, c: u8) -> bool {
        self.next_is(|&a| c == a)
    }

    fn src_seg<'b>(&'b self) -> &'a str {
        str::from_utf8(&self.src[self.start..self.cur]).unwrap()
    }

    fn src_seg_offset<'b>(&'b self, offset: usize) -> &'a str {
        str::from_utf8(&self.src[self.start + offset..self.cur - offset]).unwrap()
    }

    fn add_token(&mut self, tt: TokenType) {
        self._add_token(tt, None);
    }

    fn add_lit_token(&mut self, tt: TokenType, lit: &'a str) {
        self._add_token(tt, Some(lit));
    }

    fn _add_token(&mut self, tt: TokenType, lit: Option<&'a str>) {
        if self.start < self.src.len() {
            let src_seg = self.src_seg();

            self.tokens.push(Token::new(tt, None, Some(src_seg), lit));

            self.consume();
        }
    }

    fn error(&mut self, msg: &'static str) -> Error {
        let error = Error::LexerError(LexerError {
            pos: self.pos.clone(),
            src: String::from(self.src_seg()),
            msg: msg,
        });

        self.error = Some(error.clone());

        error
    }

    pub fn parse<'b>(&'b mut self) -> Result<&'b Vec<Token<'a>>, Error> {
        while let Some(c) = self.next() {
            match *c {
                b'!' => {
                    let symbol = if self.next_match(b'=') {
                        if self.next_match(b'=') {
                            "!=="
                        } else {
                            "!="
                        }
                    } else {
                        "!"
                    };
                    self.add_token(TokenType::from_str(symbol).unwrap())
                }
                b'.' => {
                    if self.next_match(b'.') {
                        if self.next_match(b'.') {
                            self.add_token(TokenType::from_str(".").unwrap());
                        } else {
                            self.error("Unexpected character");
                        }
                    } else if self.next_is(is_digit) {
                        // **This is a decimal**
                        self.number()?;
                    } else {
                        self.add_token(TokenType::from_str(".").unwrap());
                    }
                }
                b'<' => {
                    let symbol = if self.next_match(b'=') {
                        "<="
                    } else if self.next_match(b'<') {
                        if self.next_match(b'=') {
                            "<<="
                        } else {
                            "<<"
                        }
                    } else {
                        "<"
                    };
                    self.add_token(TokenType::from_str(symbol).unwrap());
                }
                b'>' => {
                    let symbol = if self.next_match(b'=') {
                        ">="
                    } else if self.next_match(b'>') {
                        if self.next_match(b'=') {
                            ">>="
                        } else if self.next_match(b'>') {
                            if self.next_match(b'=') {
                                ">>>="
                            } else {
                                ">>>"
                            }
                        } else {
                            ">>"
                        }
                    } else {
                        ">"
                    };
                    self.add_token(TokenType::from_str(symbol).unwrap());
                }
                b'=' => {
                    let symbol = if self.next_match(b'=') {
                        if self.next_match(b'=') {
                            "==="
                        } else {
                            "=="
                        }
                    } else if self.next_match(b'>') {
                        "=>"
                    } else {
                        "="
                    };

                    self.add_token(TokenType::from_str(symbol).unwrap())
                }
                c @ b'+' | c @ b'-' | c @ b'&' | c @ b'|' => {
                    let symbol = if self.next_match(b'=') {
                        self.add_token(TokenType::from_bytes(&[c, b'=']).unwrap())
                    } else if self.next_match(c) {
                        self.add_token(TokenType::from_bytes(&[c, c]).unwrap())
                    } else {
                        self.add_token(TokenType::from_bytes(&[c]).unwrap())
                    };
                }
                c @ b'*' | c @ b'/' | c @ b'^' | c @ b'%' => {
                    if self.next_match(b'=') {
                        self.add_token(TokenType::from_bytes(&[c, b'=']).unwrap())
                    } else {
                        self.add_token(TokenType::from_bytes(&[c]).unwrap())
                    }
                }

                c @ b'{'
                | c @ b'}'
                | c @ b'('
                | c @ b')'
                | c @ b'['
                | c @ b']'
                | c @ b';'
                | c @ b','
                | c @ b'~'
                | c @ b'?'
                | c @ b':' => self.add_token(TokenType::from_bytes(&[c]).unwrap()),

                b'"' => self.string(b'"')?,
                b'\'' => self.string(b'\'')?,
                b'`' => self.template()?,
                _ => {
                    if is_whitespace(c) {
                        // TODO: we should also update the position
                        self.consume();
                    } else if is_digit(c) {
                        self.number()?;
                    } else if is_identifier_start(c) {
                        self.identifier()?;
                    } else {
                        return Err(self.error("Unexpected character"));
                    }
                }
            }
        }

        Ok(&self.tokens)
    }

    fn string(&mut self, quote: u8) -> Result<(), Error> {
        while let Some(&c) = self.peek() {
            if c == quote {
                break;
            }

            self.next();
        }

        if self.is_ended() {
            // This is an error, string literal should close
            Err(self.error("String literal unclosed"))
        } else {
            // The closing quote
            self.next();
            self.add_lit_token(TokenType::String, self.src_seg_offset(1));

            Ok(())
        }
    }

    fn template(&mut self) -> Result<(), Error> {
        unimplemented!()
    }

    fn number(&mut self) -> Result<(), Error> {
        unimplemented!()
        // let tt = TokenType::Decimal;

        // while let Some(c) = self.peek() {
        //     if !is_digit(c) || c != b'.' {}
        // }

        // Ok(())
    }

    fn identifier(&mut self) -> Result<(), Error> {
        let tt = TokenType::Identifier;

        while let Some(c) = self.peek() {
            if !is_identifier(c) {
                break;
            }

            self.next();
        }

        self.add_token(tt);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Error;
    use crate::pos::Position;
    use crate::token::{Token, TokenType};

    #[test]
    fn boolean_single_true() {
        assert_eq!(
            Lexer::new("true").parse(),
            Ok(&Token::from_vec(vec!["true"]).unwrap())
        )
    }

    #[test]
    fn boolean_single_false() {
        assert_eq!(
            Lexer::new("false").parse(),
            Ok(&Token::from_vec(vec!["false"]).unwrap())
        )
    }

    #[test]
    fn boolean_single_False() {
        assert_ne!(
            Lexer::new("False").parse(),
            Ok(&Token::from_vec(vec!["false"]).unwrap())
        )
    }

    #[test]
    fn boolean_single_True() {
        assert_ne!(
            Lexer::new("True").parse(),
            Ok(&Token::from_vec(vec!["true"]).unwrap())
        )
    }

    #[test]
    fn boolean_multiple() {
        assert_eq!(
            Lexer::new("true false true false").parse(),
            Ok(&Token::from_vec(vec!["true", "false", "true", "false"]).unwrap())
        )
    }

    #[test]
    fn reserve() {
        assert_eq!(
            Lexer::new("this await in break do    null").parse(),
            Ok(&Token::from_vec(vec!["this", "await", "in", "break", "do", "null"]).unwrap())
        )
    }

    #[test]
    fn linefeed() {
        assert_eq!(
            Lexer::new(
                r"
            this
            in break   
            while if"
            )
            .parse(),
            Ok(&Token::from_vec(vec!["this", "in", "break", "while", "if"]).unwrap())
        )
    }

    #[test]
    fn operators() {
        assert_eq!(
            Lexer::new(r"+++= ===!=====)([]/==>))<=>").parse(),
            Ok(&Token::from_vec(vec![
                "++", "+=", "===", "!==", "===", ")", "(", "[", "]", "/=", "=>", ")", ")", "<=",
                ">"
            ])
            .unwrap())
        )
    }

    fn integer() {
        assert_eq!(
            Lexer::new(r"1234567890").parse(),
            Ok(&vec![Token::new_lit(TokenType::Decimal, "1234567890")])
        )
    }

    #[test]
    fn numbers() {
        assert_eq!(
            Lexer::new(r"12314 123.231 1223e12 .12992 0.12341 12312. 122.2e8").parse(),
            Ok(&vec![
                Token::new_lit(TokenType::Decimal, "12314"),
                Token::new_lit(TokenType::Decimal, "123.231"),
                Token::new_lit(TokenType::Decimal, "1223e12"),
                Token::new_lit(TokenType::Decimal, ".12992"),
                Token::new_lit(TokenType::Decimal, "0.12341"),
                Token::new_lit(TokenType::Decimal, "12312."),
                Token::new_lit(TokenType::Decimal, "122.2e8")
            ])
        )
    }

    #[test]
    fn string_double_quote() {
        assert_eq!(
            Lexer::new(r#""This is really fun""#).parse(),
            Ok(&vec![Token::new_lit(
                TokenType::String,
                "This is really fun"
            )])
        )
    }

    #[test]
    fn string_single_quote() {
        assert_eq!(
            Lexer::new(r#"'This is really fun'"#).parse(),
            Ok(&vec![Token::new_lit(
                TokenType::String,
                "This is really fun"
            )])
        )
    }

    #[test]
    fn string_include_reserve() {
        assert_eq!(
            Lexer::new(r#""this is in my switch while""#).parse(),
            Ok(&vec![Token::new_lit(
                TokenType::String,
                "this is in my switch while"
            )])
        )
    }

    #[test]
    fn string_unclosed() {
        assert_eq!(
            Lexer::new(r#""Hello world!"#).parse(),
            Err(Error::LexerError(LexerError {
                pos: Position::new(0, 0),
                src: String::from("\"Hello world!"),
                msg: "String literal unclosed"
            }))
        )
    }
}
