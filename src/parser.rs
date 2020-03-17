use crate::ast::{binding, expr, stmt, Expr, Op, Program, Stmt};
use crate::error::Error;
use crate::pos::Position;
use crate::token::{Token, TokenType};
use nom::{
    branch::alt,
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ParserError {
    pos: [Position; 2],
    msg: String,
}

impl ParserError {
    fn new(pos: &[Position; 2], msg: &str) -> Self {
        Self {
            pos: *pos,
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
                    msg: format!("Unfinished: {:?} \n {:?}", left_input, statements),
                }))
            }
        }
        Err(err) => Err(Error::ParserError(ParserError {
            pos: Default::default(),
            msg: format!("{:?}", err),
        })),
    }
}

type Input<'a> = &'a [Token];
type ParseResult<'a, T> = IResult<Input<'a>, T>;

fn any_token(input: Input) -> ParseResult<&Token> {
    if input.is_empty() {
        Err(nom::Err::Error((input, nom::error::ErrorKind::Alpha)))
    } else {
        Ok((&input[1..], &input[0]))
    }
}

fn t(name: &'static str) -> impl Fn(Input) -> ParseResult<&Token> {
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

fn tt(tt: TokenType) -> impl Fn(Input) -> ParseResult<&Token> {
    move |input| {
        if input.is_empty() {
            Err(nom::Err::Error((input, nom::error::ErrorKind::Alpha)))
        } else {
            let token = &input[0];
            if token.tt == tt {
                Ok((&input[1..], &input[0]))
            } else {
                Err(nom::Err::Error((input, nom::error::ErrorKind::Alpha)))
            }
        }
    }
}

// actual parsers
fn expr(input: Input) -> ParseResult<Expr> {
    expr_assign(input)
}

macro_rules! bin_op {
    ($next:ident,[$token:literal,$struct:ident]) => {
        map_res(
            pair($next, many0(pair((t($token)), $next))),
            |(a, bs)| -> Result<_, ()> {
                let mut ret = a;

                for (op, b) in bs {
                    ret = match &op.src[..] {
                        $token => Expr::Op(Op::$struct(expr::$struct(Box::new(ret), Box::new(b)))),
                        _ => panic!("Token is wrong"),
                    }
                }

                Ok(ret)
            },
        )
    };
    ($next:ident,[$($token:literal,$struct:ident);*]) => {
        map_res(
            pair($next, many0(pair(alt(($(t($token)),*)), $next))),
            |(a, bs)| -> Result<_, ()> {
                let mut ret = a;

                for (op, b) in bs {
                    ret = match &op.src[..] {
                        $(
                            $token => Expr::Op(Op::$struct(expr::$struct(Box::new(ret), Box::new(b)))),
                        )*
                        _ => panic!("Token is wrong"),
                    }
                }

                Ok(ret)
            },
        )
    };
}

fn expr_assign(input: Input) -> ParseResult<Expr> {
    // FIXME: error when the left hand side is not valid
    bin_op!(expr_cond, [
    "=", Assign;
    "+=", AddAssign;
    "-=", SubAssign;
    "*=", MulAssign;
    "/=", DivAssign
    ])(input)
}

fn expr_cond(input: Input) -> ParseResult<Expr> {
    map_res(
        pair(
            expr_or,
            many0(pair(delimited(t("?"), expr, t(":")), expr_or)),
        ),
        |(a, bs)| -> Result<_, ()> {
            let mut ret = a;

            for (b, c) in bs {
                ret = Expr::Op(Op::Cond(expr::Cond(
                    Box::new(ret),
                    Box::new(b),
                    Box::new(c),
                )))
            }

            Ok(ret)
        },
    )(input)
}

fn expr_or(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_equality, ["||", Or])(input)
}

fn expr_and(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_equality, ["&&", And])(input)
}

fn expr_equality(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_addition, ["==", Equal; "!=", Neq; "===", StrictEq; "!==", StrictNeq])(input)
}

fn expr_comparison(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_addition, ["<", Less; "<=", LessEq; ">", Greater; ">=", GreaterEq; "in", In; "instanceof", InstanceOf])(
        input,
    )
}

fn expr_addition(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_multiplication, ["+", Add; "-", Sub])(input)
}

fn expr_multiplication(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_unary, ["*", Mul; "/", Div; "%", Mod])(input)
}

fn expr_unary(input: Input) -> ParseResult<Expr> {
    alt((
        map_res(
            pair(alt((t("!"), t("-"))), expr_unary),
            |(op, e)| match &op.src[..] {
                "!" => Ok(Expr::Op(Op::Not(expr::Not(Box::new(e))))),
                "-" => Ok(Expr::Op(Op::Neg(expr::Neg(Box::new(e))))),
                _ => Err(nom::Err::Error(nom::error::ErrorKind::Alpha)),
            },
        ),
        expr_primary,
    ))(input)
}

fn expr_primary(input: Input) -> ParseResult<Expr> {
    alt((
        map_res(tt(TokenType::Decimal), |t| -> Result<Expr, ()> {
            // TODO: parse the real number
            Ok(Expr::Number(expr::Number(
                t.literal.clone().unwrap().parse().unwrap(),
            )))
        }),
        map_res(tt(TokenType::String), |t| -> Result<Expr, ()> {
            Ok(Expr::Str(expr::Str(t.literal.clone().unwrap())))
        }),
        map_res(tt(TokenType::Template), |t| -> Result<Expr, ()> {
            Ok(Expr::Template(expr::Template(
                t.literal.clone().unwrap(),
                vec![],
            )))
        }),
        map_res(t("false"), |_| -> Result<Expr, ()> {
            Ok(Expr::False(expr::False))
        }),
        map_res(t("true"), |_| -> Result<Expr, ()> {
            Ok(Expr::True(expr::True))
        }),
        map_res(t("null"), |_| -> Result<Expr, ()> {
            Ok(Expr::Null(expr::Null))
        }),
        map_res(tt(TokenType::Identifier), |t| -> Result<_, ()> {
            Ok(Expr::Identifier(expr::Identifier(t.clone())))
        }),
        map_res(t("this"), |_| -> Result<_, ()> {
            Ok(Expr::This(expr::This))
        }),
        delimited(t("("), expr, t(")")),
    ))(input)
}

fn stmt_list(input: Input) -> ParseResult<Vec<Stmt>> {
    many0(alt((stmt, declr)))(input)
}

fn stmt(input: Input) -> ParseResult<Stmt> {
    alt((stmt_expr, stmt_empty, stmt_if, stmt_debugger, stmt_block))(input)
}

fn stmt_empty(input: Input) -> ParseResult<Stmt> {
    map_res(t(";"), |_| -> Result<_, ()> {
        Ok(Stmt::Empty(stmt::Empty))
    })(input)
}

fn stmt_expr(input: Input) -> ParseResult<Stmt> {
    map_res(terminated(expr, t(";")), |e| -> Result<_, ()> {
        Ok(Stmt::Expr(stmt::Expr(e)))
    })(input)
}

fn declr(input: Input) -> ParseResult<Stmt> {
    map_res(
        terminated(
            pair(
                tuple((
                    alt((t("let"), t("const"), t("var"))),
                    tt(TokenType::Identifier),
                    t("="),
                )),
                expr,
            ),
            t(";"),
        ),
        |((quantifier, name, _), e)| match &quantifier.src[..] {
            "let" => Ok(Stmt::LexicalDeclr(stmt::LexicalDeclr(
                quantifier.clone(),
                binding::Lexical::Ident(binding::SingleName {
                    ident: name.clone(),
                    init: e,
                }),
            ))),
            "const" => Ok(Stmt::LexicalDeclr(stmt::LexicalDeclr(
                quantifier.clone(),
                binding::Lexical::Ident(binding::SingleName {
                    ident: name.clone(),
                    init: e,
                }),
            ))),
            "var" => Ok(Stmt::Var(stmt::Var(binding::Var::Ident(
                binding::SingleName {
                    ident: name.clone(),
                    init: e,
                },
            )))),
            _ => Err(nom::Err::Error(Error::ParserError)),
        },
    )(input)
}

fn stmt_if(input: Input) -> ParseResult<Stmt> {
    map_res(
        pair(
            pair(preceded(t("if"), delimited(t("("), expr, t(")"))), stmt),
            opt(preceded(t("else"), stmt)),
        ),
        |((e, b), s)| -> Result<_, ()> {
            Ok(Stmt::If(stmt::If {
                cond: e,
                body: Box::new(b),
                body_else: s.map(Box::new),
            }))
        },
    )(input)
}

fn stmt_while(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_dowhile(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_for(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_switch(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_continue(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_break(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_return(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_with(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_labeled(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_throw(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_try(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_debugger(input: Input) -> ParseResult<Stmt> {
    map_res(terminated(t("debugger"), t(";")), |_| -> Result<_, ()> {
        Ok(Stmt::Debugger(stmt::Debugger))
    })(input)
}

fn stmt_class(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_function(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_generator(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_lexical(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_block(input: Input) -> ParseResult<Stmt> {
    map_res(
        delimited(t("{"), stmt_list, t("}")),
        |list| -> Result<_, ()> { Ok(Stmt::Block(stmt::Block { stmts: list })) },
    )(input)
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
