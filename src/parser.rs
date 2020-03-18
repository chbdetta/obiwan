use crate::ast::{binding, expr, stmt, Argument, Arguments, Expr, Program, Stmt};
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
        Err(err) => {
            eprintln!("{:#?}", err);
            Err(Error::ParserError(ParserError {
                pos: Default::default(),
                msg: format!("{:?}", err),
            }))
        }
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

// For reserved token
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
    t_is(move |t| t.tt == tt)
}

fn t_is<F>(f: F) -> impl Fn(Input) -> ParseResult<&Token>
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

fn t_elision(input: Input) -> ParseResult<&Token> {
    unimplemented!();
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
                        $token => Expr::$struct(expr::$struct(Box::new(ret), Box::new(b))),
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
                            $token => Expr::$struct(expr::$struct(Box::new(ret), Box::new(b))),
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
    map_res(
        pair(
            many0(pair(
                expr_left_hand_side,
                alt((t("="), t("+="), t("-="), t("*="), t("/="))),
            )),
            expr_cond,
        ),
        |(mut assigns, e)| {
            let mut r = e;
            assigns.reverse();

            for (l, op) in assigns {
                r = match &op.src[..] {
                    "=" => Expr::Assign(expr::Assign(Box::new(l), Box::new(r))),
                    "+=" => Expr::AddAssign(expr::AddAssign(Box::new(l), Box::new(r))),
                    "-=" => Expr::SubAssign(expr::SubAssign(Box::new(l), Box::new(r))),
                    "*=" => Expr::MulAssign(expr::MulAssign(Box::new(l), Box::new(r))),
                    "/=" => Expr::DivAssign(expr::DivAssign(Box::new(l), Box::new(r))),
                    _ => return Err(nom::Err::Error((input, nom::error::ErrorKind::Alpha))),
                }
            }

            Ok(r)
        },
    )(input)
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
                ret = Expr::Cond(expr::Cond(Box::new(ret), Box::new(b), Box::new(c)))
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
                "!" => Ok(Expr::Not(expr::Not(Box::new(e)))),
                "-" => Ok(Expr::Neg(expr::Neg(Box::new(e)))),
                _ => Err(nom::Err::Error(nom::error::ErrorKind::Alpha)),
            },
        ),
        expr_left_hand_side,
    ))(input)
}

fn expr_left_hand_side(input: Input) -> ParseResult<Expr> {
    alt((expr_new, expr_call))(input)
}

fn expr_new(input: Input) -> ParseResult<Expr> {
    alt((
        map(preceded(t("new"), expr_new), |e| {
            Expr::New(expr::New(Box::new(e)))
        }),
        expr_member,
    ))(input)
}

fn expr_member(input: Input) -> ParseResult<Expr> {
    map(
        pair(
            alt((expr_primary, expr_member_super_meta)),
            many0(alt((
                map(delimited(t("["), expr, t("]")), |a| (a, true)),
                map(preceded(t("."), expr_ident), |a| (a, false)),
                map(expr_template, |a| (a, false)),
            ))),
        ),
        |(a, bs)| {
            let mut ret = a;
            for (b, computed) in bs {
                ret = Expr::Member(expr::Member {
                    object: Box::new(ret),
                    property: Box::new(b),
                    computed: computed,
                })
            }

            ret
        },
    )(input)
}

fn expr_member_super_meta(input: Input) -> ParseResult<Expr> {
    alt((
        map(preceded(t("super"), delimited(t("["), expr, t("]"))), |e| {
            Expr::Member(expr::Member {
                object: Box::new(Expr::Super(expr::Super)),
                property: Box::new(e),
                computed: true,
            })
        }),
        // FIXME: Might regret this, treating the second member as a identifier.
        // See what would happen when evaluate the result
        map(
            preceded(
                alt((t("super"), t("new"))),
                preceded(t("."), expr_identname),
            ),
            |e| {
                Expr::Member(expr::Member {
                    object: Box::new(Expr::Super(expr::Super)),
                    property: Box::new(e),
                    computed: false,
                })
            },
        ),
    ))(input)
}

fn expr_call(input: Input) -> ParseResult<Expr> {
    map(
        pair(
            alt((
                pair(expr_member, arguments),
                pair(map(t("super"), |_| Expr::Super(expr::Super)), arguments),
            )),
            many0(arguments),
        ),
        |((callee, args), args_list)| {
            let mut ret = Expr::Call(expr::Call {
                callee: Box::new(callee),
                arguments: args,
            });

            for a in args_list {
                ret = Expr::Call(expr::Call {
                    callee: Box::new(ret),
                    arguments: a,
                })
            }

            ret
        },
    )(input)
}

fn arguments(input: Input) -> ParseResult<Arguments> {
    map(
        delimited(
            t("("),
            pair(
                pair(opt(t("...")), expr_assign),
                many0(preceded(t(","), pair(opt(t("...")), expr_assign))),
            ),
            t(")"),
        ),
        |((r1, e), args)| {
            let mut ret = vec![Argument {
                assign: Box::new(e),
                spread: r1.is_some(),
            }];

            for (r, arg) in args {
                ret.push(Argument {
                    assign: Box::new(arg),
                    spread: r.is_some(),
                })
            }

            ret
        },
    )(input)
}

fn expr_literal(input: Input) -> ParseResult<Expr> {
    map_res(
        alt((
            tt(TokenType::Decimal),
            tt(TokenType::String),
            t("false"),
            t("true"),
            t("null"),
        )),
        |tk| {
            let value = match tk.tt {
                TokenType::False => expr::LiteralValue::Boolean(false),
                TokenType::True => expr::LiteralValue::Boolean(true),
                TokenType::Null => expr::LiteralValue::Null,
                TokenType::Decimal => expr::LiteralValue::Number(
                    tk.literal
                        .clone()
                        .unwrap()
                        .parse()
                        .expect("Only support i32 now"),
                ),
                TokenType::String => expr::LiteralValue::String(tk.literal.clone().unwrap()),
                _ => return Err(nom::Err::Error((input, nom::error::ErrorKind::Alpha))),
            };

            Ok(Expr::Literal(expr::Literal {
                token: tk.clone(),
                value: value,
            }))
        },
    )(input)
}

fn expr_primary(input: Input) -> ParseResult<Expr> {
    alt((
        expr_literal,
        expr_template,
        map(t("this"), |_| Expr::This(expr::This)),
        expr_ident,
        delimited(t("("), expr, t(")")),
    ))(input)
}

fn expr_template(input: Input) -> ParseResult<Expr> {
    map(tt(TokenType::Template), |t| {
        Expr::Template(expr::Template(t.literal.clone().unwrap(), vec![]))
    })(input)
}

fn expr_ident(input: Input) -> ParseResult<Expr> {
    map(tt(TokenType::Identifier), |t| {
        Expr::Ident(expr::Ident { name: t.clone() })
    })(input)
}

fn expr_identname(input: Input) -> ParseResult<Expr> {
    map(
        t_is(|token| token.tt == TokenType::Identifier || token.is_reserve()),
        |token| {
            Expr::Ident(expr::Ident {
                name: token.clone(),
            })
        },
    )(input)
}

fn stmt_list(input: Input) -> ParseResult<Vec<Stmt>> {
    many0(alt((stmt, declr_lexical)))(input)
}

fn stmt(input: Input) -> ParseResult<Stmt> {
    alt((
        stmt_expr,
        stmt_empty,
        stmt_if,
        stmt_while,
        stmt_dowhile,
        stmt_for,
        stmt_break,
        stmt_continue,
        stmt_return,
        stmt_var,
        stmt_debugger,
        stmt_block,
    ))(input)
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

fn stmt_var(input: Input) -> ParseResult<Stmt> {
    map_res(
        terminated(
            pair(preceded(t("var"), expr_ident), preceded(t("="), expr)),
            t(";"),
        ),
        |(ident, e)| -> Result<_, ()> {
            Ok(Stmt::Var(stmt::Var(binding::Var::Ident(
                binding::SingleNameBinding {
                    ident: ident,
                    init: Some(e),
                },
            ))))
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
    map_res(
        pair(preceded(t("while"), delimited(t("("), expr, t(")"))), stmt),
        |(e, b)| -> Result<_, ()> {
            Ok(Stmt::While(stmt::While {
                cond: e,
                body: Box::new(b),
            }))
        },
    )(input)
}

fn stmt_dowhile(input: Input) -> ParseResult<Stmt> {
    map_res(
        pair(
            preceded(t("do"), stmt),
            preceded(t("while"), delimited(t("("), expr, t(")"))),
        ),
        |(b, e)| -> Result<_, ()> {
            Ok(Stmt::DoWhile(stmt::DoWhile {
                cond: e,
                body: Box::new(b),
            }))
        },
    )(input)
}

fn stmt_for(input: Input) -> ParseResult<Stmt> {
    map_res(
        pair(
            preceded(
                t("for"),
                delimited(t("("), tuple((expr, expr, expr)), t(")")),
            ),
            stmt,
        ),
        |((e1, e2, e3), s)| -> Result<_, ()> {
            Ok(Stmt::For(stmt::For {
                init: e1,
                cond: e2,
                modi: e3,
                body: Box::new(s),
            }))
        },
    )(input)
}

fn stmt_switch(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_continue(input: Input) -> ParseResult<Stmt> {
    map_res(
        terminated(
            preceded(t("continue"), opt(tt(TokenType::Identifier))),
            t(";"),
        ),
        |tk| -> Result<_, ()> {
            Ok(Stmt::Continue(stmt::Continue {
                label: tk.map(|k| Expr::Ident(expr::Ident { name: k.clone() })),
            }))
        },
    )(input)
}

fn stmt_break(input: Input) -> ParseResult<Stmt> {
    map_res(
        terminated(preceded(t("break"), opt(tt(TokenType::Identifier))), t(";")),
        |tk| -> Result<_, ()> {
            Ok(Stmt::Break(stmt::Break {
                label: tk.map(|k| Expr::Ident(expr::Ident { name: k.clone() })),
            }))
        },
    )(input)
}

fn stmt_return(input: Input) -> ParseResult<Stmt> {
    map_res(
        terminated(preceded(t("return"), opt(expr)), t(";")),
        |e| -> Result<_, ()> { Ok(Stmt::Return(stmt::Return { value: e })) },
    )(input)
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

fn declr_lexical(input: Input) -> ParseResult<Stmt> {
    map(
        terminated(
            pair(
                pair(
                    alt((t("let"), t("const"))),
                    pair(expr_ident, opt(preceded(t("="), expr_assign))),
                ),
                many0(preceded(
                    t(","),
                    pair(expr_ident, opt(preceded(t("="), expr_assign))),
                )),
            ),
            t(";"),
        ),
        |((kind, binding), mut bindings)| {
            bindings.insert(0, binding);
            let declrs = bindings
                .into_iter()
                .map(|(ident, init)| stmt::VariableDeclarator {
                    ident: ident,
                    init: init,
                })
                .collect();

            Stmt::LexicalDeclr(stmt::LexicalDeclr {
                kind: kind.clone(),
                declarations: declrs,
            })
        },
    )(input)
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
