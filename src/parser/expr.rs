use super::arg::arguments;
use super::util::*;
use crate::ast::{expr, Expr};

// actual parsers
pub fn expr(input: Input) -> ParseResult<Expr> {
    expr_assign(input)
}

macro_rules! bin_op {
    ($next:ident,[$token:literal,$struct:ident],$input:ident) => {
        map_res(
            pair($next, many0(pair((t($token)), $next))),
            |(a, bs)| {
                let mut ret = a;

                for (op, b) in bs {
                    ret = match &op.src[..] {
                        $token => Expr::$struct(expr::$struct(Box::new(ret), Box::new(b))),
                        _ => return Err(nom::Err::Error(($input, nom::error::ErrorKind::Alpha))),
                    }
                }

                Ok(ret)
            },
        )($input)
    };
    ($next:ident,[$($token:literal,$struct:ident);*],$input:ident) => {
        map_res(
            pair($next, many0(pair(alt(($(t($token)),*)), $next))),
            |(a, bs)| {
                let mut ret = a;

                for (op, b) in bs {
                    ret = match &op.src[..] {
                        $(
                            $token => Expr::$struct(expr::$struct(Box::new(ret), Box::new(b))),
                        )*
                        _ => return Err(nom::Err::Error(($input, nom::error::ErrorKind::Alpha))),
                    }
                }

                Ok(ret)
            },
        )($input)
    };
}

pub fn expr_assign(input: Input) -> ParseResult<Expr> {
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
    map(
        pair(
            expr_or,
            many0(pair(delimited(t("?"), expr, t(":")), expr_or)),
        ),
        |(a, bs)| {
            let mut ret = a;

            for (b, c) in bs {
                ret = Expr::Cond(expr::Cond(Box::new(ret), Box::new(b), Box::new(c)))
            }

            ret
        },
    )(input)
}

fn expr_or(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_and, ["||", Or], input)
}

fn expr_and(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_equality, ["&&", And], input)
}

fn expr_equality(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_comparison, ["==", Equal; "!=", Neq; "===", StrictEq; "!==", StrictNeq], input)
}

fn expr_comparison(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_addition, [
        "<", Less;
        "<=", LessEq;
        ">", Greater;
        ">=", GreaterEq;
        "in", In;
        "instanceof", InstanceOf],
        input
    )
}

fn expr_addition(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_multiplication, ["+", Add; "-", Sub], input)
}

fn expr_multiplication(input: Input) -> ParseResult<Expr> {
    bin_op!(expr_unary, ["*", Mul; "/", Div; "%", Mod], input)
}

fn expr_unary(input: Input) -> ParseResult<Expr> {
    alt((
        map_res(
            pair(
                alt((
                    t("!"),
                    t("-"),
                    t("+"),
                    t("typeof"),
                    t("delete"),
                    t("void"),
                    t("++"),
                    t("--"),
                )),
                expr_unary,
            ),
            |(op, e)| match &op.src[..] {
                "!" => Ok(Expr::Not(expr::Not(Box::new(e)))),
                "-" => Ok(Expr::Neg(expr::Neg(Box::new(e)))),
                "+" => Ok(Expr::Positive(expr::Positive(Box::new(e)))),
                "typeof" => Ok(Expr::Typeof(expr::Typeof(Box::new(e)))),
                "delete" => Ok(Expr::Delete(expr::Delete(Box::new(e)))),
                "void" => Ok(Expr::Void(expr::Void(Box::new(e)))),
                "++" => Ok(Expr::PreIncr(expr::PreIncr(Box::new(e)))),
                "--" => Ok(Expr::PreDecr(expr::PreDecr(Box::new(e)))),
                _ => Err(nom::Err::Error(nom::error::ErrorKind::Alpha)),
            },
        ),
        expr_update,
    ))(input)
}

fn expr_update(input: Input) -> ParseResult<Expr> {
    map(
        pair(expr_left_hand_side, opt(alt((t("++"), t("--"))))),
        |(l, op)| match op.map(|o| &o.src[..]) {
            Some("++") => Expr::PostIncr(expr::PostIncr(Box::new(l))),
            Some("--") => Expr::PostDecr(expr::PostDecr(Box::new(l))),
            _ => l,
        },
    )(input)
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

pub fn expr_ident(input: Input) -> ParseResult<Expr> {
    map(tt(TokenType::Identifier), |t| {
        Expr::Ident(expr::Ident { name: t.clone() })
    })(input)
}

pub fn expr_identname(input: Input) -> ParseResult<Expr> {
    map(
        t_is(|token| token.tt == TokenType::Identifier || token.is_reserve()),
        |token| {
            Expr::Ident(expr::Ident {
                name: token.clone(),
            })
        },
    )(input)
}
