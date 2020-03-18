use super::expr::{expr as parse_expr, expr_assign, expr_ident, expr_identname};
use super::util::*;
use crate::ast::{binding, expr, stmt, stmt::*};

pub fn stmt_list(input: Input) -> ParseResult<Vec<Stmt>> {
    many0(alt((stmt, declr_lexical)))(input)
}

pub fn stmt(input: Input) -> ParseResult<Stmt> {
    alt((
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
        stmt_expr,
        stmt_empty,
    ))(input)
}

fn stmt_empty(input: Input) -> ParseResult<Stmt> {
    map_res(t(";"), |_| -> Result<_, ()> { Ok(Stmt::Empty(Empty)) })(input)
}

fn stmt_expr(input: Input) -> ParseResult<Stmt> {
    map_res(terminated(parse_expr, t(";")), |e| -> Result<_, ()> {
        Ok(Stmt::Expr(Expr(e)))
    })(input)
}

fn stmt_var(input: Input) -> ParseResult<Stmt> {
    map_res(
        terminated(
            pair(preceded(t("var"), expr_ident), preceded(t("="), parse_expr)),
            t(";"),
        ),
        |(ident, e)| -> Result<_, ()> {
            Ok(Stmt::Var(Var(binding::Var::Ident(
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
            pair(
                preceded(t("if"), delimited(t("("), parse_expr, t(")"))),
                stmt,
            ),
            opt(preceded(t("else"), stmt)),
        ),
        |((e, b), s)| -> Result<_, ()> {
            Ok(Stmt::If(If {
                cond: e,
                body: Box::new(b),
                body_else: s.map(Box::new),
            }))
        },
    )(input)
}

fn stmt_while(input: Input) -> ParseResult<Stmt> {
    map_res(
        pair(
            preceded(t("while"), delimited(t("("), parse_expr, t(")"))),
            stmt,
        ),
        |(e, b)| -> Result<_, ()> {
            Ok(Stmt::While(While {
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
            preceded(t("while"), delimited(t("("), parse_expr, t(")"))),
        ),
        |(b, e)| -> Result<_, ()> {
            Ok(Stmt::DoWhile(DoWhile {
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
                delimited(t("("), tuple((parse_expr, parse_expr, parse_expr)), t(")")),
            ),
            stmt,
        ),
        |((e1, e2, e3), s)| -> Result<_, ()> {
            Ok(Stmt::For(For {
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
        terminated(preceded(t("continue"), opt(expr_ident)), t(";")),
        |e| -> Result<_, ()> { Ok(Stmt::Continue(Continue { label: e })) },
    )(input)
}

fn stmt_break(input: Input) -> ParseResult<Stmt> {
    map_res(
        terminated(preceded(t("break"), opt(expr_ident)), t(";")),
        |e| -> Result<_, ()> { Ok(Stmt::Break(Break { label: e })) },
    )(input)
}

fn stmt_return(input: Input) -> ParseResult<Stmt> {
    map_res(
        terminated(preceded(t("return"), opt(parse_expr)), t(";")),
        |e| -> Result<_, ()> { Ok(Stmt::Return(Return { value: e })) },
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
        Ok(Stmt::Debugger(Debugger))
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
                .map(|(ident, init)| VariableDeclarator {
                    ident: ident,
                    init: init,
                })
                .collect();

            Stmt::LexicalDeclr(LexicalDeclr {
                kind: kind.clone(),
                declarations: declrs,
            })
        },
    )(input)
}

fn stmt_block(input: Input) -> ParseResult<Stmt> {
    map_res(
        delimited(t("{"), stmt_list, t("}")),
        |list| -> Result<_, ()> { Ok(Stmt::Block(Block { stmts: list })) },
    )(input)
}
