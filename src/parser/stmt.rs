use super::expr::{expr as parse_expr, expr_assign, expr_ident};
use super::util::*;
use crate::ast::{binding, stmt::*};

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
        stmt_debugger,
        stmt_block,
        stmt_expr,
        stmt_empty,
    ))(input)
}

fn stmt_empty(input: Input) -> ParseResult<Stmt> {
    map(t(";"), |_| Stmt::Empty(Empty))(input)
}

fn stmt_expr(input: Input) -> ParseResult<Stmt> {
    map(terminated(parse_expr, t(";")), |e| Stmt::Expr(Expr(e)))(input)
}

fn stmt_if(input: Input) -> ParseResult<Stmt> {
    map(
        pair(
            pair(
                preceded(t("if"), delimited(t("("), parse_expr, t(")"))),
                stmt,
            ),
            opt(preceded(t("else"), stmt)),
        ),
        |((e, b), s)| {
            Stmt::If(If {
                cond: e,
                body: Box::new(b),
                body_else: s.map(Box::new),
            })
        },
    )(input)
}

fn stmt_while(input: Input) -> ParseResult<Stmt> {
    map(
        pair(
            preceded(t("while"), delimited(t("("), parse_expr, t(")"))),
            stmt,
        ),
        |(e, b)| {
            Stmt::While(While {
                cond: e,
                body: Box::new(b),
            })
        },
    )(input)
}

fn stmt_dowhile(input: Input) -> ParseResult<Stmt> {
    map(
        pair(
            preceded(t("do"), stmt),
            preceded(t("while"), delimited(t("("), parse_expr, t(")"))),
        ),
        |(b, e)| {
            Stmt::DoWhile(DoWhile {
                cond: e,
                body: Box::new(b),
            })
        },
    )(input)
}

fn stmt_for(input: Input) -> ParseResult<Stmt> {
    map(
        pair(
            preceded(
                t("for"),
                delimited(
                    t("("),
                    tuple((
                        parse_expr,
                        preceded(t(";"), parse_expr),
                        preceded(t(";"), parse_expr),
                    )),
                    t(")"),
                ),
            ),
            stmt,
        ),
        |((e1, e2, e3), s)| {
            Stmt::For(For {
                init: e1,
                cond: e2,
                modi: e3,
                body: Box::new(s),
            })
        },
    )(input)
}

fn stmt_switch(input: Input) -> ParseResult<Stmt> {
    unimplemented!()
}

fn stmt_continue(input: Input) -> ParseResult<Stmt> {
    map(
        terminated(preceded(t("continue"), opt(expr_ident)), t(";")),
        |e| Stmt::Continue(Continue { label: e }),
    )(input)
}

fn stmt_break(input: Input) -> ParseResult<Stmt> {
    map(
        terminated(preceded(t("break"), opt(expr_ident)), t(";")),
        |e| Stmt::Break(Break { label: e }),
    )(input)
}

fn stmt_return(input: Input) -> ParseResult<Stmt> {
    map(
        terminated(preceded(t("return"), opt(parse_expr)), t(";")),
        |e| Stmt::Return(Return { value: e }),
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
    map(terminated(t("debugger"), t(";")), |_| {
        Stmt::Debugger(Debugger)
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
                    alt((t("let"), t("const"), t("var"))),
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
                kind: kind.into(),
                declarations: declrs,
            })
        },
    )(input)
}

fn stmt_block(input: Input) -> ParseResult<Stmt> {
    map(delimited(t("{"), stmt_list, t("}")), |list| {
        Stmt::Block(Block { stmts: list })
    })(input)
}
