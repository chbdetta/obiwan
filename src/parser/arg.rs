use super::expr::expr_assign;
use super::util::*;
use crate::ast::{Argument, Arguments};

pub fn arguments(input: Input) -> ParseResult<Arguments> {
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
