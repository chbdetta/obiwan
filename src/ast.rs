pub mod args;
pub mod binding;
pub mod expr;
pub mod stmt;

pub use args::{Argument, Arguments};
pub use expr::Expr;
pub use stmt::{Stmt, StmtList};

use crate::codegen::Codegen;
use crate::eval::Eval;

#[derive(Debug, Codegen, Eval)]
pub enum Program {
    Script(StmtList),
    Module(StmtList),
}

#[derive(Debug, Codegen, Eval)]
pub enum ExprOrStmt {
    Expr(Expr),
    Stmt(Stmt),
}
