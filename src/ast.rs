pub mod args;
pub mod binding;
pub mod expr;
pub mod stmt;

pub use args::{Argument, Arguments};
pub use expr::Expr;
pub use stmt::{Stmt, StmtList};

use crate::codegen::Codegen;
use crate::eval::Eval;

#[derive(Debug, Codegen)]
pub enum Program {
    Script(StmtList),
    Module(StmtList),
}

#[derive(Debug, Codegen)]
pub enum ExprOrStmt {
    Expr(Expr),
    Stmt(Stmt),
}

impl Eval<()> for Program {}
impl Eval<()> for ExprOrStmt {}
