pub mod binding;
pub mod expr;
pub mod stmt;

pub use expr::{Argument, Arguments, Expr};
pub use stmt::Stmt;

use crate::codegen::Codegen;
use crate::eval::Eval;

#[derive(Debug, Codegen)]
pub enum Program {
    Script(Vec<Stmt>),
    Module(Vec<Stmt>),
}

impl Eval<()> for Program {}
