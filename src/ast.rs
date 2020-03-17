pub mod binding;
pub mod expr;
pub mod stmt;

pub use expr::{Expr, Op};
pub use stmt::Stmt;

use crate::codegen::Codegen;

#[derive(Debug, Codegen)]
pub enum Program {
    Script(Vec<Stmt>),
    Module(Vec<Stmt>),
}
