use super::expr::Expr;
use crate::codegen::Codegen;
use crate::token::Token;

#[derive(Debug, Clone, Codegen)]
pub enum Lexical {
    Ident(SingleName),
}

#[derive(Debug, Clone, Codegen)]
pub enum Var {
    Ident(SingleName),
}

#[derive(Debug, Clone)]
pub struct SingleName {
    pub ident: Token,
    // This should be AssignmentExpression
    pub init: Expr,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    ObjectPat(ObjectPat),
    ArrayPat(ArrayPat),
}

#[derive(Debug, Clone)]
pub struct ArrayPat {}

#[derive(Debug, Clone)]
pub struct ObjectPat;

impl Codegen for SingleName {
    fn to_code(&self) -> String {
        format!("{} = {}", self.ident.to_code(), self.init.to_code())
    }
}

impl Codegen for Pattern {
    fn to_code(&self) -> String {
        unimplemented!()
    }
}

impl Codegen for ArrayPat {
    fn to_code(&self) -> String {
        unimplemented!()
    }
}

impl Codegen for ObjectPat {
    fn to_code(&self) -> String {
        unimplemented!()
    }
}
