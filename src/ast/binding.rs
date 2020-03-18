use super::expr::Expr;
use crate::codegen::Codegen;
use crate::token::Token;

#[derive(Debug, Clone, Eq, PartialEq, Codegen)]
pub enum Lexical {
    Ident(SingleNameBinding),
}

#[derive(Debug, Clone, Eq, PartialEq, Codegen)]
pub enum Var {
    Ident(SingleNameBinding),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SingleNameBinding {
    pub ident: Expr,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pattern {
    ObjectPat(ObjectPat),
    ArrayPat(ArrayPat),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ArrayPat {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ObjectPat;

impl Codegen for SingleNameBinding {
    fn to_code(&self) -> String {
        let init = if let Some(e) = &self.init {
            format!(" = {}", e.to_code())
        } else {
            format!("")
        };
        format!("{}{}", self.ident.to_code(), init)
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
