use super::binding::{Lexical as LexicalBind, SingleName, Var as VarBind};
use super::expr::Expr as EExpr;
use crate::codegen::Codegen;
use crate::token::Token;

#[derive(Debug, Clone, Codegen)]
pub enum Stmt {
    Block(Block),
    Empty(Empty),
    Expr(Expr),
    If(If),
    While(While),
    DoWhile(DoWhile),
    For(For),
    Switch(Switch),
    Continue(Continue),
    Break(Break),
    Return(Return),
    With(With),
    Labeled(Labeled),
    Throw(Throw),
    Try(Try),
    Debugger(Debugger),
    Var(Var),
    ClassDeclr(ClassDeclr),
    FunctionDeclr(FunctionDeclr),
    GeneratorDeclr(GeneratorDeclr),
    LexicalDeclr(LexicalDeclr),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub struct Var(pub VarBind);
#[derive(Debug, Clone)]
pub struct LexicalDeclr(pub Token, pub LexicalBind);
#[derive(Debug, Clone)]
pub struct Empty;
#[derive(Debug, Clone)]
pub struct Expr(pub EExpr);
#[derive(Debug, Clone)]
pub struct If {
    pub cond: EExpr,
    pub body: Box<Stmt>,
    pub elses: Vec<Box<Stmt>>,
}
#[derive(Debug, Clone)]
pub struct While(pub EExpr, pub Box<Stmt>);
#[derive(Debug, Clone)]
pub struct DoWhile(pub EExpr, pub Box<Stmt>);
#[derive(Debug, Clone)]
pub struct For(pub EExpr, pub EExpr, pub EExpr, pub Box<Stmt>);
#[derive(Debug, Clone)]
pub struct Switch(
    pub EExpr,
    pub Vec<(EExpr, Box<Stmt>)>,
    pub Option<Box<Stmt>>,
);
#[derive(Debug, Clone)]
pub struct Continue(pub Option<Token>);
#[derive(Debug, Clone)]
pub struct Break(pub Option<Token>);
#[derive(Debug, Clone)]
pub struct Return(pub Option<EExpr>);
#[derive(Debug, Clone)]
pub struct With(pub EExpr, pub Box<Stmt>);
#[derive(Debug, Clone)]
pub struct Labeled(pub Token, pub Box<Stmt>);
#[derive(Debug, Clone)]
pub struct Throw(pub EExpr);
#[derive(Debug, Clone)]
pub struct Try;
#[derive(Debug, Clone)]
pub struct Debugger;
#[derive(Debug, Clone)]
pub struct ClassDeclr;
#[derive(Debug, Clone)]
pub struct FunctionDeclr {
    pub ident: Option<Token>,
    pub parameters: Vec<SingleName>,
    pub body: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub struct GeneratorDeclr;

impl Codegen for Block {
    fn to_code(&self) -> String {
        format!("{{{}}}", self.stmts.to_code(),)
    }
}

impl Codegen for Var {
    fn to_code(&self) -> String {
        format!("var {}", self.0.to_code())
    }
}

impl Codegen for LexicalDeclr {
    fn to_code(&self) -> String {
        format!("{} {}", self.0.src, self.1.to_code())
    }
}

impl Codegen for Empty {
    fn to_code(&self) -> String {
        format!("")
    }
}

impl Codegen for Expr {
    fn to_code(&self) -> String {
        format!("{}", self.0.to_code())
    }
}

impl Codegen for If {
    fn to_code(&self) -> String {
        format!(
            "if({}) {} else {}",
            self.cond.to_code(),
            self.body.to_code(),
            self.elses.to_code()
        )
    }
}

impl Codegen for While {
    fn to_code(&self) -> String {
        format!("while({}) {}", self.0.to_code(), self.1.to_code())
    }
}

impl Codegen for DoWhile {
    fn to_code(&self) -> String {
        format!("do {} while({})", self.1.to_code(), self.0.to_code())
    }
}

impl Codegen for For {
    fn to_code(&self) -> String {
        format!(
            "for({};{};{}) {}",
            self.0.to_code(),
            self.1.to_code(),
            self.2.to_code(),
            self.3.to_code(),
        )
    }
}

impl Codegen for Switch {
    fn to_code(&self) -> String {
        format!(
            "switch({}) {{{} {}}}",
            self.0.to_code(),
            self.1
                .iter()
                .map(|(e, s)| format!("case {}: {}", e.to_code(), s.to_code()))
                .collect::<Vec<String>>()
                .join("\n"),
            if let Some(s) = &self.2 {
                format!("default: {}", s.to_code())
            } else {
                format!("")
            }
        )
    }
}

impl Codegen for Continue {
    fn to_code(&self) -> String {
        String::from(format!("continue {}", self.0.to_code()).trim())
    }
}

impl Codegen for Break {
    fn to_code(&self) -> String {
        String::from(format!("break {}", self.0.to_code()).trim())
    }
}

impl Codegen for Return {
    fn to_code(&self) -> String {
        String::from(format!("return {}", self.0.to_code()).trim())
    }
}

impl Codegen for With {
    fn to_code(&self) -> String {
        format!("with({}) {}", self.0.to_code(), self.1.to_code())
    }
}

impl Codegen for Labeled {
    fn to_code(&self) -> String {
        format!("{}: {}", self.0.to_code(), self.1.to_code())
    }
}

impl Codegen for Throw {
    fn to_code(&self) -> String {
        unimplemented!()
    }
}

impl Codegen for Try {
    fn to_code(&self) -> String {
        unimplemented!()
    }
}

impl Codegen for Debugger {
    fn to_code(&self) -> String {
        format!("debugger")
    }
}

impl Codegen for ClassDeclr {
    fn to_code(&self) -> String {
        unimplemented!()
    }
}

impl Codegen for FunctionDeclr {
    fn to_code(&self) -> String {
        format!(
            "function {}({}) {{{}}}",
            self.ident.to_code(),
            self.parameters.to_code(),
            self.body.to_code()
        )
    }
}

impl Codegen for GeneratorDeclr {
    fn to_code(&self) -> String {
        unimplemented!()
    }
}
