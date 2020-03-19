use super::binding::SingleNameBinding;
use super::expr::Expr as EExpr;
use crate::codegen::Codegen;
use crate::eval::Eval;
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
    ClassDeclr(ClassDeclr),
    FunctionDeclr(FunctionDeclr),
    GeneratorDeclr(GeneratorDeclr),
    LexicalDeclr(LexicalDeclr),
}

type StmtList = Vec<Stmt>;

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: StmtList,
}
#[derive(Debug, Clone)]
pub struct Empty;
#[derive(Debug, Clone)]
pub struct Expr(pub EExpr);
#[derive(Debug, Clone)]
pub struct If {
    pub cond: EExpr,
    pub body: Box<Stmt>,
    pub body_else: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: EExpr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct DoWhile {
    pub cond: EExpr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub init: EExpr,
    pub cond: EExpr,
    pub modi: EExpr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Switch {
    pub test: EExpr,
    pub cases: Vec<(EExpr, Box<Stmt>)>,
    pub default: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Continue {
    pub label: Option<EExpr>,
}

#[derive(Debug, Clone)]
pub struct Break {
    pub label: Option<EExpr>,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<EExpr>,
}

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableDeclarator {
    pub ident: EExpr,
    pub init: Option<EExpr>,
}

impl Codegen for VariableDeclarator {
    fn to_code(&self) -> String {
        let init = if self.init.is_some() {
            format!(" = {}", self.init.to_code())
        } else {
            format!("")
        };
        format!("{}{}", self.ident.to_code(), init)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Codegen)]
pub enum VariableDeclrKind {
    Let,
    Const,
    Var,
}

impl From<&Token> for VariableDeclrKind {
    fn from(tk: &Token) -> Self {
        match &tk.tt {
            TokenType::Let => VariableDeclrKind::Let,
            TokenType::Const => VariableDeclrKind::Const,
            TokenType::Var => VariableDeclrKind::Var,
            _ => panic!("Impossible to convert tokens except Let, Const and Var"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LexicalDeclr {
    pub kind: VariableDeclrKind,
    pub declarations: Vec<VariableDeclarator>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Parameter {
    binding: SingleNameBinding,
    rest: bool,
}

pub type Parameters = Vec<Parameter>;

impl Codegen for Parameter {
    fn to_code(&self) -> String {
        format!(
            "{}{}",
            if self.rest { "..." } else { "" },
            self.binding.to_code()
        )
    }
}

impl Codegen for Parameters {
    fn to_code(&self) -> String {
        self.iter()
            .map(|p| p.to_code())
            .collect::<Vec<_>>()
            .join(", ")
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclr {
    pub name: Option<Token>,
    pub parameters: Parameters,
    pub body: StmtList,
}
#[derive(Debug, Clone)]
pub struct GeneratorDeclr;

impl Codegen for Block {
    fn to_code(&self) -> String {
        format!("{{\n{}\n}}", self.stmts.to_code(),)
    }
}

impl Codegen for LexicalDeclr {
    fn to_code(&self) -> String {
        format!(
            "{} {};",
            self.kind.to_code(),
            self.declarations
                .iter()
                .map(|d| d.to_code())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl Codegen for Empty {
    fn to_code(&self) -> String {
        format!(";")
    }
}

impl Codegen for Expr {
    fn to_code(&self) -> String {
        format!("{};", self.0.to_code())
    }
}

impl Codegen for If {
    fn to_code(&self) -> String {
        format!(
            "if({}) {}{}",
            self.cond.to_code(),
            self.body.to_code(),
            if let Some(b) = &self.body_else {
                format!(" else {}", b.to_code())
            } else {
                format!("")
            }
        )
    }
}

impl Codegen for While {
    fn to_code(&self) -> String {
        format!("while({}) {}", self.cond.to_code(), self.body.to_code())
    }
}

impl Codegen for DoWhile {
    fn to_code(&self) -> String {
        format!("do {} while({})", self.body.to_code(), self.cond.to_code())
    }
}

impl Codegen for For {
    fn to_code(&self) -> String {
        format!(
            "for({};{};{}) {}",
            self.init.to_code(),
            self.cond.to_code(),
            self.modi.to_code(),
            self.body.to_code(),
        )
    }
}

impl Codegen for Switch {
    fn to_code(&self) -> String {
        format!(
            "switch({}) {{{} {}}}",
            self.test.to_code(),
            self.cases
                .iter()
                .map(|(e, s)| format!("case {}: {}", e.to_code(), s.to_code()))
                .collect::<Vec<String>>()
                .join("\n"),
            if let Some(s) = &self.default {
                format!("default: {}", s.to_code())
            } else {
                format!("")
            }
        )
    }
}

impl Codegen for Continue {
    fn to_code(&self) -> String {
        String::from(format!("continue {};", self.label.to_code()).trim())
    }
}

impl Codegen for Break {
    fn to_code(&self) -> String {
        String::from(format!("break {};", self.label.to_code()).trim())
    }
}

impl Codegen for Return {
    fn to_code(&self) -> String {
        String::from(format!("return {};", self.value.to_code()).trim())
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
        format!("debugger;")
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
            self.name.to_code(),
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

impl Codegen for StmtList {
    fn to_code(&self) -> String {
        self.iter()
            .map(|s| s.to_code())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

// Evaluate
impl Eval<()> for Stmt {}
impl Eval<()> for Block {}
impl Eval<()> for Empty {}
impl Eval<()> for Expr {}
impl Eval<()> for If {}
impl Eval<()> for While {}
impl Eval<()> for DoWhile {}
impl Eval<()> for For {}
impl Eval<()> for Switch {}
impl Eval<()> for Continue {}
impl Eval<()> for Break {}
impl Eval<()> for Return {}
impl Eval<()> for With {}
impl Eval<()> for Labeled {}
impl Eval<()> for Throw {}
impl Eval<()> for Try {}
impl Eval<()> for Debugger {}
impl Eval<()> for ClassDeclr {}
impl Eval<()> for FunctionDeclr {}
impl Eval<()> for GeneratorDeclr {}
impl Eval<()> for LexicalDeclr {}
