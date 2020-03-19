use crate::codegen::Codegen;
use crate::eval::{Eval, Evaluator, Value};
use crate::token::Token;

mod precedence;

use super::args::Arguments;
pub use precedence::Precedence;

#[derive(Debug, Clone, PartialEq, Eq, Codegen, Precedence, Eval)]
pub enum Expr {
    // Operators
    #[precedence(3)]
    Assign(Assign),
    #[precedence(3)]
    AddAssign(AddAssign),
    #[precedence(3)]
    SubAssign(SubAssign),
    #[precedence(3)]
    MulAssign(MulAssign),
    #[precedence(3)]
    DivAssign(DivAssign),
    #[precedence(3)]
    ModAssign(ModAssign),

    #[precedence(11)]
    Equal(Equal),
    #[precedence(11)]
    StrictEq(StrictEq),
    #[precedence(11)]
    Neq(Neq),
    #[precedence(11)]
    StrictNeq(StrictNeq),

    #[precedence(12)]
    InstanceOf(InstanceOf),
    #[precedence(12)]
    In(In),
    #[precedence(12)]
    Less(Less),
    #[precedence(12)]
    Greater(Greater),
    #[precedence(12)]
    LessEq(LessEq),
    #[precedence(12)]
    GreaterEq(GreaterEq),

    #[precedence(14)]
    Add(Add),
    #[precedence(14)]
    Sub(Sub),
    #[precedence(15)]
    Mul(Mul),
    #[precedence(15)]
    Div(Div),
    #[precedence(15)]
    Mod(Mod),

    #[precedence(17)]
    PreIncr(PreIncr),
    #[precedence(17)]
    PreDecr(PreDecr),

    #[precedence(18)]
    PostIncr(PostIncr),
    #[precedence(18)]
    PostDecr(PostDecr),

    #[precedence(20)]
    Computed(Computed),
    #[precedence(20)]
    Member(Member),
    #[precedence(20)]
    Call(Call),
    #[precedence(20)]
    New(New),

    #[precedence(6)]
    And(And),
    #[precedence(5)]
    Or(Or),
    #[precedence(4)]
    Cond(Cond),

    #[precedence(17)]
    Not(Not),
    #[precedence(17)]
    Neg(Neg),
    #[precedence(17)]
    Positive(Positive),
    #[precedence(17)]
    Delete(Delete),
    #[precedence(17)]
    Void(Void),
    #[precedence(17)]
    Typeof(Typeof),

    // primary values
    // literals
    #[precedence(22)]
    Literal(Literal),

    #[precedence(22)]
    Template(Template),

    #[precedence(22)]
    Ident(Ident),

    #[precedence(22)]
    This(This),

    #[precedence(22)]
    Super(Super),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralValue {
    String(String),
    Number(i32),
    Boolean(bool),
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq, Eval)]
pub struct Literal {
    pub token: Token,
    pub value: LiteralValue,
}

impl Codegen for Literal {
    fn to_code(&self) -> String {
        format!("{}", self.token.to_code())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Eval)]
pub struct Template(pub String, pub Vec<Box<Expr>>);

impl Codegen for Template {
    fn to_code(&self) -> String {
        format!("`{}`", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Eval)]
pub struct Ident {
    pub name: Token,
}

#[derive(Debug, Clone, PartialEq, Eq, Eval)]
pub struct This;

#[derive(Debug, Clone, PartialEq, Eq, Eval)]
pub struct Super;

impl Codegen for This {
    fn to_code(&self) -> String {
        format!("this")
    }
}

impl Codegen for Super {
    fn to_code(&self) -> String {
        format!("super")
    }
}

impl Codegen for Ident {
    fn to_code(&self) -> String {
        format!("{}", self.name.to_code())
    }
}

// Operators

macro_rules! binary_op {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Eval)]
        pub struct $name(pub Box<Expr>, pub Box<Expr>);
    };
}

macro_rules! unary_op {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Eval)]
        pub struct $name(pub Box<Expr>);
    };
}

binary_op!(Equal);
binary_op!(StrictEq);
binary_op!(Neq);
binary_op!(StrictNeq);
binary_op!(InstanceOf);
binary_op!(In);
binary_op!(Less);
binary_op!(Greater);
binary_op!(LessEq);
binary_op!(GreaterEq);
binary_op!(Add);
binary_op!(Sub);
binary_op!(Mul);
binary_op!(Div);
binary_op!(Mod);

binary_op!(Computed);

binary_op!(Assign);
binary_op!(AddAssign);
binary_op!(SubAssign);
binary_op!(MulAssign);
binary_op!(DivAssign);
binary_op!(ModAssign);

binary_op!(And);
binary_op!(Or);

unary_op!(New);

unary_op!(Not);
unary_op!(Neg);
unary_op!(Positive);
unary_op!(Typeof);
unary_op!(Delete);
unary_op!(Void);

unary_op!(PostDecr);
unary_op!(PreDecr);
unary_op!(PostIncr);
unary_op!(PreIncr);

#[derive(Debug, Clone, PartialEq, Eq, Eval)]
pub struct Cond(pub Box<Expr>, pub Box<Expr>, pub Box<Expr>);

#[derive(Debug, Clone, PartialEq, Eq, Eval)]
pub struct Call {
    pub callee: Box<Expr>,
    pub arguments: Arguments,
}

#[derive(Debug, Clone, PartialEq, Eq, Eval)]
pub struct Member {
    pub object: Box<Expr>,
    pub property: Box<Expr>,
    pub computed: bool,
}

/**
 * Code generation definitions
 *
 */

fn parens<A: Precedence, B: Precedence + Codegen>(a: &A, b: &B) -> String {
    if a.precedence() > b.precedence() {
        format!("({})", b.to_code())
    } else {
        b.to_code()
    }
}

macro_rules! bin_codegen {
    ($name:ident,$symbol:literal) => {
        impl Codegen for $name {
            fn to_code(&self) -> String {
                let left = parens(self, &self.0);
                let right = parens(self, &self.1);
                format!("{} {} {}", left, $symbol, right)
            }
        }
    };
}

bin_codegen!(Assign, "=");
bin_codegen!(AddAssign, "+=");
bin_codegen!(SubAssign, "-=");
bin_codegen!(MulAssign, "*=");
bin_codegen!(DivAssign, "/=");
bin_codegen!(ModAssign, "%=");
bin_codegen!(Equal, "==");
bin_codegen!(StrictEq, "===");
bin_codegen!(Neq, "!=");
bin_codegen!(StrictNeq, "!==");
bin_codegen!(InstanceOf, "instanceof");
bin_codegen!(In, "in");
bin_codegen!(Less, "<");
bin_codegen!(Greater, ">");
bin_codegen!(LessEq, "<=");
bin_codegen!(GreaterEq, ">=");
bin_codegen!(Add, "+");
bin_codegen!(Sub, "-");
bin_codegen!(Mul, "*");
bin_codegen!(Div, "/");
bin_codegen!(Mod, "%");
bin_codegen!(And, "&&");
bin_codegen!(Or, "||");

impl Codegen for Cond {
    fn to_code(&self) -> String {
        format!(
            "{} ? {} : {}",
            self.0.to_code(),
            self.1.to_code(),
            self.2.to_code()
        )
    }
}

impl Codegen for Member {
    fn to_code(&self) -> String {
        let property = if self.computed {
            format!("[{}]", self.property.to_code())
        } else {
            format!(".{}", self.property.to_code())
        };

        format!("{}{}", self.object.to_code(), property)
    }
}

impl Codegen for Typeof {
    fn to_code(&self) -> String {
        let child = parens(self, &self.0);
        format!("typeof {}", child)
    }
}

impl Codegen for Delete {
    fn to_code(&self) -> String {
        let child = parens(self, &self.0);
        format!("delete {}", child)
    }
}

impl Codegen for Void {
    fn to_code(&self) -> String {
        let child = parens(self, &self.0);
        format!("void {}", child)
    }
}

impl Codegen for Not {
    fn to_code(&self) -> String {
        let child = parens(self, &self.0);
        format!("!{}", child)
    }
}

impl Codegen for Neg {
    fn to_code(&self) -> String {
        let child = parens(self, &self.0);
        format!("-{}", child)
    }
}

impl Codegen for Positive {
    fn to_code(&self) -> String {
        let child = parens(self, &self.0);
        format!("+{}", child)
    }
}

impl Codegen for PostDecr {
    fn to_code(&self) -> String {
        let child = parens(self, &self.0);
        format!("{}--", child)
    }
}

impl Codegen for PostIncr {
    fn to_code(&self) -> String {
        let child = parens(self, &self.0);
        format!("{}++", child)
    }
}

impl Codegen for PreDecr {
    fn to_code(&self) -> String {
        let child = parens(self, &self.0);
        format!("--{}", child)
    }
}

impl Codegen for PreIncr {
    fn to_code(&self) -> String {
        let child = parens(self, &self.0);
        format!("++{}", child)
    }
}

impl Codegen for Computed {
    fn to_code(&self) -> String {
        format!("{}[{}]", self.0.to_code(), self.1.to_code())
    }
}

impl Codegen for New {
    fn to_code(&self) -> String {
        format!("new {}", self.0.to_code())
    }
}

impl Codegen for Call {
    fn to_code(&self) -> String {
        format!("{}({})", self.callee.to_code(), self.arguments.to_code())
    }
}
