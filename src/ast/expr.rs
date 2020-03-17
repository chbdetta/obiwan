use crate::codegen::Codegen;
use crate::token::Token;

mod precedence;

use precedence::Precedence;

#[derive(Debug, Clone, PartialEq, Eq, Codegen, Precedence)]
pub enum Expr {
    Op(Op),
    // primary values
    // literals
    #[precedence(21)]
    True(True),
    #[precedence(21)]
    False(False),
    #[precedence(21)]
    Null(Null),
    #[precedence(21)]
    Number(Number),
    #[precedence(21)]
    Str(Str),
    #[precedence(21)]
    Template(Template),
    // identifier
    #[precedence(21)]
    This(This),
    #[precedence(21)]
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Codegen, Precedence)]
pub enum Op {
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
}

macro_rules! binary_op {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $name(pub Box<Expr>, pub Box<Expr>);
    };
}

macro_rules! unary_op {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
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

binary_op!(Assign);
binary_op!(AddAssign);
binary_op!(SubAssign);
binary_op!(MulAssign);
binary_op!(DivAssign);
binary_op!(ModAssign);

binary_op!(And);
binary_op!(Or);

unary_op!(Not);
unary_op!(Neg);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cond(pub Box<Expr>, pub Box<Expr>, pub Box<Expr>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Number(pub i32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Str(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Null;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct True;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct False;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Template(pub String, pub Vec<Box<Expr>>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier(pub Token);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct This;

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

impl Codegen for True {
    fn to_code(&self) -> String {
        format!("true")
    }
}

impl Codegen for False {
    fn to_code(&self) -> String {
        format!("false")
    }
}

impl Codegen for Null {
    fn to_code(&self) -> String {
        format!("null")
    }
}

impl Codegen for Number {
    fn to_code(&self) -> String {
        format!("{}", self.0)
    }
}

impl Codegen for Str {
    fn to_code(&self) -> String {
        format!("\"{}\"", self.0)
    }
}

impl Codegen for Template {
    fn to_code(&self) -> String {
        format!("`{}`", self.0)
    }
}

impl Codegen for This {
    fn to_code(&self) -> String {
        format!("this")
    }
}

impl Codegen for Identifier {
    fn to_code(&self) -> String {
        format!("{}", self.0.src)
    }
}
