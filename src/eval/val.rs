use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Boolean(bool),
    Number(Number),
    String(String),
    Undefined(Undefined),
    Null(Null),
    NaN(NaN),
    // This variant represent the nonexist value from a statement
    None,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Null;
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct NaN;
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Undefined;

pub type Number = i32;

impl From<Value> for bool {
    fn from(val: Value) -> Self {
        match val {
            Value::Boolean(b) => b,
            Value::Number(n) => n != 0,
            Value::String(_) => true,
            Value::Undefined(_) => false,
            Value::Null(_) => false,
            Value::NaN(_) => false,
            Value::None => panic!("None value can not be converted to bool"),
        }
    }
}

impl From<Value> for Number {
    fn from(val: Value) -> Self {
        match val {
            Value::Boolean(b) => {
                if b {
                    1
                } else {
                    0
                }
            }
            Value::Number(n) => n,
            Value::String(_) => 1,
            Value::Undefined(_) => 0,
            Value::Null(_) => 0,
            // FIXME:
            Value::NaN(_) => std::i32::MAX,
            Value::None => panic!("None value can not be converted to number"),
        }
    }
}
impl From<Value> for String {
    fn from(val: Value) -> Self {
        match val {
            Value::Boolean(b) => {
                if b {
                    String::from("true")
                } else {
                    String::from("false")
                }
            }
            Value::Number(n) => format!("{}", n),
            Value::String(s) => s,
            Value::Undefined(_) => format!("undefined"),
            Value::Null(_) => format!("null"),
            Value::NaN(_) => format!("NaN"),
            Value::None => panic!("None value can not be converted to string"),
        }
    }
}

impl Add for Value {
    type Output = Value;
    fn add(self, other: Self) -> Self::Output {
        unimplemented!();
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Boolean(b) => {
                    if *b {
                        String::from("true")
                    } else {
                        String::from("false")
                    }
                }
                Value::Number(n) => format!("{}", n),
                Value::String(s) => format!("\"{}\"", s),
                Value::Undefined(_) => format!("undefined"),
                Value::Null(_) => format!("null"),
                Value::NaN(_) => format!("NaN"),
                Value::None => String::new(),
            }
        )
    }
}
