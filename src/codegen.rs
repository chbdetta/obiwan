use crate::ast::*;
use crate::token::Token;

pub trait Codegen {
    fn to_code(&self) -> String;
}

impl<T: Codegen> Codegen for Box<T> {
    fn to_code(&self) -> String {
        (**self).to_code()
    }
}

impl<T: Codegen> Codegen for &T {
    fn to_code(&self) -> String {
        (**self).to_code()
    }
}

fn gen_list<T: Codegen>(l: &[T]) -> String {
    l.iter().map(|t| t.to_code()).collect::<Vec<_>>().join(" ")
}

impl<T: Codegen> Codegen for [T] {
    fn to_code(&self) -> String {
        gen_list(self)
    }
}

default impl<T: Codegen> Codegen for Vec<T> {
    fn to_code(&self) -> String {
        gen_list(self)
    }
}

impl<T> Codegen for Option<T>
where
    T: Codegen,
{
    fn to_code(&self) -> String {
        if let Some(t) = self {
            t.to_code()
        } else {
            String::from("")
        }
    }
}
