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

impl<T> Codegen for Vec<T>
where
    T: Codegen,
{
    fn to_code(&self) -> String {
        self.iter()
            .map(|s| s.to_code())
            .collect::<Vec<String>>()
            .join("\n")
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
