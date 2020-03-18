use super::Expr;
use crate::codegen::Codegen;

pub type Arguments = Vec<Argument>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Argument {
    pub assign: Box<Expr>,
    pub spread: bool,
}

impl Codegen for Argument {
    fn to_code(&self) -> String {
        format!(
            "{}{}",
            if self.spread { "..." } else { "" },
            self.assign.to_code()
        )
    }
}

impl Codegen for Arguments {
    fn to_code(&self) -> String {
        format!(
            "({})",
            self.iter()
                .map(|a| a.to_code())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
