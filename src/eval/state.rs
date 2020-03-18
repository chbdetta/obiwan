use super::val::{Undefined, Value};
use crate::ast::expr::Ident;
use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result},
};

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct State {
    data: HashMap<String, Value>,
}

impl State {
    pub fn new() -> Self {
        State {
            data: HashMap::new(),
        }
    }

    pub fn is_declared(&self, var: &Ident) -> bool {
        self.data.get(&var.name.src).is_some()
    }

    pub fn get(&self, var: &Ident) -> Value {
        let src = &var.name.src;
        if let None = self.data.get(src) {
            // Always return 0
            Value::Undefined(Undefined)
        } else {
            self.data[src].clone()
        }
    }

    pub fn update(&mut self, var: &Ident, val: Value) {
        self.data.insert(var.name.src.clone(), val);
    }

    pub fn remove(&mut self, var: &Ident) {
        self.data.remove(&var.name.src);
    }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut ret = self
            .data
            .iter()
            .map(|(a, v)| format!("{} → {}", a, v))
            .collect::<Vec<String>>();
        ret.sort_unstable();

        write!(f, "{{{}}}", ret.join(", "))
    }
}
