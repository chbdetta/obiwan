#![feature(specialization)]
#![allow(dead_code)]

#[macro_use]
extern crate obiwan_derive;

#[macro_use]
mod macros;

mod ast;
mod codegen;
mod error;
mod eval;
mod lexer;
mod parser;
mod pos;
mod token;

use codegen::Codegen;
use error::Error;
use eval::{Eval, ObiwanEval};
use lexer::Lexer;
use parser::parse;

pub struct Obiwan<'a> {
    lexer: Lexer<'a>,
    evaluator: ObiwanEval,
}

pub enum Mode {
    Ast,
    Token,
    Eval,
    Code,
}

impl Default for Mode {
    fn default() -> Self {
        Mode::Eval
    }
}

impl<'a> Obiwan<'a> {
    fn new(src: &'a str) -> Self {
        Obiwan {
            lexer: Lexer::new(src),
            evaluator: ObiwanEval::new(),
        }
    }

    fn run(&mut self, mode: Mode) -> Result<String, Error> {
        let tokens = self.lexer.parse()?;

        let ast = parse(tokens)?;

        Ok(match mode {
            Mode::Ast => format!("{:#?}", ast),
            Mode::Code => ast.to_code(),
            Mode::Eval => {
                ast.eval(&mut self.evaluator);
                format!("{:#?}", self.evaluator.state)
            }
            Mode::Token => format!("{:#?}", tokens),
        })
    }
}

pub fn run_file(file_path: &str, mode: Mode) {
    use std::fs;

    let src = fs::read_to_string(file_path).expect("Reading file error");

    run_string(&src[..], mode);
}

pub fn run_string(src: &str, mode: Mode) {
    match Obiwan::new(src).run(mode) {
        Ok(s) => {
            println!("Parsed result: \n\n{}\n", s);
        }
        Err(err) => eprint!("{:#?}\n", err),
    }
}
