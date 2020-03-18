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
use eval::Evaluator;
use lexer::Lexer;
use parser::parse;

pub struct Obiwan<'a> {
    lexer: Lexer<'a>,
    evaluator: Evaluator,
}

impl<'a> Obiwan<'a> {
    fn new(src: &'a str) -> Self {
        Obiwan {
            lexer: Lexer::new(src),
            evaluator: Evaluator::new(),
        }
    }

    fn run(&mut self) -> Result<String, Error> {
        let tokens = self.lexer.parse()?;

        let ast = parse(tokens)?;

        println!("{:#?}", ast);

        Ok(format!("{}", ast.to_code()))
    }
}

pub fn run_file(file_path: &str) {
    use std::fs;

    let src = fs::read_to_string(file_path).expect("Reading file error");

    run_string(&src[..]);
}

pub fn run_string(src: &str) {
    match Obiwan::new(src).run() {
        Ok(s) => {
            println!("Parsed result: \n\n{}\n", s);
        }
        Err(err) => eprint!("{:#?}\n", err),
    }
}
