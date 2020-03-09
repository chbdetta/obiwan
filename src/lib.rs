#![allow(dead_code)]

mod error;
mod eval;
mod lexer;
mod parser;
mod pos;
mod token;

use error::Error;
use eval::Evaluator;
use lexer::Lexer;
use parser::Parser;

struct Obiwan<'a> {
    parser: Parser,
    lexer: Lexer<'a>,
    evaluator: Evaluator,
}

impl<'a> Obiwan<'a> {
    fn new(src: &'a str) -> Self {
        Obiwan {
            parser: Parser::new(),
            lexer: Lexer::new(src),
            evaluator: Evaluator::new(),
        }
    }

    fn run(&self) -> Result<String, Error> {
        unimplemented!()
    }
}

fn run_file(file_path: &str) {
    // TODO: read file and run
    // let obiwan = Obiwan.new().run();

    unimplemented!()
}

fn run_std(string: &str) {
    Obiwan::new(string).run();
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
