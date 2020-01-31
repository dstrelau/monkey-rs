use std::io::{self, BufRead};

use monkey::Lexer;
use monkey::Parser;

fn main() {
    loop {
        let lines: Result<Vec<String>, io::Error> = io::stdin().lock().lines().collect();
        let source = lines.unwrap().join("\n");
        let lexer = Lexer::new(source.as_str());
        let mut parser = Parser::new(lexer);
        let (prog, errs) = parser.parse();
        for e in errs {
            println!("Error: {:?}", e);
        }
        for s in prog.statements() {
            println!("{:?}", s);
        }
    }
}
