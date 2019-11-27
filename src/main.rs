use std::io::{self, BufRead};

use monkey::*;

fn main() {
    loop {
        let lines: Result<Vec<String>, io::Error> = io::stdin().lock().lines().collect();
        let source = lines.unwrap().join("\n");
        let lexer = Lexer::new(source.as_str());
        for tok in lexer {
            print!("{:?} ", tok)
        }
        println!()
    }
}
