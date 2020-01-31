use crate::ast;
use crate::lexer::{Lexer, Token};
use failure::{bail, Error};
use std::iter::Peekable;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl Parser<'_> {
    pub fn new<'a>(l: Lexer<'a>) -> Parser<'a> {
        Parser {
            lexer: l.peekable(),
        }
    }

    pub fn parse(&mut self) -> (ast::Program, Vec<Error>) {
        let mut prog = ast::Program::new();
        let mut errs = Vec::new();
        while let Some(_) = self.lexer.peek() {
            match self.parse_statement() {
                Ok(s) => prog.push(s),
                Err(e) => errs.push(e),
            }
        }
        (prog, errs)
    }

    pub fn parse_statement(&mut self) -> Result<ast::Statement, Error> {
        match self.lexer.next() {
            Some(Token::Let) => self.parse_let_statement(),
            Some(_) => Ok(ast::Statement::Incomplete),
            None => bail!("parseStatement called with no more tokens"),
        }
    }

    pub fn expect_next(&mut self, expected: Token) -> Result<(), Error> {
        match self.lexer.next() {
            None => bail!("expect_next called with no more tokens"),
            Some(actual) => {
                if expected == actual {
                    Ok(())
                } else {
                    bail!("expected {:?} but found {:?}", expected, actual)
                }
            }
        }
    }

    pub fn read_identifier(&mut self) -> Result<String, Error> {
        match self.lexer.next() {
            Some(Token::Ident(s)) => Ok(s),
            Some(other) => bail!("expected Ident but found {:?}", other),
            None => bail!("read_identifier called with no more tokens"),
        }
    }

    pub fn parse_let_statement(&mut self) -> Result<ast::Statement, Error> {
        let ident = self.read_identifier()?;
        self.expect_next(Token::Assign)?;
        // TODO read expr
        while let Some(expr) = self.lexer.next() {
            match expr {
                Token::Semi => break,
                _ => {}
            }
        }
        Ok(ast::Statement::Let(ast::Ident(ident), ast::Expr::Todo))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        let code = "
            let x = 9;
            let y = 10;
            let z = 898989;";
        let l = Lexer::new(code);
        let mut p = Parser::new(l);
        let (prog, _errs) = p.parse();
        assert_eq!(3, prog.statements().len())
    }
}
