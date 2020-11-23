use std::iter::Peekable;

use ast::*;
use failure::{bail, Error};

use crate::ast;
use crate::lexer::{Lexer, Token};

enum Precedence {
    Lowest,
}
pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl Parser<'_> {
    pub fn new<'a>(l: Lexer<'a>) -> Parser<'a> {
        Parser {
            lexer: l.peekable(),
        }
    }

    pub fn parse(&mut self) -> (Program, Vec<Error>) {
        let mut prog = Program::new();
        let mut errs = Vec::new();
        while let Some(_) = self.lexer.peek() {
            match self.parse_statement() {
                Ok(s) => {
                    println!("got statement: {:?}", s);
                    prog.push(s)
                }
                Err(e) => {
                    println!("got error: {:?}", e);
                    errs.push(e)
                }
            }
        }
        (prog, errs)
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        // println!("parse_statement");
        match self.lexer.peek() {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(_) => self.parse_expression_statement(),
            None => bail!("parseStatement called with no more tokens"),
        }
    }

    fn expect_next(&mut self, expected: Token) -> Result<(), Error> {
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

    fn read_identifier(&mut self) -> Result<String, Error> {
        match self.lexer.next() {
            Some(Token::Ident(s)) => Ok(s),
            Some(other) => bail!("expected Ident but found {:?}", other),
            None => bail!("read_identifier called with no more tokens"),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, Error> {
        // println!("parse_let_statement");
        self.expect_next(Token::Let)?;
        let ident = self.read_identifier()?;
        self.expect_next(Token::Assign)?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semi)?;
        Ok(Statement::Let(Ident(ident), expr))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, Error> {
        // println!("parse_return_statement");
        self.expect_next(Token::Return)?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semi)?;
        Ok(Statement::Return(expr))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
        // println!("parse_expression_statement");
        let expr = self.parse_expression(Precedence::Lowest)?;
        if self.lexer.peek() == Some(&Token::Semi) {
            self.lexer.next();
        }
        Ok(Statement::Expression(expr))
    }

    fn parse_expression(&mut self, _p: Precedence) -> Result<Expr, Error> {
        let tkn = self.lexer.next();
        // println!("parse_expression peeked: {:?}", tkn);
        match tkn {
            Some(Token::Ident(s)) => Ok(Expr::Ident(s.to_owned())),
            Some(Token::Int(s)) => {
                let n = i64::from_str_radix(&s, 10)?;
                Ok(Expr::Int(n))
            }
            Some(_) => Ok(Expr::Todo),
            None => bail!("parse_expression called with no more tokens"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(code: &str) -> (Program, Vec<Error>) {
        let l = Lexer::new(code);
        let mut p = Parser::new(l);
        p.parse()
    }

    #[test]
    fn test_parser() {
        let (prog, _errs) = parse(
            "
            let x = 9;
            let y = 10;
            return 1 + 2;",
        );
        let stmts = prog.statements();
        assert_eq!(3, stmts.len());
        assert_eq!(
            stmts[0],
            Statement::Let(Ident("x".to_owned()), Expr::Int(9))
        );
        assert_eq!(
            stmts[1],
            Statement::Let(Ident("y".to_owned()), Expr::Int(10))
        );
        assert_eq!(stmts[2], Statement::Return(Expr::Todo));
    }

    #[test]
    fn test_parse_expression_statement() {
        let (prog, _errs) = parse("foo;");
        assert_eq!(1, prog.statements().len());
        assert_eq!(
            prog.statements()[0],
            Statement::Expression(Expr::Ident("foo".to_owned()))
        );
    }
}
