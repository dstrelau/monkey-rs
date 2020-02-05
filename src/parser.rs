use crate::ast;
use crate::lexer::{Lexer, Token};
use ast::*;
use failure::{bail, Error};
use std::iter::Peekable;

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
                Ok(s) => prog.push(s),
                Err(e) => errs.push(e),
            }
        }
        (prog, errs)
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
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

    fn consume_until_semi(&mut self) {
        while let Some(expr) = self.lexer.next() {
            match expr {
                Token::Semi => break,
                _ => {}
            }
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, Error> {
        self.expect_next(Token::Let)?;
        let ident = self.read_identifier()?;
        self.expect_next(Token::Assign)?;
        // TODO read expr
        self.consume_until_semi();
        Ok(Statement::Let(Ident(ident), Expr::Todo))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, Error> {
        self.expect_next(Token::Return)?;
        // TODO read expr
        self.consume_until_semi();
        Ok(Statement::Return(Expr::Todo))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
        let expr = self.parse_expression(Precedence::Lowest);
        if self.lexer.peek() == Some(&Token::Semi) {
            self.lexer.next();
        }
        Ok(Statement::Expression(expr))
    }

    fn parse_expression(&mut self, _p: Precedence) -> Expr {
        self.consume_until_semi();
        Expr::Todo
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
            return 1 + 2;";
        let l = Lexer::new(code);
        let mut p = Parser::new(l);
        let (prog, _errs) = p.parse();
        let stmts = prog.statements();
        assert_eq!(3, stmts.len());
        assert_eq!(stmts[0], Statement::Let(Ident("x".to_owned()), Expr::Todo));
        assert_eq!(stmts[1], Statement::Let(Ident("y".to_owned()), Expr::Todo));
        assert_eq!(stmts[2], Statement::Return(Expr::Todo));
    }

    #[test]
    fn test_parse_expression_statement() {
        let code = "foo;";
        let l = Lexer::new(code);
        let mut p = Parser::new(l);
        let (prog, errs) = p.parse();
        println!("{:?} {:?}", prog, errs);
        assert_eq!(1, prog.statements().len());
        assert_eq!(
            prog.statements()[0],
            Statement::Expression(Expr::Ident("foo".to_owned()))
        );
    }
}
