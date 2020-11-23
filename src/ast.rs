#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

#[derive(Debug, PartialEq)]
pub enum Expr {
    Todo,
    Ident(String),
    Int(i64),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Ident, Expr),
    Return(Expr),
    Expression(Expr),
    Incomplete,
}

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program { statements: vec![] }
    }

    pub fn push(&mut self, s: Statement) {
        self.statements.push(s)
    }

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}
