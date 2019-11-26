use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl Lexer<'_> {
    pub fn new<'a>(s: &'a str) -> Lexer<'a> {
        Lexer {
            chars: s.chars().peekable(),
        }
    }

    fn read_identifier(&mut self) -> Token {
        let mut ident: Vec<char> = vec![];
        while { self.chars.peek().map_or(false, |c| c.is_alphanumeric()) } {
            ident.push(self.chars.next().unwrap());
        }
        let ident_s = ident.iter().collect::<String>();
        match ident_s.as_str() {
            "fn" => Token::Func,
            "let" => Token::Let,
            _ => Token::Ident(ident_s),
        }
    }

    fn eat_whitespace(&mut self) {
        while { self.chars.peek().map_or(false, |c| c.is_whitespace()) } {
            self.chars.next();
        }
    }

    fn eat_char(&mut self) {
        self.chars.next();
        ()
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.eat_whitespace();

        let opt = self.chars.peek();
        // map causes double reference to self, b/c self.chars has one and read_identifier needs
        // another one. explicit destructure here creates copy of char I guess??
        match opt {
            None => None,
            Some(c) => {
                let tok = match c {
                    'a'..='z' | 'A'..='Z' => self.read_identifier(),
                    '=' => Token::Assign,
                    '+' => Token::Plus,
                    '(' => Token::LParen,
                    ')' => Token::RParen,
                    '{' => Token::LBrace,
                    '}' => Token::RBrace,
                    ',' => Token::Comma,
                    ';' => Token::Semi,
                    _ => Token::Illegal(c.to_string()),
                };
                self.eat_char();
                Some(tok)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(String),

    // Syntax
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semi,

    // Operators
    Plus,

    // Keywords
    Let,
    Func,
    Assign,

    // Values
    Ident(String),
    Int(i64),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input = "
let ten = 10;
let add = fn(x, y) {
     x + y;
};
let result = add(five, ten);";

        let mut lex = Lexer::new(input);

        let expects = [
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semi,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Func,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::Semi,
        ];

        for ex in expects.iter() {
            assert_eq!(*ex, lex.next().unwrap())
        }
    }
}
