use std::iter::{Enumerate, Peekable};
use std::str::Chars;

pub struct Lexer<'a> {
    chars: Peekable<Enumerate<Chars<'a>>>,
}

impl Lexer<'_> {
    pub fn new<'a>(s: &'a str) -> Lexer<'a> {
        Lexer {
            chars: s.chars().enumerate().peekable(),
        }
    }

    fn read_identifier(&mut self) -> Token {
        let i = self.read_while(char::is_alphanumeric);
        match i.as_str() {
            "fn" => Token::Func,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(i),
        }
    }

    fn read_int(&mut self) -> Token {
        let i = self.read_while(char::is_numeric);
        Token::Int(i)
    }

    fn read_while<F>(&mut self, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut ident: Vec<char> = vec![];
        while { self.chars.peek().map_or(false, |(_, c)| f(*c)) } {
            let (_, c) = self.chars.next().unwrap();
            ident.push(c);
        }
        ident.into_iter().collect::<String>()
    }

    fn eat_whitespace(&mut self) {
        while { self.chars.peek().map_or(false, |(_, c)| c.is_whitespace()) } {
            self.chars.next();
        }
    }

    pub fn current_pos(&mut self) -> Option<usize> {
        self.chars.peek().map(|(pos, _)| *pos)
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
            Some((idx, c)) => {
                let tok = match c {
                    'a'..='z' | 'A'..='Z' => self.read_identifier(),
                    '0'..='9' => self.read_int(),
                    '=' => {
                        self.chars.next();
                        Token::Assign
                    }
                    '+' => {
                        self.chars.next();
                        Token::Plus
                    }
                    '-' => {
                        self.chars.next();
                        Token::Minus
                    }
                    '*' => {
                        self.chars.next();
                        Token::Asterisk
                    }
                    '/' => {
                        self.chars.next();
                        Token::FSlash
                    }
                    '!' => {
                        self.chars.next();
                        Token::Bang
                    }
                    '<' => {
                        self.chars.next();
                        Token::LT
                    }
                    '>' => {
                        self.chars.next();
                        Token::GT
                    }
                    '(' => {
                        self.chars.next();
                        Token::LParen
                    }
                    ')' => {
                        self.chars.next();
                        Token::RParen
                    }
                    '{' => {
                        self.chars.next();
                        Token::LBrace
                    }
                    '}' => {
                        self.chars.next();
                        Token::RBrace
                    }
                    ',' => {
                        self.chars.next();
                        Token::Comma
                    }
                    ';' => {
                        self.chars.next();
                        Token::Semi
                    }
                    _ => Token::Illegal(*idx, c.to_string()),
                };
                Some(tok)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(usize, String),

    // Syntax
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semi,

    // Operators
    Plus,
    Asterisk,
    FSlash,
    Bang,
    GT,
    LT,
    Minus,

    // Keywords
    Let,
    Func,
    Assign,
    True,
    False,

    // Values
    Ident(String),
    Int(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn expect_tokens(input: &str, tokens: Vec<Token>) {
        let mut lex = Lexer::new(input);

        for (i, ex) in tokens.into_iter().enumerate() {
            let tok = lex.next().unwrap();
            let pos = lex.current_pos().unwrap_or(0);
            assert_eq!(
                ex,
                tok,
                "expectation: {}, source char: {},\naround here in source:\n----------\n{}\n----------",
                i + 1,
                pos,
                input
                    .chars()
                    .skip(if pos < 5 { 0 } else { pos - 5 })
                    .take(10)
                    .collect::<String>()
                    .escape_default(),
            )
        }
        assert_eq!(
            None,
            lex.next(),
            "expected end of input but found more tokens"
        );
    }

    #[test]
    fn test_lexer_some_tokens() {
        expect_tokens(
            "a+b; a-b; a*b; a/b;
            true;
            false;
            !a;
            a > b; b < c;",
            vec![
                Token::Ident("a".to_string()),
                Token::Plus,
                Token::Ident("b".to_string()),
                Token::Semi,
                Token::Ident("a".to_string()),
                Token::Minus,
                Token::Ident("b".to_string()),
                Token::Semi,
                Token::Ident("a".to_string()),
                Token::Asterisk,
                Token::Ident("b".to_string()),
                Token::Semi,
                Token::Ident("a".to_string()),
                Token::FSlash,
                Token::Ident("b".to_string()),
                Token::Semi,
                Token::True,
                Token::Semi,
                Token::False,
                Token::Semi,
                Token::Bang,
                Token::Ident("a".to_string()),
                Token::Semi,
                Token::Ident("a".to_string()),
                Token::GT,
                Token::Ident("b".to_string()),
                Token::Semi,
                Token::Ident("b".to_string()),
                Token::LT,
                Token::Ident("c".to_string()),
                Token::Semi,
            ],
        )
    }

    #[test]
    fn test_lexer_let_int() {
        expect_tokens(
            "let five = 5;",
            vec![
                Token::Let,
                Token::Ident("five".to_string()),
                Token::Assign,
                Token::Int("5".to_string()),
                Token::Semi,
            ],
        )
    }

    #[test]
    fn test_lexer_multiple_lets() {
        expect_tokens(
            "let five = 5;
             let ten = 10;",
            vec![
                Token::Let,
                Token::Ident("five".to_string()),
                Token::Assign,
                Token::Int("5".to_string()),
                Token::Semi,
                Token::Let,
                Token::Ident("ten".to_string()),
                Token::Assign,
                Token::Int("10".to_string()),
                Token::Semi,
            ],
        )
    }

    #[test]
    fn test_lexer_function_and_call() {
        expect_tokens(
            "
    let add = fn(x, y) {
         x + y;
    };
    let result = add(5, 10);",
            vec![
                Token::Let,
                Token::Ident("add".to_string()),
                Token::Assign,
                Token::Func,
                Token::LParen,
                Token::Ident("x".to_string()),
                Token::Comma,
                Token::Ident("y".to_string()),
                Token::RParen,
                Token::LBrace,
                Token::Ident("x".to_string()),
                Token::Plus,
                Token::Ident("y".to_string()),
                Token::Semi,
                Token::RBrace,
                Token::Semi,
                Token::Let,
                Token::Ident("result".to_string()),
                Token::Assign,
                Token::Ident("add".to_string()),
                Token::LParen,
                Token::Int("5".to_string()),
                Token::Comma,
                Token::Int("10".to_string()),
                Token::RParen,
                Token::Semi,
            ],
        )
    }
}
