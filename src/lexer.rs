use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug, PartialEq, Eq, Hash)]
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
    Eq,
    NotEq,

    // Keywords
    Let,
    Func,
    Assign,
    True,
    False,
    If,
    Else,
    Return,

    // Values
    Ident(String),
    Int(String),
}

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
            "else" => Token::Else,
            "false" => Token::False,
            "fn" => Token::Func,
            "if" => Token::If,
            "let" => Token::Let,
            "return" => Token::Return,
            "true" => Token::True,
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
                        if let Some((_, '=')) = self.chars.peek() {
                            self.chars.next();
                            Token::Eq
                        } else {
                            Token::Assign
                        }
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
                        if let Some((_, '=')) = self.chars.peek() {
                            self.chars.next();
                            Token::NotEq
                        } else {
                            Token::Bang
                        }
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
                Token::Ident(String::from("a")),
                Token::Plus,
                Token::Ident(String::from("b")),
                Token::Semi,
                Token::Ident(String::from("a")),
                Token::Minus,
                Token::Ident(String::from("b")),
                Token::Semi,
                Token::Ident(String::from("a")),
                Token::Asterisk,
                Token::Ident(String::from("b")),
                Token::Semi,
                Token::Ident(String::from("a")),
                Token::FSlash,
                Token::Ident(String::from("b")),
                Token::Semi,
                Token::True,
                Token::Semi,
                Token::False,
                Token::Semi,
                Token::Bang,
                Token::Ident(String::from("a")),
                Token::Semi,
                Token::Ident(String::from("a")),
                Token::GT,
                Token::Ident(String::from("b")),
                Token::Semi,
                Token::Ident(String::from("b")),
                Token::LT,
                Token::Ident(String::from("c")),
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
                Token::Ident(String::from("five")),
                Token::Assign,
                Token::Int(String::from("5")),
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
                Token::Ident(String::from("five")),
                Token::Assign,
                Token::Int(String::from("5")),
                Token::Semi,
                Token::Let,
                Token::Ident(String::from("ten")),
                Token::Assign,
                Token::Int(String::from("10")),
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
                Token::Ident(String::from("add")),
                Token::Assign,
                Token::Func,
                Token::LParen,
                Token::Ident(String::from("x")),
                Token::Comma,
                Token::Ident(String::from("y")),
                Token::RParen,
                Token::LBrace,
                Token::Ident(String::from("x")),
                Token::Plus,
                Token::Ident(String::from("y")),
                Token::Semi,
                Token::RBrace,
                Token::Semi,
                Token::Let,
                Token::Ident(String::from("result")),
                Token::Assign,
                Token::Ident(String::from("add")),
                Token::LParen,
                Token::Int(String::from("5")),
                Token::Comma,
                Token::Int(String::from("10")),
                Token::RParen,
                Token::Semi,
            ],
        )
    }

    #[test]
    fn test_lexer_if_else_return() {
        expect_tokens(
            "if (a < b) {
                return c;
            } else {
                return d;
            }",
            vec![
                Token::If,
                Token::LParen,
                Token::Ident(String::from("a")),
                Token::LT,
                Token::Ident(String::from("b")),
                Token::RParen,
                Token::LBrace,
                Token::Return,
                Token::Ident(String::from("c")),
                Token::Semi,
                Token::RBrace,
                Token::Else,
                Token::LBrace,
                Token::Return,
                Token::Ident(String::from("d")),
                Token::Semi,
                Token::RBrace,
            ],
        )
    }

    #[test]
    fn test_lexer_equality() {
        expect_tokens(
            "a == b ; b != c;",
            vec![
                Token::Ident(String::from("a")),
                Token::Eq,
                Token::Ident(String::from("b")),
                Token::Semi,
                Token::Ident(String::from("b")),
                Token::NotEq,
                Token::Ident(String::from("c")),
                Token::Semi,
            ],
        )
    }
}
