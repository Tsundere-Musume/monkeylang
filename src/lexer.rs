use std::i64;

use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    read_ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            read_ch: 0,
        };
        lexer.read_char();
        return lexer;
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.read_ch = 0;
        } else {
            self.read_ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.read_ch {
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,
            b'{' => Token::Lbrace,
            b'}' => Token::Rbrace,
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'<' => Token::Lt,
            b'>' => Token::Gt,
            0 => Token::EOF,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            b'_' | b'A'..=b'Z' | b'a'..=b'z' => return self.identifierize(),
            b'0'..=b'9' => {
                let val = self.read_number();
                return Token::Int(val);
            }
            _ => Token::Illegal,
        };
        self.read_char();
        return tok;
    }

    fn read_number(&mut self) -> i64 {
        let pos = self.position;
        while (b'0'..=b'9').contains(&self.read_ch) {
            self.read_char();
        }

        let lexeme = &self.input[pos..self.position];
        lexeme.parse().unwrap()
    }

    fn identifierize(&mut self) -> Token {
        let pos = self.position;
        loop {
            match self.read_ch {
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => self.read_char(),
                _ => break,
            }
        }

        let identifier = &self.input[pos..self.position];
        Lexer::create_ident(identifier)
    }

    fn create_ident(name: &str) -> Token {
        match name {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(String::from(name)),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.read_ch == b' '
            || self.read_ch == b'\t'
            || self.read_ch == b'\r'
            || self.read_ch == b'\n'
        {
            self.read_char();
        }
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;
    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;
"#;

        #[rustfmt::skip]
        let tests = vec![
            Token::Let, Token::Ident(String::from("five")), Token::Assign, Token::Int(5), Token::Semicolon,

            Token::Let, Token::Ident(String::from("ten")), Token::Assign, Token::Int(10), Token::Semicolon,

            Token::Let, Token::Ident(String::from("add")), Token::Assign, Token::Function, Token::Lparen, Token::Ident(String::from("x")), Token::Comma, Token::Ident(String::from("y")), Token::Rparen, Token::Lbrace,
            Token::Ident(String::from("x")), Token::Plus, Token::Ident(String::from("y")), Token::Semicolon,
            Token::Rbrace, Token::Semicolon,

            Token::Let, Token::Ident(String::from("result")), Token::Assign, Token::Ident(String::from("add")), Token::Lparen, Token::Ident(String::from("five")), Token::Comma, Token::Ident(String::from("ten")), Token::Rparen, Token::Semicolon,

            Token::Bang, Token::Minus, Token::Slash, Token::Asterisk, Token::Int(5), Token::Semicolon,

            Token::Int(5), Token::Lt, Token::Int(10), Token::Gt, Token::Int(5), Token::Semicolon,

            Token::If, Token::Lparen, Token::Int(5), Token::Lt, Token::Int(10), Token::Rparen, Token::Lbrace,
            Token::Return, Token::True, Token::Semicolon,
            Token::Rbrace, Token::Else, Token::Lbrace,
            Token::Return, Token::False, Token::Semicolon,
            Token::Rbrace,
            Token::Int(10), Token::Eq, Token::Int(10), Token::Semicolon,
            Token::Int(10), Token::NotEq, Token::Int(9), Token::Semicolon,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);

        for expect in tests {
            let tok = lexer.next_token();

            assert_eq!(expect, tok);
        }
    }
}
