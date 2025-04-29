#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,
    // Identifiers + literal,
    Ident(String),
    Int(i64),
    // Delimiter,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    // Operator,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
    // Keyword,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn lookup_ident(name: &str) -> Token {
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
}
