use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::Token,
};

pub struct Parser<'a> {
    l: Lexer<'a>,

    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(l: Lexer<'a>) -> Self {
        let mut parser = Parser {
            l,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
        };

        parser.next_token();
        parser.next_token();

        return parser;
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.cur_token != Token::EOF {
            match self.parse_statement() {
                Some(stmt) => program.statements.push(stmt),
                None => (),
            };
            self.next_token();
        }
        return program;
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => todo!(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let name = match &self.peek_token {
            Token::Ident(val) => val.clone(),
            _ => return None,
        };
        self.next_token();

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }

        return Some(Statement::Let(Identifier(name), Expression::Expression));
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if token != self.peek_token {
            false
        } else {
            self.next_token();
            true
        }
    }

    fn cur_token_is(&self, token: Token) -> bool {
        token == self.cur_token
    }
    // fn peek_token_is(&self, token: Token) -> bool {
    //     token == self.peek_token
    // }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Identifier, Statement},
        lexer,
    };

    use super::Parser;

    #[test]
    fn test_let_statement() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;
        let l = lexer::Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();

        assert_eq!(
            program.statements.len(),
            3,
            "program.statetments does not contain 3 statements. got={}",
            program.statements.len()
        );

        let tests = vec!["x", "y", "foobar"];
        for (idx, stmt) in program.statements.iter().enumerate() {
            match stmt {
                Statement::Let(Identifier(name), _) => {
                    assert_eq!(tests[idx], name, "identifier name doesn't match.")
                }
                _ => assert!(false, "not a let statement"),
            }
        }
    }
}
