use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

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
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression(Precedence::Lowest);
        match expr {
            Some(expr) => Some(Statement::ExpressionStmt(expr)),
            None => None,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        match &self.cur_token {
            Token::Ident(val) => Some(Expression::Identifier(Identifier(val.clone()))),
            Token::Int(val) => Some(Expression::Integer(*val)),
            Token::Bang => self.parse_prefix_expression(),
            Token::Minus => self.parse_prefix_expression(),
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        self.next_token();
        match self.parse_expression(Precedence::Prefix) {
            Some(exp) => Some(Expression::Prefix {
                op: token,
                right: Box::new(exp),
            }),
            None => None,
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        // WARN: doesn't check for EOF
        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }
        return Some(Statement::Return(Expression::Expression));
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

        // WARN: doesn't check for EOF
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
