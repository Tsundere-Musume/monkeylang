use crate::{
    ast::{BlockStatement, Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    //TODO
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
        let stmt = match expr {
            Some(expr) => Some(Statement::ExpressionStmt(expr)),
            None => None,
        };

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        return stmt;
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left_exp = match &self.cur_token {
            Token::Ident(val) => Some(Expression::Identifier(Identifier(val.clone()))),
            Token::Int(val) => Some(Expression::Integer(*val)),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::True | Token::False => Some(Expression::Boolean(self.cur_token_is(Token::True))),
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            _ => return None,
        };

        while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
            left_exp = match &self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Eq
                | Token::NotEq
                | Token::Lt
                | Token::Gt => {
                    self.next_token();
                    //TODO: check unwrap thing
                    self.parse_infix_expression(left_exp.unwrap())
                }
                _ => break,
            }
        }
        return left_exp;
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(Token::Lparen) {
            return None;
        }
        self.next_token();

        let condition = match self.parse_expression(Precedence::Lowest) {
            Some(cond) => cond,
            None => return None,
        };

        if !self.expect_peek(Token::Rparen) {
            return None;
        }

        if !self.expect_peek(Token::Lbrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.expect_peek(Token::Else) {
            if !self.expect_peek(Token::Lbrace) {
                None
            } else {
                Some(self.parse_block_statement())
            }
        } else {
            None
        };

        return Some(Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        });
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements = vec![];
        self.next_token();

        while !self.cur_token_is(Token::Rbrace) && !self.cur_token_is(Token::EOF) {
            match self.parse_statement() {
                Some(statement) => statements.push(statement),
                None => {}
            }
            self.next_token();
        }

        BlockStatement(statements)
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(Token::Rparen) {
            None
        } else {
            expression
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

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        match self.parse_expression(precedence) {
            Some(exp) => Some(Expression::Infix {
                left: Box::new(left),
                op: token,
                right: Box::new(exp),
            }),
            None => None,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        Self::get_operator_precedence(&self.cur_token)
    }

    fn peek_precedence(&self) -> Precedence {
        Self::get_operator_precedence(&self.peek_token)
    }

    fn get_operator_precedence(token: &Token) -> Precedence {
        match token {
            Token::Eq => Precedence::Equals,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Asterisk => Precedence::Product,
            Token::Slash => Precedence::Product,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::NotEq => Precedence::Equals,
            _ => Precedence::Lowest,
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

    fn peek_token_is(&self, token: Token) -> bool {
        token == self.peek_token
    }
}
