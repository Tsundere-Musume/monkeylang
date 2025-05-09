use crate::{
    ast::{BlockStatement, Expression, Identifier, Program, Statement},
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
    errors: Vec<String>,
    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(l: Lexer<'a>) -> Self {
        let mut parser = Parser {
            l,
            errors: vec![],
            cur_token: Token::EOF,
            peek_token: Token::EOF,
        };

        parser.next_token();
        parser.next_token();

        return parser;
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token: Token) {
        self.errors.push(format!(
            "expected next token to be {}, got {} instead.",
            token, self.cur_token
        ))
    }

    fn no_prefix_fn_error(&mut self) {
        self.errors
            .push(format!("no prefix fn associated for {}", self.cur_token));
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

    fn run_prefix_fn(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::Ident(val) => Some(Expression::Identifier(Identifier(val.clone()))),
            Token::Int(val) => Some(Expression::Integer(*val)),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::True | Token::False => Some(Expression::Boolean(self.cur_token_is(Token::True))),
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            _ => {
                self.no_prefix_fn_error();
                None
            }
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left_exp = match self.run_prefix_fn() {
            Some(expr) => Some(expr),
            None => return None,
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
                Token::Lparen => {
                    self.next_token();
                    self.parse_call_expression(left_exp.unwrap())
                }
                _ => break,
            }
        }
        return left_exp;
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let arguments = self.parse_call_arguments();
        Some(Expression::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut arguments = vec![];

        if self.peek_token_is(Token::Rparen) {
            self.next_token();
            return arguments;
        }

        self.next_token();
        //WARN: not checked
        arguments.push(self.parse_expression(Precedence::Lowest).unwrap());

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();

            arguments.push(self.parse_expression(Precedence::Lowest).unwrap());
        }

        if !self.expect_peek(Token::Rparen) {
            return vec![];
        }

        arguments
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        if !self.expect_peek(Token::Lparen) {
            return None;
        }

        let parameters = match self.parse_function_parameters() {
            Some(params) => params,
            //TODO: parse the function body before returing the error
            None => return None,
        };

        if !self.expect_peek(Token::Lbrace) {
            return None;
        }
        let body = self.parse_block_statement();

        Some(Expression::Function { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Expression>> {
        let mut params = vec![];

        if self.peek_token_is(Token::Rparen) {
            self.next_token();
            return Some(params);
        }

        self.next_token();
        match &self.cur_token {
            Token::Ident(val) => params.push(Expression::Identifier(Identifier(val.clone()))),
            _ => todo!("error handling for arguments"),
        };

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            match &self.cur_token {
                Token::Ident(val) => params.push(Expression::Identifier(Identifier(val.clone()))),
                _ => todo!("error handling for arguments"),
            };
        }

        if !self.expect_peek(Token::Rparen) {
            return None;
        }

        Some(params)
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

        let alternative = if self.peek_token_is(Token::Else) {
            self.next_token();
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
            Token::Lparen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        // WARN: doesn't check for EOF
        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => todo!("couldn't parse an expression"),
        };

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        return Some(Statement::Return(expression));
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

        self.next_token();

        // WARN: doesn't check for EOF
        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => todo!("couldn't parse an expression"),
        };

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        return Some(Statement::Let(Identifier(name), expression));
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if token != self.peek_token {
            self.peek_error(token);
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
