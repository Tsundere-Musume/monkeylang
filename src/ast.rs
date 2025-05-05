use std::fmt;

use crate::token::Token;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut repr = String::new();
        for stmt in &self.statements {
            repr.push_str(stmt.to_string().as_str());
        }
        write!(f, "{}", repr)
    }
}

#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    ExpressionStmt(Expression),
}
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Statement::*;
        match self {
            Let(ident, expr) => write!(f, "let {} = {};", ident.0, expr),
            Return(expr) => write!(f, "return {}", expr),
            ExpressionStmt(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Integer(i64),
    Prefix {
        op: Token,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        op: Token,
        right: Box<Expression>,
    },
    Boolean(bool),
    Expression,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident.0),
            Expression::Integer(val) => write!(f, "{}", val),
            Expression::Prefix { op, right } => write!(f, "({}{})", op, right),
            Expression::Infix { left, op, right } => write!(f, "({} {} {})", left, op, right),
            Expression::Boolean(val) => write!(f, "{}", val),
            Expression::Expression => write!(f, ""),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Expression, Identifier, Program, Statement};
    #[test]
    fn test_ast_string() {
        let program = Program {
            statements: vec![Statement::Let(
                Identifier(String::from("my_var")),
                Expression::Identifier(Identifier(String::from("another_var"))),
            )],
        };
        assert_eq!(
            program.to_string(),
            "let my_var = another_var;",
            "program.to_string() wrong. got = {}",
            program.to_string()
        );
    }
}
