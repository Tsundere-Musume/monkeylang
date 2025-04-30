use std::fmt;

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
            Let(ident, expr) => write!(f, "let {} = {}", ident.0, expr),
            _ => write!(f, ""),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Expression,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident.0),
            Expression::Expression => write!(f, ""),
        }
    }
}

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
            "let my_var = another_var",
            "program.to_string() wrong. got = {}",
            program.to_string()
        );
    }
}
