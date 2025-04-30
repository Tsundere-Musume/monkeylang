pub struct Program {
    pub statements: Vec<Statement>,
}

pub struct Identifier(pub String);

pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    ExpressionStmt(Expression),
}

pub enum Expression {
    Expression,
}
