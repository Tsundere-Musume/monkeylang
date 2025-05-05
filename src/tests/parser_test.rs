use core::panic;

use crate::{
    ast::{Expression, Identifier, Statement},
    lexer,
    parser::Parser,
    token::Token,
};

//TODO: parser errors
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

#[test]
fn test_identifier_expr() {
    let input = "foobar";
    let l = lexer::Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();

    assert_eq!(
        program.statements.len(),
        1,
        "program.statetments does not contain 1 statements. got={}",
        program.statements.len()
    );

    match &program.statements[0] {
        Statement::ExpressionStmt(ident @ Expression::Identifier(_)) => {
            assert_eq!(
                input,
                ident.to_string(),
                "identifier value not {}. got = {}",
                input,
                ident
            );
        }
        stmt => panic!("not an expression statement: got {:?}", stmt),
    };
}

#[test]
fn test_return_statement() {
    let input = r#"
            return 5;
            return 10;
            return 993322;
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

    for stmt in program.statements {
        if !matches!(stmt, Statement::Return(_)) {
            panic!("not a return statement");
        }
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";
    let l = lexer::Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();

    assert_eq!(
        program.statements.len(),
        1,
        "program.statetments does not contain 1 statements. got={}",
        program.statements.len()
    );

    match program.statements[0] {
        Statement::ExpressionStmt(Expression::Integer(val)) => {
            assert_eq!(val, 5, "integer value not {}, got = {}", 5, val)
        }
        _ => panic!("not a integer literal expression"),
    };
}

#[test]
fn test_parsing_prefix_expressions() {
    let tests = vec![("!5", Token::Bang, "!", 5), ("-15", Token::Minus, "-", 15)];

    for (input, ex_tok, ex_op, val) in tests {
        let l = lexer::Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        assert_eq!(
            1,
            program.statements.len(),
            "program.statement does not contain {} statements. got = {}",
            1,
            program.statements.len()
        );

        let stmt = &program.statements[0];
        match stmt {
            Statement::ExpressionStmt(Expression::Prefix { op, right }) => {
                assert_eq!(
                    ex_tok, *op,
                    "prefix expresion token doesn't match: {:?}, got = {:?}",
                    ex_tok, op
                );
                assert_eq!(
                    ex_op,
                    (*op).to_string().as_str(),
                    "prefix expression operator str don't match: {}, got = {}",
                    ex_op,
                    (*op).to_string().as_str()
                );
                match **right {
                    Expression::Integer(v) => assert_eq!(v, val),
                    _ => panic!("boxed value is not an integer"),
                }
            }
            _ => panic!("not a infix expression"),
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let tests = vec![
        ("5 + 5", 5, Token::Plus, "+", 5),
        ("5 - 5;", 5, Token::Minus, "-", 5),
        ("5 * 5;", 5, Token::Asterisk, "*", 5),
        ("5 / 5;", 5, Token::Slash, "/", 5),
        ("5 > 5;", 5, Token::Gt, ">", 5),
        ("5 < 5;", 5, Token::Lt, "<", 5),
        ("5 == 5;", 5, Token::Eq, "==", 5),
        ("5 != 5;", 5, Token::NotEq, "!=", 5),
    ];
    for (input, ex_left, ex_tok, ex_op, ex_right) in tests {
        let l = lexer::Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        assert_eq!(
            1,
            program.statements.len(),
            "program.statement does not contain {} statements. got = {}",
            1,
            program.statements.len()
        );

        match &program.statements[0] {
            Statement::ExpressionStmt(Expression::Infix { left, op, right }) => {
                match **left {
                    Expression::Integer(v) => assert_eq!(v, ex_left),
                    _ => panic!("left value is not an integer: {:?}", left),
                }
                assert_eq!(
                    ex_tok, *op,
                    "infix expresion token doesn't match: {:?}, got = {:?}",
                    ex_tok, op
                );
                assert_eq!(
                    ex_op,
                    (*op).to_string().as_str(),
                    "infix expression operator str don't match: {}, got = {}",
                    ex_op,
                    (*op).to_string().as_str()
                );
                match **right {
                    Expression::Integer(v) => assert_eq!(v, ex_right),
                    _ => panic!("right value is not an integer: {:?}", right),
                }
            }
            _ => panic!("not an infix expression: {}", &program.statements[0]),
        };
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
    ];

    for (input, expected) in tests {
        let l = lexer::Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        assert_eq!(program.to_string(), expected);
    }
}

#[test]
fn test_boolean_expression() {
    let tests = vec![("true", true), ("false", false)];

    for (input, expected) in tests {
        let l = lexer::Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        assert_eq!(
            program.statements.len(),
            1,
            "program.statetments does not contain 1 statements. got={}",
            program.statements.len()
        );

        match program.statements[0] {
            Statement::ExpressionStmt(Expression::Boolean(val)) => {
                assert_eq!(
                    val, expected,
                    "boolean value not {}, got = {}",
                    expected, val
                )
            }
            _ => panic!("not a boolean literal expression"),
        };
    }
}
