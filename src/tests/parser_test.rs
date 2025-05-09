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
    let tests = vec![
        ("let x = 5;", "x", Expression::Integer(5)),
        ("let y = true;", "y", Expression::Boolean(true)),
        (
            "let foobar = major;",
            "foobar",
            Expression::Identifier(Identifier(String::from("major"))),
        ),
    ];
    for (input, expected_ident, expected_expr) in tests {
        let l = lexer::Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statetments does not contain 1 statements. got={}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        match stmt {
            Statement::Let(Identifier(name), expression) => {
                assert_eq!(
                    expected_ident, name,
                    "identifier name doesn't match : {}, got = {}",
                    expected_ident, name
                );
                assert_eq!(
                    *expression, expected_expr,
                    "rvalue doesn't match : {}, got = {}",
                    expected_expr, expression
                );
            }
            _ => assert!(false, "not a let statement"),
        }
    }
}

fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();
    if errors.len() != 0 {
        let mut error = format!("parser had {} errors\n", errors.len());
        for e in errors {
            error.push_str(format!("parser error: {}\n", e).as_str());
        }
        panic!("{}", error)
    }
}

#[test]
fn test_identifier_expr() {
    let input = "foobar";
    let l = lexer::Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();
    check_parser_errors(&parser);
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
    let tests = vec![
        ("return 5", Expression::Integer(5)),
        ("return false;", Expression::Boolean(false)),
        (
            "return major",
            Expression::Identifier(Identifier(String::from("major"))),
        ),
    ];
    for (input, expected_expr) in tests {
        let l = lexer::Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statetments does not contain 1 statements. got={}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        match stmt {
            Statement::Return(expr) => {
                assert_eq!(
                    *expr, expected_expr,
                    "return value mismatch: expected = {}, got = {}",
                    expected_expr, expr
                )
            }
            _ => panic!("not a return statement"),
        }
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";
    let l = lexer::Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();

    check_parser_errors(&parser);
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

        check_parser_errors(&parser);
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

        check_parser_errors(&parser);
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
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
    ];

    for (input, expected) in tests {
        let l = lexer::Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();

        check_parser_errors(&parser);
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
        check_parser_errors(&parser);
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

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";

    let l = lexer::Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();

    check_parser_errors(&parser);
    assert_eq!(
        program.statements.len(),
        1,
        "program.statetments does not contain 1 statements. got={}",
        program.statements.len()
    );

    match &program.statements[0] {
        Statement::ExpressionStmt(Expression::If {
            condition,
            consequence,
            alternative,
        }) => {
            assert_infix_expression(&**condition, "x", Token::Lt, "y");

            assert_eq!(
                1,
                consequence.0.len(),
                "consequence block doesn't contain 1 statement, got = {}",
                consequence.0.len()
            );

            match &consequence.0[0] {
                Statement::ExpressionStmt(Expression::Identifier(Identifier(val))) => {
                    assert_eq!(val, "x")
                }
                _ => panic!("expected an identifier 'x', got = {}", &consequence.0[0]),
            }

            assert_eq!(*alternative, None, "alternative block not expected");
        }
        _ => panic!("not an if expression"),
    }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";

    let l = lexer::Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();

    check_parser_errors(&parser);
    assert_eq!(
        program.statements.len(),
        1,
        "program.statetments does not contain 1 statements. got={}",
        program.statements.len()
    );

    match &program.statements[0] {
        Statement::ExpressionStmt(Expression::If {
            condition,
            consequence,
            alternative,
        }) => {
            assert_infix_expression(&**condition, "x", Token::Lt, "y");

            assert_eq!(
                1,
                consequence.0.len(),
                "consequence block doesn't contain 1 statement, got = {}",
                consequence.0.len()
            );

            match &consequence.0[0] {
                Statement::ExpressionStmt(Expression::Identifier(Identifier(val))) => {
                    assert_eq!(val, "x")
                }
                _ => panic!("expected an identifier 'x', got = {}", &consequence.0[0]),
            }

            if let Some(alternative) = alternative {
                assert_eq!(
                    1,
                    alternative.0.len(),
                    "alternative block doesn't contain 1 statement, got = {}",
                    alternative.0.len()
                );

                match &alternative.0[0] {
                    Statement::ExpressionStmt(Expression::Identifier(Identifier(val))) => {
                        assert_eq!(val, "y")
                    }
                    _ => panic!("expected an identifier 'y', got = {}", &alternative.0[0]),
                }
            } else {
                panic!("expected an alternative block");
            }
        }
        _ => panic!("not an if expression"),
    }
}

//TODO: make these functions work with any literals
fn assert_infix_expression(expression: &Expression, left: &str, operator: Token, right: &str) {
    let expected = Expression::Infix {
        left: Box::new(Expression::Identifier(Identifier(String::from(left)))),
        op: operator,
        right: Box::new(Expression::Identifier(Identifier(String::from(right)))),
    };

    assert_eq!(
        *expression, expected,
        "conditional expression don't match: {}, got: {}",
        expression, expected
    );
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";

    let l = lexer::Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();

    check_parser_errors(&parser);
    assert_eq!(
        program.statements.len(),
        1,
        "program.statetments does not contain 1 statements. got={}",
        program.statements.len()
    );

    match &program.statements[0] {
        Statement::ExpressionStmt(Expression::Function { parameters, body }) => {
            assert_eq!(
                parameters.len(),
                2,
                "function literal parameters wrong, want 2, got = {}",
                parameters.len()
            );

            assert_eq!(
                parameters[0],
                Expression::Identifier(Identifier(String::from("x"))),
                "expecting identifier {}, got = {}",
                "x",
                parameters[0]
            );

            assert_eq!(
                parameters[1],
                Expression::Identifier(Identifier(String::from("y"))),
                "expecting identifier {}, got = {}",
                "y",
                parameters[1]
            );

            match &body.0[0] {
                Statement::ExpressionStmt(infix) => {
                    assert_infix_expression(infix, "x", Token::Plus, "y")
                }
                _ => panic!("statement should be an infix expression statement"),
            }
        }
        _ => panic!("program.statements[0] is not a function expression"),
    }
}

#[test]
fn test_function_parameter_parsing() {
    let tests = vec![
        ("fn() {};", vec![]),
        ("fn(x) {};", vec!["x"]),
        ("fn(x, y, z) {};", vec!["x", "y", "z"]),
    ];

    for (input, expected_params) in tests {
        let l = lexer::Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        match &program.statements[0] {
            Statement::ExpressionStmt(Expression::Function {
                parameters,
                body: _,
            }) => {
                assert_eq!(
                    parameters.len(),
                    expected_params.len(),
                    "length parameters wrong, want = {}, got = {}",
                    expected_params.len(),
                    parameters.len()
                );

                for (idx, ident) in expected_params.iter().enumerate() {
                    match &parameters[idx] {
                        Expression::Identifier(Identifier(val)) => {
                            assert_eq!(val, ident, "identifier value not {}, got {}", ident, val);
                        }
                        _ => panic!("expected an identifier expression"),
                    }
                }
            }
            _ => panic!("not a function literal"),
        };
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let l = lexer::Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();

    check_parser_errors(&parser);
    assert_eq!(
        program.statements.len(),
        1,
        "program.statetments does not contain 1 statements. got={}",
        program.statements.len()
    );

    match &program.statements[0] {
        Statement::ExpressionStmt(Expression::Call {
            function,
            arguments,
        }) => {
            match &**function {
                Expression::Identifier(Identifier(name)) => assert_eq!(
                    name, "add",
                    "epected identifer name = {}, got = {}",
                    "add", name
                ),
                _ => panic!("not an identifer expression"),
            };

            assert_eq!(
                arguments.len(),
                3,
                "expected 3 arguments, got = {}",
                arguments.len()
            );
            assert_integer_literal(&arguments[0], 1);
            let make_infix_int = |x, tok, y| Expression::Infix {
                left: Box::new(Expression::Integer(x)),
                op: tok,
                right: Box::new(Expression::Integer(y)),
            };
            assert_eq!(
                arguments[1],
                make_infix_int(2, Token::Asterisk, 3),
                "expecting 2 * 3, got = {}",
                arguments[1]
            );
            assert_eq!(
                arguments[2],
                make_infix_int(4, Token::Plus, 5),
                "expecting 4 + 5, got = {}",
                arguments[2]
            );
        }
        _ => panic!("expected a call expression",),
    };
}

fn assert_integer_literal(expression: &Expression, expected: i64) {
    match expression {
        Expression::Integer(val) => assert_eq!(
            *val, expected,
            "integer value expected = {}, got = {}",
            expected, val
        ),
        _ => panic!("[{}]: not an integer literal expression", expression),
    };
}
