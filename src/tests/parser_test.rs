use crate::{
    ast::{Expression, Identifier, Statement},
    lexer,
    parser::Parser,
};

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
