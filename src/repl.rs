use std::io::{self, Write};

use crate::{lexer::Lexer, parser::Parser};

const PROMPT: &'static str = "> ";

const MONKEY_FACE: &'static str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start() {
    loop {
        print!("{PROMPT}");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .expect("Failed to read user input.");
        let lexer = Lexer::new(&line[..]);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if parser.errors().len() != 0 {
            print_parser_errors(parser.errors());
            continue;
        }

        println!("{}", program);
    }
}

fn print_parser_errors(errors: &Vec<String>) {
    println!("{}", MONKEY_FACE);
    println!("Woops! We ran into some monkey business here!\n");
    println!(" parser errors:");
    for error in errors {
        println!("\t{}", error)
    }
}
