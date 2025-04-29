use std::io::{self, Write};

use crate::{lexer::Lexer, token::Token};

const PROMPT: &'static str = "> ";

pub fn start() {
    loop {
        print!("{PROMPT}");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .expect("Failed to read user input.");
        let mut lexer = Lexer::new(&line[..]);

        loop {
            match lexer.next_token() {
                Token::EOF => break,
                token => println!("\t| {:?}", token),
            }
        }
    }
}
