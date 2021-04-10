#![no_main]
extern crate libfuzzer_sys;
extern crate compiler;
use compiler::parser::{Parse, Statements, ParseCursor};
use compiler::lexer;

libfuzzer_sys::fuzz_target!(|statements: Statements| {
    let program = statements.to_string();
    let mut cursor = ParseCursor::new(lexer::lex(program).unwrap());
    let statements = Statements::parse(&mut cursor);
    assert!(!statements.is_err());
});
