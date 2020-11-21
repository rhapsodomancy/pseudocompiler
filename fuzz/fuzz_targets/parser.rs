#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate pseudocompiler;
use pseudocompiler::parser::{Parse, Statements, ParseCursor};
use pseudocompiler::lexer;

/// Tests that all valid targets test and fail.
libfuzzer_sys::fuzz_target!(|statements: Statements| {
    let program = statements.to_string();
    let mut cursor = ParseCursor::new(lexer::lex(program).unwrap());
    let statements = Statements::parse(&mut cursor);
    assert!(!statements.is_err());
});
