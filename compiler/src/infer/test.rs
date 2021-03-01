use crate::{
    lexer,
    parser::{Parse, ParseCursor, Statements},
    transformer::{var_to_u32::VarStatementsTransformer, Transformer},
};

use super::ty_infer;

fn expect_type_check_pass(input: String) {
    let tokens = lexer::lex(input).unwrap();
    let parsed = Statements::parse(&mut ParseCursor::new(tokens)).unwrap();
    let transformed = VarStatementsTransformer::default().transform(parsed);
    let res = ty_infer(transformed);
    if let Err(e) = res {
        panic!("Expected this test to pass, *but* it failed! Err: {:#?}", e);
    }
}

fn expect_type_check_fail(input: String) {
    let tokens = lexer::lex(input).unwrap();
    let parsed = Statements::parse(&mut ParseCursor::new(tokens)).unwrap();
    let transformed = VarStatementsTransformer::default().transform(parsed);
    let res = ty_infer(transformed);
    if let Ok(t) = res {
        panic!("Expected this test to fail, *but* it passed! {:#?}", t);
    }
}

#[test]
fn test_valid_is_ok() {
    let programs = [r#"x=12"#];
    for program in programs.iter() {
        expect_type_check_pass(program.to_string());
    }
}

#[test]
fn test_invalid_errors() {
    let programs = ["y = 5 \ny = true"];
    for program in programs.iter() {
        expect_type_check_fail(program.to_string());
    }
}
