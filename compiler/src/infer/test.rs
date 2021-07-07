use crate::{
    lexer,
    parser::{Parse, ParseCursor, Ast},
    transformer::{var_to_u32::VarStatementsTransformer, Transformer},
};

use super::ty_infer;

fn expect_type_check_pass(input: String) {
    let tokens = lexer::lex(input).unwrap();
    let parsed = Ast::parse(&mut ParseCursor::new(tokens)).unwrap();
    let transformed = VarStatementsTransformer::default().transform(parsed);
    let res = ty_infer(transformed);
    if let Err(e) = res {
        panic!("Expected this test to pass, *but* it failed! Err: {:#?}", e);
    }
}

fn expect_type_check_fail(input: String) {
    let tokens = lexer::lex(input).unwrap();
    let parsed = Ast::parse(&mut ParseCursor::new(tokens)).unwrap();
    let transformed = VarStatementsTransformer::default().transform(parsed);
    let res = ty_infer(transformed);
    if let Ok(t) = res {
        panic!("Expected this test to fail, *but* it passed! {:#?}", t);
    }
}

#[test]
fn test_valid_is_ok() {
    expect_type_check_pass("x = 13 + 15\ny = 5\nx = y + 10".to_string());
    expect_type_check_pass("true AND (false OR (NOT true))".to_string());
    expect_type_check_pass("print(true)".to_string());
}

#[test]
fn test_invalid_errors() {
    expect_type_check_fail("y = 5 \ny = true".into());
    expect_type_check_fail("function x(y,)\n    x AND y\nendfunction\n x(5)".into());
    expect_type_check_fail("x = 13 + 15\ny = true\nx = y".into());
    expect_type_check_fail("true AND (false OR (NOT 10))".to_string());
}
