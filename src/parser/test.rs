use std::fmt::Debug;

use crate::lexer::lex;

use super::{Expression, Parse, ParseCursor, Statement};

fn test_parse<T>(input: &str, should_parse: bool)
where
    T: Parse + Debug,
{
    let result = T::parse(&mut ParseCursor::new(lex(input.to_string()).unwrap()));
    if should_parse {
        if result.is_err() {
            println!("{:?}", result.unwrap_err());
            panic!("The input `{}` should have passed, but it failed.", input)
        }
    } else {
        if result.is_ok() {
            println!("{:?}", result.unwrap_err());
            panic!("The input `{}` should have failed, but it passed.", input)
        }
    }
}

#[test]
fn test_expressions() {
    test_parse::<Expression>("(((12 + 8)))", true);
    test_parse::<Expression>("1 + 2 + 3 + 4 + 5", true);
    test_parse::<Expression>("1 / (2 / 3)", true);
    test_parse::<Expression>("x", true);
    test_parse::<Expression>("1 / x", true);
    test_parse::<Expression>("1 / (2 / (3 + x))", true);
    test_parse::<Expression>("x[0][1]", true);
    test_parse::<Expression>("print(\"12\")", true);
    test_parse::<Expression>("multiply(13, 14)", true);
}

#[test]
fn test_for_statements() {
    test_parse::<Statement>(
        r#"for i=(1*2) to 12 do
    print("12")
    print(i)
endfor"#,
        true,
    );
    test_parse::<Statement>(
        r#"for i=x to (15 - 12) do
    print("12")
    print(i)
endfor"#,
        true,
    );
}

#[test]
fn test_while_statement() {
    test_parse::<Statement>(
        r#"while (x==12) do
    print("12")
    print(i)
endwhile"#,
        true,
    );
    test_parse::<Statement>(
        r#"while (x==12) OR (y==13) do
            print("12")
            print(i)
        endwhile"#,
        true,
    );
    test_parse::<Statement>(
        r#"while (z=="hello") AND (a==(12 + 12)) do
            print("12")
            print(i)
        endwhile"#,
        true,
    );
}
