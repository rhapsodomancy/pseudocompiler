use super::{Lex, LexCursor, SpannedToken};

macro_rules! test_case {
    ($x:expr, $pass:expr) => {
        use super::lex;
        let lexed = lex($x.to_string());
        match lexed {
            Ok(output) => {
                if !($pass) {
                    panic!("Expected this test to fail but it passed. {:?}", output)
                }
            }
            Err(error) => {
                if $pass {
                    panic!("Expected this test to pass but it failed. {:?}", error);
                }
            }
        }
    };
}

#[test]
fn test_lex_string() {
    test_case!(
        r#"
"hello world"
"#,
        true
    );
}

#[test]
fn test_lex_bool_true() {
    test_case!(
        r#"
true
"#,
        true
    );
}

#[test]
fn test_lex_bool_false() {
    test_case!(
        r#"
false
"#,
        true
    );
}

#[test]
fn test_for_loop() {
    test_case!(
        r#"
for i=1 to 4 do
    print("1")
endfor"#,
        true
    );
}

#[test]
fn test_while_loop() {
    test_case!(
        r#"
while true do
    print("13")
endwhile"#,
        true
    );
}

#[test]
fn test_while_loop_with_more_complex_condition() {
    test_case!(
        r#"
while x=="13" AND b=="16" AND (d * 8) == 32 do
    print("13")
endwhile"#,
        true
    );
}

#[test]
fn test_basic_expressions() {
    test_case!(r#"12 * 8"#, true);
}
