use super::{Lex, LexCursor, SpannedToken};

macro_rules! test_case {
    ($x:expr, $pass:expr) => {
        let x = $x;
        let mut cursor = LexCursor::new(x);
        let mut spanned_tokens = vec![];
        while cursor.input.len() > 0 {
            spanned_tokens.push(SpannedToken::lex(&mut cursor));
        }
        if $pass {
            for spanned_token in spanned_tokens {
                match spanned_token {
                    Ok(_) => {}
                    Err(token) => panic!(
                        "Expected this test to pass, but it failed! The output was: \n {:?}",
                        token
                    ),
                }
            }
        } else {
            for spanned_token in spanned_tokens {
                match spanned_token {
                    Ok(token) => panic!(
                        "Expected this test to fail, but it passed! The output was: \n {:?}",
                        token
                    ),
                    Err(_) => {}
                }
            }
        }
    };
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
