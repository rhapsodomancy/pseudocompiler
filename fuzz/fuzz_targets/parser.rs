#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate pseudocompiler;

/// Tests that all valid targets test and fail.
libfuzzer_sys::fuzz_target!(|statements: Statements| {
    let program = statements.to_string();
    let statements = statements.parse();
    assert!(!statements.is_err());
});
