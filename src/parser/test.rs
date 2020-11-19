use libfuzzer_sys;

/// Tests that all valid targets test and fail.
libfuzzer_sys::fuzz_target!(|statements: Statements| {
    let program = statements.to_string();
    let statements = statements.parse();
    assert!(!statements.is_err());
});

