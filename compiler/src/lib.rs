use parser::Parse;

pub mod js_codegen;
pub mod lexer;
pub mod parser;

pub fn compile(input: String) -> String {
    let tokens = lexer::lex(input).unwrap();
    let parsed = parser::Statements::parse(&mut parser::ParseCursor::new(tokens)).unwrap();
    let output = js_codegen::codegen(parsed);
    output
}
