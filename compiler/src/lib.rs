use infer::ty_infer;
use parser::Parse;
use transformer::{var_to_u32::VarStatementsTransformer, Transformer};

pub mod infer;
pub mod js_codegen;
pub mod lexer;
pub mod parser;
pub mod transformer;
pub mod utils;

pub fn compile(input: String) -> String {
    let tokens = lexer::lex(input).unwrap();
    let parsed = parser::Statements::parse(&mut parser::ParseCursor::new(tokens)).unwrap();
    let transformed = VarStatementsTransformer::default().transform(parsed.clone());
    if ty_infer(transformed).is_err() {
        panic!("type error");
    }
    js_codegen::codegen(parsed)
}
