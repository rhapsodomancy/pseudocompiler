use parser::Parse;
mod js_codegen;
mod lexer;
mod parser;
mod solver;

#[cfg(test)]
mod tests;

fn main() {
    let string = std::fs::read_to_string("main.pl").expect("failed to open file");
    let tokens = lexer::lex(string).unwrap();
    let parsed = parser::Statements::parse(&mut parser::ParseCursor::new(tokens)).unwrap();
    println!("{:#?}", parsed);
    let output = js_codegen::codegen(parsed);
    println!("{}", output);
}
