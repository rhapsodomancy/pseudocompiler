use parser::Parse;
mod js_codegen;
mod lexer;
mod parser;
mod solver;

#[cfg(test)]
mod tests;

fn main() -> std::io::Result<()> {
    let string = std::fs::read_to_string("main.pl")?;
    let tokens = lexer::lex(string).unwrap();
    let parsed = parser::Statements::parse(&mut parser::ParseCursor::new(tokens)).unwrap();
    let output = js_codegen::codegen(parsed);
    println!("{}", output);
    Ok(())
}
