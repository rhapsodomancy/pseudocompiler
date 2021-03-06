use crate::parser::{
    Block, Expression, ForStatement, FunctionDefinition, Statement, Ast, WhileStatement,
};

use thiserror::Error as ThisError;

#[derive(ThisError, Debug)]
pub enum JSCodegenError {
    // not actually sure what will go here yet
}

pub struct Buffer {
    output_string: String,
}

impl Buffer {
    fn write<T>(&mut self, t: T)
    where
        T: AsRef<str>,
    {
        self.output_string += t.as_ref();
    }
}

pub trait OutputBuffer {
    fn output(&self, b: &mut Buffer);
}

impl OutputBuffer for Ast {
    fn output(&self, b: &mut Buffer) {
        b.write(include_str!("../../../js_stdlib/lib.js"));
        for statement in &self.0 {
            statement.output(b);
        }
        b.write("log");
    }
}

impl OutputBuffer for Statement {
    fn output(&self, b: &mut Buffer) {
        match self {
            Statement::ForStatement(item) => {
                item.output(b);
            }
            Statement::WhileStatement(item) => {
                item.output(b);
            }
            Statement::Expression(exp) => {
                exp.output(b);
            }
            _ => panic!("unsupported operation"),
        }
    }
}

impl OutputBuffer for FunctionDefinition {
    fn output(&self, b: &mut Buffer) {
        b.write("function ");
        b.write(&self.function_name.item);
        b.write("(");
        for argument in &self.parameters {
            b.write(&argument.item);
            b.write(",");
        }
        b.write(")");
        b.write("{");
        self.block.output(b);
        b.write("}");
    }
}

impl OutputBuffer for WhileStatement {
    fn output(&self, b: &mut Buffer) {
        b.write("while (");
        self.condition.output(b);
        b.write(") {");
        self.block.output(b);
        b.write("}");
    }
}

impl OutputBuffer for ForStatement {
    fn output(&self, b: &mut Buffer) {
        b.write("var ");
        b.write(&self.variable_of_iteration.item);
        b.write(";");
        b.write("for (");
        b.write(&self.variable_of_iteration.item);
        b.write("=(");
        self.start_expression.output(b);
        b.write(");");
        b.write(&self.variable_of_iteration.item);
        b.write("<(");
        self.stop_expression.output(b);
        b.write(");");
        b.write(&format!("{}++", &self.variable_of_iteration.item));
        b.write(")");
        b.write("{");
        self.block.output(b);
        b.write("};");
    }
}

impl OutputBuffer for Block {
    fn output(&self, b: &mut Buffer) {
        for statement in &self.statements.0 {
            statement.output(b);
            b.write(";");
        }
    }
}

fn write_binary_operator(b: &mut Buffer, arguments: &Vec<Expression>, operator: &str) {
    b.write("(");
    b.write("(");
    arguments.get(0).unwrap().output(b);
    b.write(")");
    b.write(operator);
    b.write("(");
    arguments.get(1).unwrap().output(b);
    b.write(")");
    b.write(")");
}

fn write_unary_operator(b: &mut Buffer, arguments: &Vec<Expression>, operator: &str) {
    b.write(operator);
    b.write("(");
    arguments.get(0).unwrap().output(b);
    b.write(")");
}

impl OutputBuffer for Expression {
    fn output(&self, b: &mut Buffer) {
        match self {
            Expression::Operator(operator, arguments) => match operator.item {
                crate::lexer::Operator::And => {
                    write_binary_operator(b, arguments, "&&");
                }
                crate::lexer::Operator::Or => {
                    write_binary_operator(b, arguments, "||");
                }
                crate::lexer::Operator::Not => {
                    write_unary_operator(b, arguments, "!");
                }
                crate::lexer::Operator::OpenSquareBracket => {}
                crate::lexer::Operator::CloseSquareBracket => {}
                crate::lexer::Operator::Plus => {
                    write_binary_operator(b, arguments, "+");
                }
                crate::lexer::Operator::PlusEquals => {}
                crate::lexer::Operator::Minus => {
                    write_binary_operator(b, arguments, "-");
                }
                crate::lexer::Operator::MinusEquals => {}
                crate::lexer::Operator::Times => {
                    write_binary_operator(b, arguments, "*");
                }
                crate::lexer::Operator::Divide => {
                    write_binary_operator(b, arguments, "/");
                }
                crate::lexer::Operator::IntegerDivide => {
                    write_binary_operator(b, arguments, "//");
                }
                crate::lexer::Operator::Mod => {
                    write_binary_operator(b, arguments, "%");
                }
                crate::lexer::Operator::EqualsEquals => {
                    write_binary_operator(b, arguments, "==");
                }
                crate::lexer::Operator::Return => {
                    write_unary_operator(b, arguments, "return");
                }
                _ => panic!("invalid"),
            },
            Expression::FunctionCall(function, arguments) => {
                b.write(&function.item);
                b.write("(");
                for argument in arguments {
                    b.write("(");
                    argument.output(b);
                    b.write(")");
                }
                b.write(");");
            }
            Expression::Ident(identifier) => {
                b.write(&identifier.item);
            }
            Expression::Literal(literal) => match &literal.item {
                crate::lexer::Literal::String(string) => {
                    b.write("\"");
                    b.write(string);
                    b.write("\"");
                }
                crate::lexer::Literal::Boolean(boolean) => {
                    b.write(if *boolean { "true" } else { "false" });
                }
                // let us pray that Rust numbers work out as Javascript numbers
                crate::lexer::Literal::Integer(integer) => {
                    b.write(&integer.to_string());
                }
                crate::lexer::Literal::Float(float) => {
                    b.write(&float.to_string());
                }
            },
        }
    }
}

pub fn codegen(input: Ast) -> String {
    let mut buffer = Buffer {
        output_string: "".to_string(),
    };
    Ast::output(&input, &mut buffer);
    buffer.output_string
}

#[cfg(test)]
mod test {
    #[test]
    fn test_simple_for_loop_output() {}
    #[test]
    fn test_simple_while_loop_output() {}
    #[test]
    fn test_assignment_statement_output() {}
    #[test]
    fn test_single_clause_if() {}
    #[test]
    fn test_multiple_clause_if() {}
    #[test]
    fn test_function_definition() {}
}
