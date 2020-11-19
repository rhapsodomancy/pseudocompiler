use crate::lexer::{Keyword, Loc, Operator, Punctuation, Span, SpannedToken, Token};
use thiserror::Error as ThisError;

pub struct ParseCursor {
    input: Vec<SpannedToken>,
    loc: Loc,
}

impl ParseCursor {
    pub fn new(input: Vec<SpannedToken>) -> Self {
        Self {
            input,
            loc: Loc { line: 0, col: 0 },
        }
    }
    pub fn yield_span(&self, prev_loc: Loc) -> Span {
        Span {
            start: prev_loc,
            stop: self.loc,
        }
    }
    pub fn next(&mut self) -> Result<SpannedToken, ParseError> {
        if self.input.get(0).is_some() {
            Ok(self.input.remove(0))
        } else {
            Err(ParseError::UnexpectedEndOfInput(self.yield_span(self.loc)))
        }
    }
    pub fn peek(&self) -> Result<&SpannedToken, ParseError> {
        if let Some(next) = self.input.get(0) {
            Ok(next)
        } else {
            Err(ParseError::UnexpectedEndOfInput(self.yield_span(self.loc)))
        }
    }
    pub fn is_empty(&self) -> bool {
        self.input.len() > 0
    }
}

#[derive(ThisError, Debug)]
pub enum ParseError {
    #[error("Unexpected end of input")]
    UnexpectedEndOfInput(Span),
    #[error("Unexpected token")]
    UnexpectedToken(SpannedToken),
    #[error("")]
    ExpectedIndentation(SpannedToken),
}

pub trait Parse: Sized {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError>;
}

pub struct Statements(pub Vec<Statement>);

impl Parse for Statements {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        let statements = vec![];
        while !cursor.is_empty() {}
        Ok(Self(statements))
    }
}

pub enum Statement {
    ForStatement(ForStatement),
}

impl Parse for Statement {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        let next = cursor.peek()?;
        match next.token {
            Token::Keyword(keyword) => match keyword {
                Keyword::For => Ok(Self::ForStatement(ForStatement::parse(cursor)?)),
                _ => panic!("Unsupported operation."),
            },
            _ => panic!("x"),
        }
    }
}

pub struct Block {
    pub statements: Vec<Statement>,
}

impl Parse for Block {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        let indentation_token = cursor.peek()?;
        let indentation = match indentation_token.token {
            Token::Indentation(x) => x,
            _ => return Err(ParseError::ExpectedIndentation(indentation_token.clone())),
        };
        let mut statements = vec![];
        loop {
            let statement_indentation_token = cursor.peek()?;
            let statement_indentation = match statement_indentation_token.token {
                Token::Indentation(x) => x,
                _ => {
                    return Err(ParseError::ExpectedIndentation(
                        statement_indentation_token.clone(),
                    ))
                }
            };
            if statement_indentation == indentation {
                cursor.next()?;
                statements.push(Statement::parse(cursor)?);
            } else if statement_indentation - 2 == indentation
                || statement_indentation - 4 == indentation
            {
                break;
            }
        }
        Ok(Self { statements })
    }
}

pub struct SpannedItem<T> {
    pub item: T,
    pub span: Span,
}

impl<T> SpannedItem<T> {
    fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }
}

pub struct FunctionDefinition {
    pub keyword_function: SpannedItem<Keyword>,
    pub function_name: Ident,
    pub arguments: Vec<Ident>,
    pub block: Block,
    pub keyword_endfunction: SpannedItem<Keyword>,
}

impl Parse for FunctionDefinition {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        Ok(Self {
            keyword_function: parse_specific_keyword(cursor, Keyword::Function)?,
            function_name: {
                let next = cursor.next()?;
                match next.token {
                    Token::Ident(identifier) => Ident {
                        span: next.span,
                        item: identifier,
                    },
                    _ => return Err(ParseError::UnexpectedToken(next)),
                }
            },
            arguments: {
                let mut arguments = vec![];
                loop {
                    if cursor.peek()?.token == Token::Punctuation(Punctuation::CloseRoundBracket) {
                        break;
                    }
                    let next = cursor.next()?;
                    match next.token {
                        Token::Ident(identifier) => {
                            arguments.push(SpannedItem::new(identifier, next.span))
                        }
                        _ => return Err(ParseError::UnexpectedToken(next)),
                    }
                    parse_specific_punctuation(cursor, Punctuation::Comma)?;
                }
                arguments
            },
            block: Block::parse(cursor)?,
            keyword_endfunction: parse_specific_keyword(cursor, Keyword::EndFunction)?,
        })
    }
}

pub struct WhileStatement {
    pub keyword_while: SpannedItem<Keyword>,
    pub condition: Expression,
    pub keyword_do: SpannedItem<Keyword>,
    pub block: Block,
    pub keyword_endwhile: SpannedItem<Keyword>,
}

impl Parse for WhileStatement {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        Ok(WhileStatement {
            keyword_while: parse_specific_keyword(cursor, Keyword::While)?,
            condition: Expression::parse(cursor)?,
            keyword_do: parse_specific_keyword(cursor, Keyword::Do)?,
            block: Block::parse(cursor)?,
            keyword_endwhile: parse_specific_keyword(cursor, Keyword::While)?,
        })
    }
}

fn parse_specific_keyword(
    cursor: &mut ParseCursor,
    comp_keyword: Keyword,
) -> Result<SpannedItem<Keyword>, ParseError> {
    let next_token = cursor.next()?;
    if let Token::Keyword(keyword) = next_token.token {
        if keyword == comp_keyword {
            Ok(SpannedItem::new(keyword, next_token.span))
        } else {
            return Err(ParseError::UnexpectedToken(next_token));
        }
    } else {
        return Err(ParseError::UnexpectedToken(next_token));
    }
}

fn parse_specific_operator(
    cursor: &mut ParseCursor,
    comp_operator: Operator,
) -> Result<SpannedItem<Operator>, ParseError> {
    let next_token = cursor.next()?;
    if let Token::Operator(operator) = next_token.token {
        if operator == comp_operator {
            Ok(SpannedItem::new(operator, next_token.span))
        } else {
            return Err(ParseError::UnexpectedToken(next_token));
        }
    } else {
        return Err(ParseError::UnexpectedToken(next_token));
    }
}

fn parse_specific_punctuation(
    cursor: &mut ParseCursor,
    comp_punctuation: Punctuation,
) -> Result<SpannedItem<Punctuation>, ParseError> {
    let next_token = cursor.next()?;
    if let Token::Punctuation(punctuation) = next_token.token {
        if punctuation == comp_punctuation {
            Ok(SpannedItem::new(punctuation, next_token.span))
        } else {
            return Err(ParseError::UnexpectedToken(next_token));
        }
    } else {
        return Err(ParseError::UnexpectedToken(next_token));
    }
}

pub struct ForStatement {
    pub keyword_for: SpannedItem<Keyword>,
    pub variable_of_iteration: Ident,
    pub equals_operator: SpannedItem<Operator>,
    pub start_expression: Expression,
    pub keyword_to: SpannedItem<Keyword>,
    pub stop_expression: Expression,
    pub keyword_do: SpannedItem<Keyword>,
    pub block: Block,
    pub keyword_endfor: SpannedItem<Keyword>,
}

impl Parse for ForStatement {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        Ok(Self {
            keyword_for: parse_specific_keyword(cursor, Keyword::For)?,
            variable_of_iteration: {
                let next = cursor.next()?;
                match next.token {
                    Token::Ident(x) => Ident::new(x, next.span),
                    _ => return Err(ParseError::UnexpectedToken(next)),
                }
            },
            equals_operator: parse_specific_operator(cursor, Operator::Equals)?,
            start_expression: Expression::parse(cursor)?,
            keyword_to: parse_specific_keyword(cursor, Keyword::To)?,
            stop_expression: Expression::parse(cursor)?,
            keyword_do: parse_specific_keyword(cursor, Keyword::Do)?,
            block: Block::parse(cursor)?,
            keyword_endfor: parse_specific_keyword(cursor, Keyword::EndFor)?,
        })
    }
}

type Ident = SpannedItem<String>;
/// Parses expressions
pub enum Expression {
    Operator(SpannedItem<Operator>, Vec<Expression>),
    FunctionCall(Ident, Vec<Expression>),
    Ident(Ident),
    Literal(SpannedItem<crate::lexer::Literal>),
}

impl Parse for Expression {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        parse_expression(cursor, 0)
    }
}

/// A thank-you goes to Matklad for his blog post on this topic which provided much of the
/// inspiration for this function.
fn parse_expression(cursor: &mut ParseCursor, min_bp: u8) -> Result<Expression, ParseError> {
    let token = cursor.next()?;
    let mut lhs = match token.token {
        Token::Ident(x) => {
            // ok this is a bit messy, but it essentially parses function calls
            if let Ok(next) = cursor.peek() {
                match next.token {
                    Token::Punctuation(crate::lexer::Punctuation::OpenRoundBracket) => {
                        let mut arguments = vec![];
                        cursor.next()?;
                        loop {
                            if cursor.peek()?.token
                                == Token::Punctuation(crate::lexer::Punctuation::CloseRoundBracket)
                            {
                                break;
                            };
                            arguments.push(parse_expression(cursor, 0)?);
                            let should_be_comma = cursor.peek()?;
                            if should_be_comma.token
                                != Token::Punctuation(crate::lexer::Punctuation::Comma)
                            {
                                return Err(ParseError::UnexpectedToken(should_be_comma.clone()));
                            }
                        }
                        let should_be_bracket = cursor.peek()?;
                        if should_be_bracket.token
                            != Token::Punctuation(crate::lexer::Punctuation::CloseRoundBracket)
                        {
                            return Err(ParseError::UnexpectedToken(should_be_bracket.clone()));
                        }
                        Expression::FunctionCall(
                            Ident {
                                span: token.span,
                                item: x,
                            },
                            arguments,
                        )
                    }
                    _ => Expression::Ident(Ident {
                        span: token.span,
                        item: x,
                    }),
                }
            } else {
                Expression::Ident(Ident {
                    span: token.span,
                    item: x,
                })
            }
        }
        Token::Literal(x) => Expression::Literal(SpannedItem {
            span: token.span,
            item: x,
        }),
        Token::Punctuation(x) => match x {
            crate::lexer::Punctuation::OpenRoundBracket => {
                let lhs = parse_expression(cursor, 0)?;
                assert_eq!(
                    cursor.next()?.token,
                    Token::Punctuation(crate::lexer::Punctuation::CloseRoundBracket)
                );
                lhs
            }
            _ => panic!("invalid punctuation"),
        },
        Token::Operator(op) => {
            let ((), r_pb) = prefix_binding_power(&op);
            let rhs = parse_expression(cursor, r_pb)?;
            Expression::Operator(
                SpannedItem {
                    span: token.span,
                    item: op,
                },
                vec![rhs],
            )
        }
        _ => panic!("bad token"),
    };
    loop {
        let peek = cursor.peek()?.clone();
        let op = match &peek.token {
            Token::Operator(op) => op,
            _ => panic!(),
        };
        if let Some((l_bp, ())) = postfix_binding_power(&op) {
            if l_bp < min_bp {
                break;
            }
            cursor.next()?;
            lhs = if *op == Operator::OpenSquareBracket {
                let rhs = parse_expression(cursor, 0)?;
                assert_eq!(
                    cursor.next()?.token,
                    Token::Operator(Operator::CloseSquareBracket)
                );
                Expression::Operator(
                    SpannedItem {
                        span: peek.span,
                        item: *op,
                    },
                    vec![lhs, rhs],
                )
            } else {
                Expression::Operator(
                    SpannedItem {
                        span: peek.span,
                        item: *op,
                    },
                    vec![lhs],
                )
            };
            continue;
        }
        if let Some((l_bp, r_bp)) = infix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }
            cursor.next()?;
            lhs = {
                let rhs = parse_expression(cursor, r_bp)?;
                Expression::Operator(
                    SpannedItem {
                        item: *op,
                        span: peek.span,
                    },
                    vec![lhs, rhs],
                )
            }
        }

        break;
    }
    Ok(lhs)
}

fn prefix_binding_power(operator: &Operator) -> ((), u8) {
    match operator {
        Operator::Plus | Operator::Minus => ((), 9),
        _ => panic!(),
    }
}

fn postfix_binding_power(operator: &Operator) -> Option<(u8, ())> {
    Some(match operator {
        Operator::Not => (11, ()),
        Operator::Return => (11, ()),
        _ => return None,
    })
}

fn infix_binding_power(op: &Operator) -> Option<(u8, u8)> {
    Some(match op {
        Operator::Equals => (2, 1),
        Operator::Plus | Operator::Minus => (5, 6),
        Operator::Times | Operator::Divide | Operator::IntegerDivide => (7, 8),
        _ => return None,
    })
}
