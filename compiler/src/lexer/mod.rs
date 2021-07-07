#[cfg(test)]
mod test;

use std::fmt::Display;

use thiserror::Error as ThisError;

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub struct Loc {
    pub line: u32,
    pub col: u32,
}

impl Default for Loc {
    fn default() -> Self {
        Self { line: 0, col: 0 }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: Loc,
    pub stop: Loc,
}

#[derive(ThisError, Debug, Eq, PartialEq)]
pub enum LexError {
    #[error("unexpected end of input")]
    UnexpectedEndOfInput(Span),
    #[error("invalid tokens")]
    InvalidTokens(String, Span),
}

pub struct LexCursor<'a> {
    input: &'a str,
    loc: Loc,
}

impl<'a> LexCursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            loc: Loc::default(),
        }
    }
    pub fn peek(&self) -> Option<char> {
        self.input.chars().next()
    }
    pub fn eat_n(&mut self, n: u32) {
        for _ in 0..n {
            self.eat_char();
        }
    }
    pub fn eat_char(&mut self) -> Option<char> {
        let mut chars = self.input.chars();
        let next = chars.next();
        self.input = self.input.get(1..).unwrap();
        next.map(|item| match item {
            '\n' => {
                self.loc.line += 1;
            }
            '\t' => {
                self.loc.col += 4;
            }
            _ => {
                self.loc.col += 1;
            }
        });
        next
    }
    pub fn eat_while(&mut self, predicate: impl Fn(char) -> bool) {
        loop {
            if let Some(next) = self.peek() {
                if predicate(next) {
                    self.eat_char();
                } else {
                    break;
                }
            }
        }
    }
    pub fn yield_span(&self, start: Loc) -> Span {
        Span {
            start,
            stop: self.loc,
        }
    }
}

trait Lex<'a>: Sized {
    fn lex(cursor: &'a mut LexCursor) -> Result<Self, LexError>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct SpannedToken {
    pub span: Span,
    pub token: Token,
}

impl<'a> Lex<'a> for SpannedToken {
    fn lex(cursor: &'a mut LexCursor) -> Result<Self, LexError> {
        let start_loc = cursor.loc;
        if let Some(item) = cursor.peek() {
            match item {
                '\n' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::NewLine,
                    })
                }
                ' ' => {
                    let mut indents_count = 1;
                    cursor.eat_char();
                    loop {
                        if let Some(item) = cursor.peek() {
                            match item {
                                ' ' => {
                                    cursor.eat_char();
                                    indents_count += 1;
                                }
                                '\n' => {
                                    return Ok(Self {
                                        span: cursor.yield_span(start_loc),
                                        token: Token::NewLine,
                                    });
                                }
                                _ => {
                                    break;
                                }
                            }
                        }
                    }
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Indentation(indents_count),
                    })
                }
                '(' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Operator(Operator::OpenRoundBracket),
                    })
                }
                ')' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Operator(Operator::CloseRoundBracket),
                    })
                }
                '[' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Operator(Operator::OpenSquareBracket),
                    })
                }
                ']' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Operator(Operator::CloseSquareBracket),
                    })
                }
                '/' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Operator(Operator::Divide),
                    })
                }
                '=' => {
                    cursor.eat_char();
                    if let Some(next) = cursor.peek() {
                        if next == '=' {
                            cursor.eat_char();
                            return Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Operator(Operator::EqualsEquals),
                            });
                        } else {
                            return Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Assignment,
                            });
                        }
                    }
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Assignment,
                    })
                }
                '+' => {
                    cursor.eat_char();
                    if let Some(next) = cursor.peek() {
                        if next == '=' {
                            cursor.eat_char();
                            return Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Operator(Operator::PlusEquals),
                            });
                        }
                    } else {
                        panic!()
                    }
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Operator(Operator::Plus),
                    })
                }
                '*' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Operator(Operator::Times),
                    })
                }
                '%' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Operator(Operator::Mod),
                    })
                }
                '-' => {
                    cursor.eat_char();
                    if let Some(next) = cursor.peek() {
                        if next == '=' {
                            cursor.eat_char();
                            return Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Operator(Operator::MinusEquals),
                            });
                        }
                    } else {
                        panic!()
                    }
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Operator(Operator::Minus),
                    })
                }
                ',' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Punctuation(Punctuation::Comma),
                    })
                }
                '"' | '0'..='9' => {
                    let literal = Literal::lex(cursor)?;
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Literal(literal),
                    })
                }
                _ => {
                    let mut identifier = String::new();
                    while match cursor.peek() {
                        Some(token) => token.is_alphanumeric(),
                        None => false,
                    } {
                        identifier.push(cursor.peek().unwrap());
                        cursor.eat_char();
                    }
                    match identifier.as_str() {
                        "AND" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Operator(Operator::And),
                        }),
                        "OR" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Operator(Operator::Or),
                        }),
                        "NOT" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Operator(Operator::Not),
                        }),
                        "for" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::For),
                        }),
                        "function" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::Function),
                        }),
                        "endfunction" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::EndFunction),
                        }),
                        "return" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Operator(Operator::Return),
                        }),
                        "to" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::To),
                        }),
                        "do" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::Do),
                        }),
                        "endfor" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::EndFor),
                        }),
                        "while" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::While),
                        }),
                        "endwhile" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::EndWhile),
                        }),
                        "if" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::If),
                        }),
                        "elseif" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::ElseIf),
                        }),
                        "endif" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::EndIf),
                        }),
                        "else" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Keyword(Keyword::Else),
                        }),
                        "true" => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Literal(Literal::Boolean(true)),
                        }),
                        _ => Ok(Self {
                            span: cursor.yield_span(start_loc),
                            token: Token::Ident(identifier),
                        }),
                    }
                }
            }
        } else {
            Err(LexError::UnexpectedEndOfInput(
                cursor.yield_span(cursor.loc),
            ))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Punctuation(Punctuation),
    Keyword(Keyword),
    Literal(Literal),
    /// The indentation of a line, containing the number of spaces.
    Indentation(u32),
    NewLine,
    Ident(String),
    Operator(Operator),
    /// Assignment is a statement, not an expression
    Assignment,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    And,
    Or,
    Not,
    OpenSquareBracket,
    CloseSquareBracket,
    Plus,
    PlusEquals,
    Minus,
    MinusEquals,
    Times,
    Divide,
    IntegerDivide,
    Mod,
    EqualsEquals,
    Return,
    OpenRoundBracket,
    CloseRoundBracket,
}

impl Display for Operator {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Punctuation {
    Comma,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Keyword {
    If,
    ElseIf,
    Else,
    EndIf,
    For,
    To,
    Do,
    EndFor,
    While,
    EndWhile,
    Function,
    EndFunction,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Boolean(bool),
    Integer(i32),
    Float(f32),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(string) => {
                f.write_str(&format!("\"{}\"", string))?;
            }
            Self::Boolean(x) => {
                f.write_str(if *x { "true" } else { "false" })?;
            }
            Self::Integer(int) => {
                int.fmt(f)?;
            }
            Self::Float(float) => {
                float.fmt(f)?;
            }
        }
        Ok(())
    }
}

impl Lex<'_> for Literal {
    fn lex(cursor: &'_ mut LexCursor) -> Result<Self, LexError> {
        if let Some(character) = cursor.peek() {
            match character {
                '"' => {
                    let _loc = cursor.loc;
                    cursor.eat_char();
                    let mut output = String::new();
                    while match cursor.peek() {
                        Some(character) => character != '"',
                        None => false,
                    } {
                        let chars = cursor.eat_char().unwrap();
                        output.push(chars);
                    }
                    cursor.eat_char();
                    Ok(Self::String(output))
                }
                '0'..='9' => {
                    let loc = cursor.loc;
                    let mut output = String::new();
                    while match cursor.peek() {
                        Some(character) => character.is_numeric() || character == '.',
                        None => false,
                    } {
                        output.push(cursor.eat_char().unwrap());
                    }
                    if output.parse::<i32>().is_ok() {
                        Ok(Self::Integer(output.parse::<i32>().unwrap()))
                    } else if output.parse::<f32>().is_ok() {
                        Ok(Self::Float(output.parse::<f32>().unwrap()))
                    } else {
                        Err(LexError::InvalidTokens(
                            String::from("This should be a valid integer or float, but it isn't"),
                            cursor.yield_span(loc),
                        ))
                    }
                }
                _ => panic!(),
            }
        } else {
            Err(LexError::UnexpectedEndOfInput(
                cursor.yield_span(cursor.loc),
            ))
        }
    }
}

pub fn lex(input: String) -> Result<Vec<SpannedToken>, LexError> {
    let mut cursor = LexCursor::new(&input);
    let mut spanned_tokens = vec![];
    while !cursor.input.is_empty() {
        spanned_tokens.push(SpannedToken::lex(&mut cursor)?);
    }
    Ok(spanned_tokens)
}
