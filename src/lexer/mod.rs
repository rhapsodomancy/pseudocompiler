#[cfg(test)]
mod test;

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
        self.input = chars.as_str();
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
        cursor.eat_while(|x| x == '\n');
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
                                ' ' => indents_count += 1,
                                '\n' => {
                                    return Ok(Self {
                                        span: cursor.yield_span(start_loc),
                                        token: Token::Punctuation(Punctuation::OpenRoundBracket),
                                    });
                                }
                                _ => {
                                    break;
                                }
                            }
                        }
                    }
                    return Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Indentation(indents_count),
                    });
                }
                '(' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Punctuation(Punctuation::OpenRoundBracket),
                    })
                }
                ')' => {
                    cursor.eat_char();
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Punctuation(Punctuation::CloseRoundBracket),
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
                            panic!()
                        }
                    }
                    Ok(Self {
                        span: cursor.yield_span(start_loc),
                        token: Token::Operator(Operator::Plus),
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
                },
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
                        token: Token::Operator(Operator::CloseSquareBracket),
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
                        Some(token) => token != ' ',
                        None => false,
                    } {
                        identifier.push(cursor.peek().unwrap());
                        cursor.eat_char();
                    }
                    match identifier.as_str() {
                        "AND" => {
                            cursor.eat_n(3);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Operator(Operator::And),
                            })
                        }
                        "OR" => {
                            cursor.eat_n(3);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Operator(Operator::Or),
                            })
                        }
                        "NOT" => {
                            cursor.eat_n(3);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Operator(Operator::Not),
                            })
                        }
                        "for" => {
                            cursor.eat_n(3);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::For),
                            })
                        }
                        "function" => {
                            cursor.eat_n(8);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::Function),
                            })
                        }
                        "endfunction" => {
                            cursor.eat_n(11);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::EndFunction),
                            })
                        }
                        "return" => {
                            cursor.eat_n(6);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Operator(Operator::Return),
                            })
                        }
                        "to" => {
                            cursor.eat_n(2);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::To),
                            })
                        }
                        "do" => {
                            cursor.eat_n(2);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::Do),
                            })
                        }
                        "endfor" => {
                            cursor.eat_n(6);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::EndFor),
                            })
                        }
                        "while" => {
                            cursor.eat_n(5);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::While),
                            })
                        }
                        "endwhile" => {
                            cursor.eat_n(7);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::EndWhile),
                            })
                        }
                        "if" => {
                            cursor.eat_n(2);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::If),
                            })
                        }
                        "elseif" => {
                            cursor.eat_n(5);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::ElseIf),
                            })
                        }
                        "endif" => {
                            cursor.eat_n(5);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::EndIf),
                            })
                        }
                        "else" => {
                            cursor.eat_n(4);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Keyword(Keyword::Else),
                            })
                        }
                        "true" => {
                            cursor.eat_n(4);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Literal(Literal::Boolean(true)),
                            })
                        }
                        _ => {
                            cursor.eat_n(identifier.len() as u32);
                            Ok(Self {
                                span: cursor.yield_span(start_loc),
                                token: Token::Ident(identifier),
                            })
                        }
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
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    Equals,
    EqualsEquals,
    Return,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Punctuation {
    OpenRoundBracket,
    CloseRoundBracket,
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

impl Lex<'_> for Literal {
    fn lex(cursor: &'_ mut LexCursor) -> Result<Self, LexError> {
        if let Some(character) = cursor.peek() {
            match character {
                '"' => {
                    let loc = cursor.loc;
                    let mut output = String::new();
                    while match cursor.peek() {
                        Some(character) => character != '"',
                        None => return Err(LexError::UnexpectedEndOfInput(cursor.yield_span(loc))),
                    } {
                        output.push(cursor.eat_char().unwrap());
                    }
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
                        cursor.eat_n(output.len() as u32);
                        Ok(Self::Integer(output.parse::<i32>().unwrap()))
                    } else if output.parse::<f32>().is_ok() {
                        cursor.eat_n(output.len() as u32);
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
    while cursor.input.len() > 0 {
        spanned_tokens.push(SpannedToken::lex(&mut cursor)?);
    }
    Ok(spanned_tokens)
}
