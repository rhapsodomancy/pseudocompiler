use crate::lexer::{Keyword, Loc, Operator, Punctuation, Span, SpannedToken, Token};
use arbitrary::Arbitrary;
use thiserror::Error as ThisError;

#[cfg(test)]
mod test;

use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct ParseCursor {
    input: Vec<SpannedToken>,
    loc: Loc,
}

impl ParseCursor {
    pub fn len(&self) -> usize {
        self.input.len()
    }
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
    pub fn peek_n(&self, n: usize) -> Result<&[SpannedToken], ParseError> {
        self.input.get(0..n).map_or(
            Err(ParseError::UnexpectedEndOfInput(self.yield_span(self.loc))),
            |x| Ok(x),
        )
    }
    pub fn is_empty(&self) -> bool {
        self.input.len() == 0
    }
    pub fn next_where<F>(&mut self, f: F) -> Result<SpannedToken, ParseError>
    where
        F: Fn(&Token) -> bool,
    {
        while !f(&self.peek()?.token) {
            self.next()?;
        }
        self.next()
    }
    pub fn next_not_space(&mut self) -> Result<SpannedToken, ParseError> {
        self.next_where(|token| {
            if let Token::Indentation(_) = token {
                false
            } else {
                true
            }
        })
    }
    pub fn peek_where<F>(&mut self, f: F) -> Result<&SpannedToken, ParseError>
    where
        F: Fn(&Token) -> bool,
    {
        while !f(&self.peek()?.token) {
            self.next()?;
        }
        self.peek()
    }
    pub fn peek_not_space(&mut self) -> Result<&SpannedToken, ParseError> {
        self.peek_where(|token| {
            if let Token::Indentation(_) = token {
                false
            } else {
                true
            }
        })
    }
    pub fn eat_where<F>(&mut self, f: F)
    where
        F: Fn(&Token) -> bool,
    {
        while let Ok(i) = self.peek() {
            if f(&i.token) {
                if self.next().is_err() {
                    return;
                };
            } else {
                return;
            }
        }
    }
}

#[derive(ThisError, Debug)]
pub enum ParseError {
    #[error("Unexpected end of input")]
    UnexpectedEndOfInput(Span),
    #[error("Unexpected token")]
    UnexpectedToken(SpannedToken),
    #[error("expected indentation")]
    ExpectedIndentation(SpannedToken),
}

pub trait Parse: Sized {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError>;
}

#[derive(Arbitrary, Clone, Debug)]
pub struct Statements<IDENT = Ident, EXP = Expression<Ident>, PARAM = IDENT>(
    pub Vec<Statement<IDENT, EXP, PARAM>>,
)
where
    IDENT: Display;

impl Parse for Statements {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        let mut statements = vec![];
        while !cursor.is_empty() {
            statements.push(Statement::parse(cursor)?)
        }
        Ok(Self(statements))
    }
}

impl Display for Statements {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.0 {
            statement.fmt(f)?;
            f.write_str("\n")?;
        }
        Ok(())
    }
}

#[derive(Arbitrary, Clone, Debug)]
pub enum Statement<IDENT = Ident, EXP = Expression<IDENT>, PARAM = IDENT>
where
    IDENT: Display,
{
    ForStatement(ForStatement<IDENT, EXP, PARAM>),
    WhileStatement(WhileStatement<IDENT, EXP, PARAM>),
    FunctionDefinition(FunctionDefinition<IDENT, EXP, PARAM>),
    Expression(EXP),
    AssignmentStatement(AssignmentStatement<IDENT, EXP>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ForStatement(statement) => {
                statement.fmt(f)?;
            }
            Self::WhileStatement(statement) => statement.fmt(f)?,
            Self::Expression(exp) => exp.fmt(f)?,
            Self::AssignmentStatement(statement) => statement.fmt(f)?,
            Self::FunctionDefinition(def) => def.fmt(f)?,
        }
        Ok(())
    }
}

impl Parse for Statement {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        cursor.eat_where(|token| token == &Token::NewLine);
        let next = cursor.peek()?;
        match next.token {
            Token::Keyword(keyword) => match keyword {
                Keyword::For => Ok(Self::ForStatement(ForStatement::parse(cursor)?)),
                Keyword::While => Ok(Self::WhileStatement(WhileStatement::parse(cursor)?)),
                Keyword::Function => {
                    Ok(Self::FunctionDefinition(FunctionDefinition::parse(cursor)?))
                }
                _ => panic!("Unsupported operation."),
            },
            _ => {
                let (ident, possible_indentation) = if cursor.len() == 2 {
                    if let [ident, possible_indentation] = cursor.peek_n(2)? {
                        (ident, possible_indentation)
                    } else {
                        unreachable!()
                    }
                } else if let [ident, possible_indentation, equals] = cursor.peek_n(3)? {
                    let equals = if let Token::Indentation(_) = possible_indentation.token {
                        equals
                    } else {
                        possible_indentation
                    };
                    (ident, equals)
                } else {
                    unreachable!()
                };
                if let Token::Ident(_) = ident.token {
                    if possible_indentation.token == Token::Assignment {
                        return Ok(Self::AssignmentStatement(AssignmentStatement::parse(
                            cursor,
                        )?));
                    }
                }
                let mut expression_tokens = vec![];
                while match cursor.peek() {
                    Ok(peek) => peek.token != Token::NewLine,
                    Err(_) => false,
                } {
                    expression_tokens.push(cursor.next()?);
                }
                #[allow(unused_must_use)]
                {
                    cursor.next();
                }
                Ok(Self::Expression(Expression::parse(&mut ParseCursor::new(
                    expression_tokens,
                ))?))
            }
        }
    }
}

#[derive(Arbitrary, Clone, Debug)]
pub struct Block<IDENT = Ident, EXP = Expression<IDENT>, PARAM = IDENT>
where
    IDENT: Display,
{
    pub statements: Statements<IDENT, EXP, PARAM>,
    pub indentation: u32,
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements.0 {
            statement.fmt(f)?;
        }
        Ok(())
    }
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
                _ => 0,
            };
            if statement_indentation == indentation {
                cursor.next()?;
                statements.push(Statement::parse(cursor)?);
            } else if statement_indentation + 2 == indentation
                || statement_indentation + 4 == indentation
            {
                break;
            }
        }
        Ok(Self {
            statements: Statements(statements),
            indentation,
        })
    }
}

#[derive(Arbitrary, Clone, Debug)]
pub struct AssignmentStatement<IDENT = Ident, EXP = Expression<Ident>>
where
    IDENT: Display,
{
    pub ident: IDENT,
    pub equals_span: Span,
    pub expression: EXP,
}

impl Display for AssignmentStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.ident.fmt(f)?;
        f.write_str(" = ")?;
        self.expression.fmt(f)?;
        f.write_str("\n")
    }
}

impl Parse for AssignmentStatement {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        Ok(Self {
            ident: {
                let next = cursor.next_not_space()?;
                match next.token {
                    Token::Ident(ident) => SpannedItem {
                        item: ident,
                        span: next.span,
                    },
                    _ => return Err(ParseError::UnexpectedToken(next)),
                }
            },
            equals_span: {
                let next = cursor.next_not_space()?;
                match next.token {
                    Token::Assignment => next.span,
                    _ => return Err(ParseError::UnexpectedToken(next)),
                }
            },
            expression: {
                let mut expression_tokens = vec![];
                while {
                    cursor
                        .peek()
                        .map(|peek| match peek.token {
                            Token::Ident(_) => true,
                            Token::Operator(_) => true,
                            Token::Indentation(_) => true,
                            Token::Literal(_) => true,
                            _ => false,
                        })
                        .unwrap_or(false)
                } {
                    expression_tokens.push(cursor.next()?)
                }
                let res = Expression::parse(&mut ParseCursor::new(expression_tokens))?;
                res
            },
        })
    }
}

#[derive(Arbitrary, Clone, Debug, Eq)]
pub struct SpannedItem<T>
where
    T: Arbitrary,
{
    pub item: T,
    pub span: Span,
}

impl<T: PartialEq + Arbitrary> PartialEq for SpannedItem<T> {
    fn eq(&self, other: &Self) -> bool {
        self.item.eq(&other.item)
    }
}

impl<T> SpannedItem<T>
where
    T: Arbitrary,
{
    fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }
}

impl<T> Display for SpannedItem<T>
where
    T: Display + Arbitrary,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.item.fmt(f)
    }
}

#[derive(Arbitrary, Clone, Debug)]
pub struct FunctionDefinition<IDENT = Ident, EXP = Expression<IDENT>, PARAM = IDENT>
where
    IDENT: Display,
{
    pub keyword_function: SpannedItem<Keyword>,
    pub function_name: IDENT,
    pub parameters: Vec<PARAM>,
    pub block: Block<IDENT, EXP, PARAM>,
    pub keyword_endfunction: SpannedItem<Keyword>,
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("fuhction ")?;
        self.function_name.fmt(f)?;
        f.write_str("(")?;
        for argument in &self.parameters {
            argument.fmt(f)?;
        }
        f.write_str(")")?;
        Ok(())
    }
}

impl Parse for FunctionDefinition {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        Ok(Self {
            keyword_function: parse_specific_keyword(cursor, Keyword::Function)?,
            function_name: {
                let next = cursor.next_not_space()?;
                match next.token {
                    Token::Ident(identifier) => Ident {
                        span: next.span,
                        item: identifier,
                    },
                    _ => return Err(ParseError::UnexpectedToken(next)),
                }
            },
            parameters: {
                parse_specific_operator(cursor, Operator::OpenRoundBracket)?;
                let mut arguments = vec![];
                loop {
                    if cursor.peek()?.token == Token::Operator(Operator::CloseRoundBracket) {
                        break;
                    }
                    let next = cursor.next_not_space()?;
                    match next.token {
                        Token::Ident(identifier) => {
                            arguments.push(SpannedItem::new(identifier, next.span))
                        }
                        _ => return Err(ParseError::UnexpectedToken(next)),
                    }
                    parse_specific_punctuation(cursor, Punctuation::Comma)?;
                }
                parse_specific_operator(cursor, Operator::CloseRoundBracket)?;
                parse_newline(cursor)?;
                arguments
            },
            block: Block::parse(cursor)?,
            keyword_endfunction: parse_specific_keyword(cursor, Keyword::EndFunction)?,
        })
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

#[derive(Arbitrary, Clone, Debug)]
pub struct WhileStatement<IDENT = Ident, EXP = Expression<IDENT>, PARAM = IDENT>
where
    IDENT: Display,
{
    pub keyword_while: SpannedItem<Keyword>,
    pub condition: EXP,
    pub keyword_do: SpannedItem<Keyword>,
    pub block: Block<IDENT, EXP, PARAM>,
    pub keyword_endwhile: SpannedItem<Keyword>,
}

impl Display for WhileStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("while ")?;
        self.condition.fmt(f)?;
        f.write_str(" do")?;
        f.write_str("\n")?;
        for statement in &self.block.statements.0 {
            // todo: fix indentation
            statement.fmt(f)?;
        }
        f.write_str("\n endwhile")
    }
}

impl Parse for WhileStatement {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        Ok(WhileStatement {
            keyword_while: parse_specific_keyword(cursor, Keyword::While)?,
            condition: {
                let mut expression_tokens = vec![];
                while cursor.peek()?.token != Token::Keyword(Keyword::Do) {
                    expression_tokens.push(cursor.next()?);
                }
                Expression::parse(&mut ParseCursor::new(expression_tokens))?
            },
            keyword_do: {
                let keyword = parse_specific_keyword(cursor, Keyword::Do)?;
                parse_newline(cursor)?;
                keyword
            },
            block: Block::parse(cursor)?,
            keyword_endwhile: { parse_specific_keyword(cursor, Keyword::EndWhile)? },
        })
    }
}

fn parse_specific_keyword(
    cursor: &mut ParseCursor,
    comp_keyword: Keyword,
) -> Result<SpannedItem<Keyword>, ParseError> {
    let next_token = cursor.next_not_space()?;
    if let Token::Keyword(keyword) = next_token.token {
        if keyword == comp_keyword {
            Ok(SpannedItem::new(keyword, next_token.span))
        } else {
            Err(ParseError::UnexpectedToken(next_token))
        }
    } else {
        Err(ParseError::UnexpectedToken(next_token))
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
            Err(ParseError::UnexpectedToken(next_token))
        }
    } else {
        Err(ParseError::UnexpectedToken(next_token))
    }
}

#[derive(Arbitrary, Clone, Debug)]
pub struct ForStatement<IDENT = Ident, EXP = Expression<IDENT>, PARAM = IDENT>
where
    IDENT: Display,
{
    pub keyword_for: SpannedItem<Keyword>,
    pub variable_of_iteration: IDENT,
    pub equals_operator: Span,
    pub start_expression: EXP,
    pub keyword_to: SpannedItem<Keyword>,
    pub stop_expression: EXP,
    pub keyword_do: SpannedItem<Keyword>,
    pub block: Block<IDENT, EXP, PARAM>,
    pub keyword_endfor: SpannedItem<Keyword>,
}

impl Display for ForStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("for")?;
        self.variable_of_iteration.fmt(f)?;
        f.write_str("=")?;
        self.start_expression.fmt(f)?;
        f.write_str("to")?;
        self.stop_expression.fmt(f)?;
        f.write_str("do")?;
        self.block.fmt(f)?;
        f.write_str("\n endfor")
    }
}

fn parse_newline(cursor: &mut ParseCursor) -> Result<SpannedToken, ParseError> {
    let next_token = cursor.next_not_space()?;
    if next_token.token == Token::NewLine {
        Ok(next_token)
    } else {
        Err(ParseError::UnexpectedToken(next_token))
    }
}

impl Parse for ForStatement {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        let keyword_for = parse_specific_keyword(cursor, Keyword::For)?;
        let variable_of_iteration = {
            let next = cursor.next_not_space()?;
            match next.token {
                Token::Ident(x) => Ident::new(x, next.span),
                _ => return Err(ParseError::UnexpectedToken(next)),
            }
        };
        let equals_operator = {
            let next_token = cursor.next()?;
            if let Token::Assignment = next_token.token {
                next_token.span
            } else {
                return Err(ParseError::UnexpectedToken(next_token));
            }
        };
        let mut expression_tokens = vec![];
        while cursor.peek()?.token != Token::Keyword(Keyword::To) {
            expression_tokens.push(cursor.next()?);
        }
        let start_expression = Expression::parse(&mut ParseCursor::new(expression_tokens))?;
        let keyword_to = parse_specific_keyword(cursor, Keyword::To)?;
        let mut expression_tokens = vec![];
        while cursor.peek()?.token != Token::Keyword(Keyword::Do) {
            expression_tokens.push(cursor.next()?);
        }
        let stop_expression = Expression::parse(&mut ParseCursor::new(expression_tokens))?;
        let keyword_do = parse_specific_keyword(cursor, Keyword::Do)?;
        parse_newline(cursor)?;
        let block = Block::parse(cursor)?;
        let keyword_endfor = parse_specific_keyword(cursor, Keyword::EndFor)?;
        Ok(Self {
            keyword_for,
            variable_of_iteration,
            equals_operator,
            start_expression,
            keyword_to,
            stop_expression,
            keyword_do,
            block,
            keyword_endfor,
        })
    }
}

pub type Ident = SpannedItem<String>;

/// Parses expressions
#[derive(Arbitrary, Clone, Debug, PartialEq)]
pub enum Expression<IDENT = Ident>
where
    IDENT: Display,
{
    Operator(SpannedItem<Operator>, Vec<Self>),
    FunctionCall(IDENT, Vec<Self>),
    Ident(IDENT),
    Literal(SpannedItem<crate::lexer::Literal>),
}

impl<IDENT> Display for Expression<IDENT>
where
    IDENT: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Operator(op, expressions) => match op.item {
                Operator::Times => {
                    expressions.get(0).unwrap().fmt(f)?;
                    f.write_str("*")?;
                    expressions.get(1).unwrap().fmt(f)?;
                }
                Operator::Plus => {
                    expressions.get(0).unwrap().fmt(f)?;
                    f.write_str("+")?;
                    expressions.get(1).unwrap().fmt(f)?;
                }
                Operator::Minus => {
                    expressions.get(0).unwrap().fmt(f)?;
                    f.write_str("-")?;
                    expressions.get(1).unwrap().fmt(f)?;
                }
                Operator::Divide => {
                    expressions.get(0).unwrap().fmt(f)?;
                    f.write_str("/")?;
                    expressions.get(1).unwrap().fmt(f)?;
                }
                _ => panic!("unsuported operator"),
            },
            Self::FunctionCall(ident, arguments) => {
                ident.fmt(f)?;
                f.write_str("(")?;
                for argument in arguments {
                    argument.fmt(f)?;
                    f.write_str(", ")?;
                }
                f.write_str(")")?;
            }
            Self::Ident(ident) => {
                ident.fmt(f)?;
            }
            Self::Literal(literal) => {
                literal.item.fmt(f)?;
            }
        }
        Ok(())
    }
}

impl Parse for Expression {
    fn parse(cursor: &mut ParseCursor) -> Result<Self, ParseError> {
        let res = parse_expression(cursor, 0);
        if res.is_ok() {}
        res
    }
}

/// A thank-you goes to Matklad for his blog post on this topic which provided much of the
/// inspiration for this function.
fn parse_expression(cursor: &mut ParseCursor, min_bp: u8) -> Result<Expression, ParseError> {
    let token = cursor.next_not_space()?;
    let mut lhs = match token.token {
        Token::Ident(x) => {
            // ok this is a bit messy, but it essentially parses function calls
            if let Ok(next) = cursor.peek_not_space() {
                match next.token {
                    Token::Operator(crate::lexer::Operator::OpenRoundBracket) => {
                        cursor.next_not_space()?;
                        let mut arguments = vec![];
                        loop {
                            if cursor.peek_not_space()?.token
                                == Token::Operator(crate::lexer::Operator::CloseRoundBracket)
                            {
                                break;
                            };
                            let mut expression_tokens = vec![];
                            while cursor.peek()?.token != Token::Punctuation(Punctuation::Comma)
                                && cursor.peek()?.token
                                    != Token::Operator(Operator::CloseRoundBracket)
                            {
                                expression_tokens.push(cursor.next()?);
                            }
                            arguments
                                .push(Expression::parse(&mut ParseCursor::new(expression_tokens))?);
                            if cursor.peek_not_space()?.token
                                == Token::Operator(crate::lexer::Operator::CloseRoundBracket)
                            {
                                break;
                            } else {
                                let should_be_comma = cursor.next_not_space()?;
                                if should_be_comma.token
                                    != Token::Punctuation(crate::lexer::Punctuation::Comma)
                                {
                                    return Err(ParseError::UnexpectedToken(should_be_comma));
                                }
                            }
                        }
                        let should_be_bracket = cursor.peek()?;
                        if should_be_bracket.token
                            != Token::Operator(crate::lexer::Operator::CloseRoundBracket)
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
                let result = Expression::Ident(Ident {
                    span: token.span,
                    item: x,
                });
                result
            }
        }
        Token::Operator(Operator::Return) => {
            let rhs = parse_expression(cursor, min_bp)?;
            Expression::Operator(
                SpannedItem {
                    span: token.span,
                    item: Operator::Return,
                },
                vec![rhs],
            )
        }
        Token::Operator(Operator::Not) => {
            let rhs = parse_expression(cursor, min_bp)?;
            Expression::Operator(
                SpannedItem {
                    span: token.span,
                    item: Operator::Not,
                },
                vec![rhs],
            )
        }
        Token::Literal(x) => Expression::Literal(SpannedItem {
            span: token.span,
            item: x,
        }),
        Token::Operator(Operator::OpenRoundBracket) => {
            let lhs = parse_expression(cursor, 0)?;
            let next = cursor.next_not_space()?;
            if next.token != Token::Operator(Operator::CloseRoundBracket) {
                return Err(ParseError::UnexpectedToken(next));
            }
            lhs
        }
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
        let peek = match cursor.peek_where(|token| !matches!(token, Token::Indentation(_))) {
            Ok(t) => t.clone(),
            Err(_) => break,
        };
        let op = match &peek.token {
            Token::Operator(op) => op,
            _ => return Err(ParseError::UnexpectedToken(peek)),
        };
        if let Some((l_bp, ())) = postfix_binding_power(&op) {
            if l_bp < min_bp {
                break;
            }
            cursor.next_not_space()?;
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
            cursor.next_not_space()?;
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
        _ => {
            panic!()
        }
    }
}

fn postfix_binding_power(operator: &Operator) -> Option<(u8, ())> {
    Some(match operator {
        Operator::Not => (11, ()),
        Operator::Return => (11, ()),
        Operator::OpenSquareBracket => (11, ()),
        _ => return None,
    })
}

fn infix_binding_power(op: &Operator) -> Option<(u8, u8)> {
    Some(match op {
        Operator::EqualsEquals => (2, 1),
        Operator::Plus | Operator::Minus => (5, 6),
        Operator::Times
        | Operator::Divide
        | Operator::IntegerDivide
        | Operator::And
        | Operator::Or => (7, 8),
        _ => return None,
    })
}
