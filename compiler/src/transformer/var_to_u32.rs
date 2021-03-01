//! Adds unique identifiers to all variables.

use std::{collections::HashMap, fmt::Display, hash::Hash};

use crate::{
    lexer::Operator,
    parser::{
        AssignmentStatement, Block, Expression, ForStatement, FunctionDefinition, Ident,
        SpannedItem, Statement, Statements, WhileStatement,
    },
};

use super::Transformer;

#[derive(Debug, Clone, Eq)]
pub struct Var {
    pub id: u32,
    pub ident: Ident,
}

impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl Hash for Var {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl Display for Var {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unimplemented!()
    }
}

pub type VarStatements = Statements<Var, VarExpression>;

pub type VarStatement = Statement<Var, VarExpression>;

pub type VarFormStatement = ForStatement<Var, VarExpression>;

#[derive(Clone, Debug, PartialEq)]
pub struct VarExpression(pub u32, pub VarExpressionItem);

#[derive(Clone, Debug, PartialEq)]
pub enum VarExpressionItem<IDENT = Var>
where
    IDENT: Display,
{
    Operator(SpannedItem<Operator>, Vec<VarExpression>),
    FunctionCall(IDENT, Vec<VarExpression>),
    Ident(IDENT),
    Literal(SpannedItem<crate::lexer::Literal>),
}

pub type VarWhileStatement = WhileStatement<Var, VarExpression>;

pub type VarBlock = Block<Var, VarExpression>;

pub type VarFunctionDefinition = FunctionDefinition<Var, VarExpression, Var>;

pub type VarAssignmentStatement = AssignmentStatement<Var, VarExpression>;

#[derive(Default, Debug)]
pub struct VarStatementsTransformer {
    counter: u32,
    ctx: HashMap<String, u32>,
}

impl VarStatementsTransformer {
    fn new_var(&mut self) -> u32 {
        let r = self.counter;
        self.counter += 1;
        r
    }
    fn walk_statements(&mut self, stmt: Statements) -> VarStatements {
        let mut output = vec![];
        for statement in stmt.0 {
            output.push(self.walk_statement(statement))
        }
        Statements(output)
    }
    fn walk_statement(&mut self, stmt: Statement) -> VarStatement {
        match stmt {
            Statement::ForStatement(stmt) => {
                VarStatement::ForStatement(self.walk_for_statement(stmt))
            }
            Statement::WhileStatement(stmt) => {
                VarStatement::WhileStatement(self.walk_while_statement(stmt))
            }
            Statement::FunctionDefinition(def) => {
                VarStatement::FunctionDefinition(self.walk_function_definition(def))
            }
            Statement::Expression(exp) => VarStatement::Expression(self.walk_expression(exp)),
            Statement::AssignmentStatement(stmt) => {
                VarStatement::AssignmentStatement(self.walk_assignment(stmt))
            }
        }
    }
    fn walk_assignment(&mut self, stmt: AssignmentStatement) -> VarAssignmentStatement {
        VarAssignmentStatement {
            ident: {
                let id = if let Some(id) = self.ctx.get(&stmt.ident.item) {
                    *id
                } else {
                    self.new_var()
                };
                self.ctx.insert(stmt.ident.item.clone(), id);
                Var {
                    id,
                    ident: stmt.ident,
                }
            },
            equals_span: stmt.equals_span,
            expression: self.walk_expression(stmt.expression),
        }
    }
    fn walk_function_definition(&mut self, def: FunctionDefinition) -> VarFunctionDefinition {
        VarFunctionDefinition {
            keyword_function: def.keyword_function,
            function_name: {
                let var = self.new_var();
                self.ctx.insert(def.function_name.item.clone(), var);
                Var {
                    id: var,
                    ident: def.function_name,
                }
            },
            parameters: {
                def.parameters
                    .iter()
                    .map(|ident| {
                        let var = self.new_var();
                        self.ctx.insert(ident.item.clone(), var);
                        Var {
                            id: var,
                            ident: ident.clone(),
                        }
                    })
                    .collect()
            },
            block: {
                let res = self.walk_block(def.block);
                for ident in def.parameters {
                    self.ctx.remove(&ident.item);
                }
                res
            },
            keyword_endfunction: def.keyword_endfunction,
        }
    }
    fn walk_while_statement(&mut self, stmt: WhileStatement) -> VarWhileStatement {
        VarWhileStatement {
            keyword_while: stmt.keyword_while,
            condition: self.walk_expression(stmt.condition),
            keyword_do: stmt.keyword_do,
            block: self.walk_block(stmt.block),
            keyword_endwhile: stmt.keyword_endwhile,
        }
    }
    fn walk_block(&mut self, block: Block) -> VarBlock {
        VarBlock {
            statements: self.walk_statements(block.statements),
            indentation: block.indentation,
        }
    }
    fn walk_for_statement(&mut self, stmt: ForStatement) -> VarFormStatement {
        VarFormStatement {
            keyword_for: stmt.keyword_for,
            variable_of_iteration: {
                let id = self.new_var();
                self.ctx.insert(stmt.variable_of_iteration.item.clone(), id);
                Var {
                    id,
                    ident: stmt.variable_of_iteration,
                }
            },
            equals_operator: stmt.equals_operator,
            start_expression: self.walk_expression(stmt.start_expression),
            keyword_to: stmt.keyword_to,
            stop_expression: self.walk_expression(stmt.stop_expression),
            keyword_do: stmt.keyword_do,
            block: self.walk_block(stmt.block),
            keyword_endfor: stmt.keyword_endfor,
        }
    }
    fn walk_expression(&mut self, exp: Expression) -> VarExpression {
        match exp {
            Expression::Operator(op, args) => VarExpression(
                self.new_var(),
                VarExpressionItem::Operator(
                    op,
                    args.into_iter()
                        .map(|exp| self.walk_expression(exp))
                        .collect(),
                ),
            ),
            Expression::FunctionCall(func_name, params) => {
                let id = if let Some(func_name) = self.ctx.get(&func_name.item) {
                    *func_name
                } else {
                    // if this happens it will be caught during type inference / checking
                    self.new_var()
                };

                VarExpression(
                    self.new_var(),
                    VarExpressionItem::FunctionCall(
                        Var {
                            id,
                            ident: func_name,
                        },
                        params
                            .into_iter()
                            .map(|exp| self.walk_expression(exp))
                            .collect(),
                    ),
                )
            }
            Expression::Ident(ident) => {
                let id = if let Some(id) = self.ctx.get(&ident.item) {
                    *id
                } else {
                    self.new_var()
                };
                VarExpression(self.new_var(), VarExpressionItem::Ident(Var { id, ident }))
            }
            Expression::Literal(lit) => {
                VarExpression(self.new_var(), VarExpressionItem::Literal(lit))
            }
        }
    }
}

impl Transformer<Statements, VarStatements> for VarStatementsTransformer {
    fn transform(&mut self, from: Statements) -> VarStatements {
        self.walk_statements(from)
    }
}
