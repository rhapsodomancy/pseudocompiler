use std::collections::{HashMap, HashSet};

use either::Either;

use crate::{
    lexer::{Literal, Operator},
    parser::{Expression, SpannedItem, Statement},
    transformer::var_to_u32::{
        Var, VarAssignmentStatement, VarBlock, VarExpression, VarFormStatement,
        VarFunctionDefinition, VarStatement, VarStatements, VarWhileStatement,
    },
};

#[cfg(test)]
mod test;

pub struct TyVar(u32);

pub struct VarVar(u32);

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum Ty {
    Bool,
    Float,
    String,
    Integer,
    Any,
}

#[derive(Debug, Clone)]
pub enum VarConstraint {
    /// variable must have the same type as a builtin
    EqBuiltin(Ty),
    /// variable must have the same type as another variable or expression
    EqNonBuiltin(u32),
}

#[derive(Debug, Clone, Default)]
pub struct ConstraintGatherer {
    /// maps variables to constraints
    ctx: HashMap<u32, Vec<VarConstraint>>,
    /// function name: <function parameters>
    ///                 ^^^ look up constraints in constraints table
    functions: HashMap<Var, Vec<Var>>,
    counter: u32,
}

#[derive(thiserror::Error, Debug)]
pub enum TypeInferenceError {
    #[error("incompatible types for operator")]
    BadOperatorTypes(SpannedItem<Operator>, Vec<VarExpression>),
    #[error("incorrect arity")]
    BadArity(SpannedItem<Operator>, Vec<VarExpression>),
    #[error("type mismatch")]
    /// operator, expression, found type, expected type
    BadType(SpannedItem<Operator>, Expression<Var>, Ty, Ty),
    #[error("cannot unify")]
    Mismatch(TyConstraint, TyConstraint),
}

impl ConstraintGatherer {
    fn walk_statements(&mut self, statements: VarStatements) -> Result<(), TypeInferenceError> {
        for stmt in statements.0 {
            self.walk_stmt(stmt)?;
        }
        Ok(())
    }
    fn walk_stmt(&mut self, stmt: VarStatement) -> Result<(), TypeInferenceError> {
        match stmt {
            Statement::ForStatement(stmt) => {
                self.walk_for_stmt(stmt)?;
            }
            Statement::WhileStatement(stmt) => {
                self.walk_while_stmt(stmt)?;
            }
            Statement::FunctionDefinition(def) => {
                self.walk_function_def(def)?;
            }
            Statement::Expression(expr) => {
                self.walk_expr(expr)?;
            }
            Statement::AssignmentStatement(stmt) => {
                self.walk_assignment_stmt(stmt)?;
            }
        }
        Ok(())
    }
    fn walk_assignment_stmt(
        &mut self,
        stmt: VarAssignmentStatement,
    ) -> Result<(), TypeInferenceError> {
        self.insert_or_merge(
            stmt.ident.id,
            vec![VarConstraint::EqNonBuiltin(stmt.expression.0)],
        );
        self.walk_expr(stmt.expression)
    }

    fn walk_for_stmt(&mut self, stmt: VarFormStatement) -> Result<(), TypeInferenceError> {
        self.insert_or_merge(
            stmt.variable_of_iteration.id,
            vec![VarConstraint::EqBuiltin(Ty::Integer)],
        );
        self.insert_or_merge(
            stmt.start_expression.0,
            vec![VarConstraint::EqBuiltin(Ty::Integer)],
        );
        self.walk_expr(stmt.start_expression)?;
        self.insert_or_merge(
            stmt.stop_expression.0,
            vec![VarConstraint::EqBuiltin(Ty::Integer)],
        );
        self.walk_expr(stmt.stop_expression)?;
        self.walk_block(stmt.block)?;
        Ok(())
    }

    fn walk_while_stmt(&mut self, stmt: VarWhileStatement) -> Result<(), TypeInferenceError> {
        self.insert_or_merge(stmt.condition.0, vec![VarConstraint::EqBuiltin(Ty::Bool)]);
        self.walk_expr(stmt.condition)?;
        self.walk_block(stmt.block)
    }

    fn walk_function_def(&mut self, def: VarFunctionDefinition) -> Result<(), TypeInferenceError> {
        let mut entry = vec![];
        for param in def.parameters {
            entry.push(param);
        }
        self.functions.insert(def.function_name, entry);
        self.walk_block(def.block)
    }
    fn walk_block(&mut self, block: VarBlock) -> Result<(), TypeInferenceError> {
        self.walk_statements(block.statements)
    }

    fn handle_op(
        &mut self,
        arity: usize,
        op: SpannedItem<Operator>,
        ty: Ty,
        args: Vec<VarExpression>,
        expr: &VarExpression,
    ) -> Result<(), TypeInferenceError> {
        if args.len() != arity {
            return Err(TypeInferenceError::BadArity(op, args));
        }
        self.insert_or_merge(expr.0, vec![VarConstraint::EqBuiltin(ty.clone())]);
        for arg in args {
            self.insert_or_merge(arg.0, vec![VarConstraint::EqBuiltin(ty.clone())]);
            self.walk_expr(arg)?;
        }
        Ok(())
    }
    fn walk_expr(&mut self, expr: VarExpression) -> Result<(), TypeInferenceError> {
        dbg!(&expr);
        match expr.1.clone() {
            crate::transformer::var_to_u32::VarExpressionItem::Operator(op, args) => {
                let (ty, arity) = match op.item {
                    Operator::And | Operator::Or | Operator::EqualsEquals => (Ty::Bool, 2),
                    Operator::Not => (Ty::Bool, 1),
                    Operator::Plus
                    | Operator::Minus
                    | Operator::Times
                    | Operator::Divide
                    | Operator::IntegerDivide
                    | Operator::Mod => (Ty::Integer, 2),
                    Operator::Return => (Ty::Any, 1),
                    _ => unreachable!(),
                };
                self.handle_op(arity, op, ty, args, &expr)?;
            }
            crate::transformer::var_to_u32::VarExpressionItem::FunctionCall(fn_name, exprs) => {
                let args = if let Some(args) = self.functions.get(&fn_name) {
                    args
                } else {
                    panic!()
                }
                .clone();
                self.insert_or_merge(expr.0, vec![VarConstraint::EqNonBuiltin(fn_name.id)]);
                for (arg, exp) in exprs.into_iter().zip(args) {
                    self.insert_or_merge(arg.0, vec![VarConstraint::EqNonBuiltin(exp.clone().id)]);
                    self.walk_expr(arg)?;
                }
            }
            crate::transformer::var_to_u32::VarExpressionItem::Ident(ident) => {
                let constraints = self.ctx.get(&expr.0).map(Clone::clone).unwrap_or_default();
                self.insert_or_merge(ident.id, constraints);
            }
            crate::transformer::var_to_u32::VarExpressionItem::Literal(lit) => {
                dbg!(&lit);
                self.insert_or_merge(
                    expr.0,
                    vec![VarConstraint::EqBuiltin(match lit.item {
                        Literal::Boolean(_) => Ty::Bool,
                        Literal::Float(_) => Ty::Float,
                        Literal::String(_) => Ty::String,
                        Literal::Integer(_) => Ty::Integer,
                    })],
                );
            }
        }
        Ok(())
    }
    fn insert_or_merge(&mut self, id: u32, constraints: Vec<VarConstraint>) {
        if let Some(mut list) = self.ctx.remove(&id) {
            list.extend(constraints);
            self.ctx.insert(id, list);
        } else {
            self.ctx.insert(id, constraints);
        }
    }
    pub fn constraints_of(
        mut self,
        tree: VarStatements,
    ) -> Result<(HashMap<u32, Vec<VarConstraint>>, HashMap<Var, Vec<Var>>), TypeInferenceError>
    {
        self.walk_statements(tree)?;
        Ok((self.ctx, self.functions))
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum TyConstraint {
    BuiltIn(Ty),
    Var(u32),
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct Equals(TyConstraint, TyConstraint);

impl Equals {
    fn check_validity(&self) -> Result<(), TypeInferenceError> {
        match (&self.0, &self.1) {
            (TyConstraint::BuiltIn(ty1), TyConstraint::BuiltIn(ty2)) => {
                if ty1 != ty2 {
                    Err(TypeInferenceError::Mismatch(self.0.clone(), self.1.clone()))
                } else {
                    Ok(())
                }
            }
            (TyConstraint::BuiltIn(_), TyConstraint::Var(_))
            | (TyConstraint::Var(_), TyConstraint::BuiltIn(_)) => Ok(()),
            (TyConstraint::Var(_), TyConstraint::Var(_)) => Ok(()),
        }
    }
}

pub type TyEnv = HashSet<Equals>;

fn ty_env_of(ctx: HashMap<u32, Vec<VarConstraint>>) -> TyEnv {
    let mut ty_env = HashSet::new();
    for (var, constraints) in ctx.iter() {
        for constraint in constraints {
            match constraint {
                VarConstraint::EqBuiltin(ty) => {
                    ty_env.insert(Equals(
                        TyConstraint::Var(*var),
                        TyConstraint::BuiltIn(ty.clone()),
                    ));
                }
                VarConstraint::EqNonBuiltin(other_var) => {
                    ty_env.insert(Equals(
                        TyConstraint::Var(*var),
                        TyConstraint::Var(*other_var),
                    ));
                }
            }
        }
    }
    ty_env
}

#[derive(Debug, Clone)]
pub struct SolvedTyEnv(HashMap<u32, Either<Ty, u32>>);

fn unify(
    env: TyEnv,
    solved: &mut HashMap<u32, Either<Ty, u32>>,
) -> Result<SolvedTyEnv, TypeInferenceError> {
    if env.is_empty() {
        return Ok(SolvedTyEnv(solved.clone()));
    }
    let mut iter = env.into_iter();
    let next = if let Some(next) = iter.next() {
        next
    } else {
        return Ok(SolvedTyEnv(solved.clone()));
    };
    if let Err(e) = next.check_validity() {
        return Err(e);
    }
    let substitution = Substitution::from(next);
    if let Substitution::VarForVar(a, b) = &substitution {
        solved.insert(*a, Either::Right(*b));
    }
    if let Substitution::BuiltInForVar(var, ty) = &substitution {
        solved.insert(*var, Either::Left(ty.clone()));
    }
    let map = iter.filter_map(|t| substitution.apply(t));
    unify(map.collect(), solved)
}

#[derive(Debug)]
enum Substitution {
    Empty,
    BuiltInForVar(u32, Ty),
    VarForVar(u32, u32),
}

impl From<Equals> for Substitution {
    fn from(constraint: Equals) -> Substitution {
        match (constraint.0, constraint.1) {
            (TyConstraint::BuiltIn(_), TyConstraint::BuiltIn(_)) => Substitution::Empty,
            (TyConstraint::BuiltIn(ty), TyConstraint::Var(var))
            | (TyConstraint::Var(var), TyConstraint::BuiltIn(ty)) => {
                Substitution::BuiltInForVar(var, ty)
            }
            (TyConstraint::Var(var_a), TyConstraint::Var(var_b)) => {
                Substitution::VarForVar(var_a, var_b)
            }
        }
    }
}

impl Substitution {
    fn apply(&self, equals: Equals) -> Option<Equals> {
        match self {
            Substitution::Empty => Some(equals),
            Substitution::BuiltInForVar(sub_var, builtin) => match (&equals.0, &equals.1) {
                (TyConstraint::BuiltIn(_), TyConstraint::BuiltIn(_)) => Some(equals),
                (TyConstraint::BuiltIn(ty), TyConstraint::Var(var))
                | (TyConstraint::Var(var), TyConstraint::BuiltIn(ty)) => {
                    if sub_var == var {
                        Some(Equals(
                            TyConstraint::BuiltIn(builtin.clone()),
                            TyConstraint::BuiltIn(ty.clone()),
                        ))
                    } else {
                        Some(equals)
                    }
                }
                (TyConstraint::Var(var_a), TyConstraint::Var(var_b)) => Some(Equals(
                    if sub_var == var_a {
                        TyConstraint::BuiltIn(builtin.clone())
                    } else {
                        TyConstraint::Var(*var_a)
                    },
                    if sub_var == var_b {
                        TyConstraint::BuiltIn(builtin.clone())
                    } else {
                        TyConstraint::Var(*var_b)
                    },
                )),
            },
            // whereve we see a, we want to replace it with b
            Substitution::VarForVar(var_b, var_a) => match (&equals.0, &equals.1) {
                (TyConstraint::BuiltIn(_), TyConstraint::BuiltIn(_)) => Some(equals),
                (TyConstraint::BuiltIn(builtin), TyConstraint::Var(var))
                | (TyConstraint::Var(var), TyConstraint::BuiltIn(builtin)) => {
                    let constraint = Some(Equals(
                        TyConstraint::BuiltIn(builtin.clone()),
                        if var == var_a {
                            TyConstraint::Var(*var_b)
                        } else {
                            TyConstraint::Var(*var)
                        },
                    ));
                    constraint
                }
                (TyConstraint::Var(to_sub_a), TyConstraint::Var(to_sub_b)) => {
                    if to_sub_a == to_sub_b && to_sub_a == var_a {
                        None
                    } else {
                        Some(Equals(
                            if to_sub_a == var_a {
                                TyConstraint::Var(*var_a)
                            } else {
                                TyConstraint::Var(*to_sub_b)
                            },
                            TyConstraint::Var(*var_b),
                        ))
                    }
                }
            },
        }
    }
}

pub fn ty_infer(ast: VarStatements) -> Result<(VarStatements, SolvedTyEnv), TypeInferenceError> {
    let (constraints, _) = ConstraintGatherer::default().constraints_of(ast.clone())?;
    let constraints = ty_env_of(constraints);
    let mut solved = HashMap::new();
    let solutions = unify(constraints, &mut solved)?;
    Ok((ast, solutions))
}
