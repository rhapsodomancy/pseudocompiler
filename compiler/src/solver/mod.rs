//! A "solver" for programs. This will try to develop a set of types which make the program
//! run. If it is not possible to do this, it returns an error explaining why the program
//! is not solvable.
//!
//! This is inspired by the logic-programming paradigm of PROLOG, with a lot of the
//! Rust <-> PROLOG conversion coming from the Rust compiler's Chalk project.

use thiserror::Error as ThisError;

use crate::parser::Statements;

#[derive(ThisError, Debug)]
/// An error encountered while solving a file. I'm still not sure on how to make this an
/// easily understood error, especially given that error messages are supposed to explain
/// to people using the compiler with limited experience.
pub enum SolveError {}

mod prolog;

/// A "solution" to a file.
pub struct Solution {}

/// The entry-point for solving a file.
pub fn solve(input: Statements) -> Result<Solution, SolveError> {
    todo!()
}
