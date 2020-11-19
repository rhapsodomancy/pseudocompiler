//! A small implementation of a number of logic-programming concepts which are useful for
//! working out whether there exists a set of types which can be determined to "fit" a
//! program. Once the types have been determined it makes it easier to reason about the
//! programs (e.g. formal verification).

pub trait Fact<PREDICATE> {
    fn holds() -> bool;
}

pub trait Query<PREDICATE> {
    fn resolve();
}
