# Pseudocompiler

An (experimental) programming language.

## Installation

This project is in early stages and isn't ready for a massive programming project to be written in
it. If you'd like to try it out locally, you'll need to build it from source.

In the meantime you can try it out on the [playground](https://compiler.teymour.tk).

## Targets

At present the compiler only emits Javascript code. Integration with LLVM is planned.

## Motivation

This is a (pretty) spec-compliant implementation of OCR's provisional "pseudocode" specification
(with some of the most questionable parts removed) which you can find on the interweb.

## Planned things
 - User-defined types
    - Records (structs, but OCR would call them records because they're like that)
    - Enumerations (both unions and algebraic data types)
 - Implicit reference counting and ownership
 - LLVM or Cranelift backend (currently we're throwing out Javascript code rather inexpertly)
 - Concurrency (lots of it!)
