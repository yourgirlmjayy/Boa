# Boa Compiler

This project implements a compiler for the Boa language in Rust.

Boa extends the Adder language with:
- variables (identifiers)
- let expressions with multiple bindings
- binary operations: +, -, *

The compiler reads a Boa program written as an S-expression and generates
x86-64 assembly code.

## Supported Expressions

The compiler supports the following syntax:

<number>
x

(add1 e)
(sub1 e)

(+ e1 e2)
(- e1 e2)
(* e1 e2)

(let ((x e1) (y e2) ...) body)

## Implementation

The compiler works in two main stages.

### Parsing

`parse_expr` converts the input S-expression into an Abstract Syntax Tree (AST).
The AST types include:

- `Number`
- `Id`
- `UnOp`
- `BinOp`
- `Let`

Bindings in let expressions are parsed using `parse_bind`.

### Compilation

`compile_to_instrs` converts the AST into x86-64 assembly instructions.

Results of expressions are stored in the `rax` register.

Variables are stored on the stack using offsets from `rsp`. The environment
maps variable names to their stack locations.

Binary operations use the stack to temporarily store the left operand while the
right operand is evaluated.

## Error Handling

The compiler reports errors for:

- invalid syntax
- duplicate variable bindings in a `let`
- use of unbound variables

## Running the Compiler

Build the project:
cargo build


Compile a Boa program:


cargo run -- input.snek output.s


Run tests:


cargo test


## Files

- `src/main.rs` – parser and compiler implementation
- `runtime/start.rs` – runtime entry point
- `test/` – example Boa programs
