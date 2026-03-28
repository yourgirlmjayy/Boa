# Cobra Compiler

This project implements a compiler for the Cobra language, an extension of the Boa language. The compiler translates Cobra programs into x86-64 assembly.

## Features Implemented

### Core Language
- Integers (tagged representation)
- Booleans (`true`, `false`)
- Variables and `let` bindings
- Arithmetic operations: `+`, `-`, `*`
- Unary operations: `add1`, `sub1`, `negate`

### Type Checking
- Runtime type checks for arithmetic operations
- Mixed-type operations (e.g. `(+ true 5)`) trigger errors
- Equality checks enforce matching types

### Control Flow
- `if` expressions with proper branching
- Nested `if` expressions supported

### Comparison Operators
- `<`, `>`, `<=`, `>=`
- `=` (with type checking)

### Advanced Features
- `block` expressions
- `loop` with `break`
- `set!` for variable mutation
- `input` support

### Error Handling
- Runtime error for invalid arguments
- All type errors jump to a shared `error` label
- Calls into `snek_error` in runtime

---

## Tagged Representation

Values are represented as:
- Numbers: `n << 1`
- `true`: `3`
- `false`: `1`

This allows efficient runtime type checking using the least significant bit.

---

## Project Structure


cobra/
├── src/
│ └── main.rs # Compiler implementation
├── runtime/
│ └── start.rs # Runtime (entry point + error handling)
├── Cargo.toml
└── README.md


---

## Running the Compiler

Compile a `.snek` program:

```bash
cargo run input.snek output.s

Then assemble and run:

make output.run
./output.run
Running Tests

Run all tests:

cargo test

Tests cover:

Parsing correctness
Control flow behavior
Type errors
Loops and break
Comparisons
Mutation (set!)
Mixed-type operations
Example Programs
(+ 1 2)
(if true 5 10)
(loop (break 7))
(let ((x 5)) (set! x 10))