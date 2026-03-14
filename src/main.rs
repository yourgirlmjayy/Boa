// Assignment 2: Boa Compiler - Completed Code
//
// This compiler translates Boa programs into x86-64 assembly.
//
// Boa extends Adder with:
//   - Variables (identifiers)
//   - Let expressions with multiple bindings
//   - Binary operations: +, -, *

use im::HashMap;
use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

// ============= Abstract Syntax Tree =============

/// Unary operators
#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
}

/// Binary operators
#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
}

/// The Boa expression AST
///
/// Grammar:
///   <expr> := <number>
///           | <identifier>
///           | (let (<binding>+) <expr>)
///           | (add1 <expr>) | (sub1 <expr>)
///           | (+ <expr> <expr>) | (- <expr> <expr>) | (* <expr> <expr>)
///   <binding> := (<identifier> <expr>)
#[derive(Debug)]
enum Expr {
    Number(i32),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
}

// ============= Assembly Representation =============

/// Values that can appear in assembly instructions
#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32), // e.g., [rsp - 8]
}

/// Registers we use
#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
}

/// Assembly instructions we generate
#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
}

// ============= Parsing Helpers =============

/// Returns true if the given name is a reserved word
///
/// Reserved words:
///   let, add1, sub1
fn is_reserved(name: &str) -> bool {
    name == "let" || name == "add1" || name == "sub1"
}

/// Returns true if the given name is a valid Boa identifier
///
/// Identifier grammar:
///   [a-zA-Z][a-zA-Z0-9_-]*
///
/// Examples of valid identifiers:
///   x
///   temp
///   my_var
///   value-1
///
/// Examples of invalid identifiers:
///   1x
///   +
///   *
fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();

    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() => (),
        _ => return false,
    }

    chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
}

// ============= Parsing =============

/// Parse an S-expression into our Expr AST
///
/// Examples of valid Boa expressions:
///   42                          -> Number(42)
///   x                           -> Id("x")
///   (add1 5)                    -> UnOp(Add1, Number(5))
///   (+ 1 2)                     -> BinOp(Plus, Number(1), Number(2))
///   (let ((x 5)) x)             -> Let([("x", Number(5))], Id("x"))
///   (let ((x 5) (y 6)) (+ x y)) -> Let([("x", Number(5)), ("y", Number(6))], BinOp(...))
///
/// Error handling:
///   - Invalid syntax: panic!("Invalid")
///   - Number out of i32 range: panic!("Invalid")
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        // Handle number atoms
        //
        // Example:
        //   42 -> Number(42)
        Sexp::Atom(I(n)) => Expr::Number(
            i32::try_from(*n).unwrap_or_else(|_| panic!("Invalid"))
        ),

        // Handle identifier atoms
        //
        // Example:
        //   x -> Id("x")
        Sexp::Atom(S(name)) => {
            // Reject reserved words and invalid identifier forms
            if is_reserved(name) || !is_valid_identifier(name) {
                panic!("Invalid");
            }

            Expr::Id(name.clone())
        }

        // Handle list expressions
        //
        // Examples:
        //   (add1 5)
        //   (+ 1 2)
        //   (let ((x 5)) x)
        Sexp::List(vec) => {
            match &vec[..] {
                // Parse unary add1
                //
                // Example:
                //   (add1 5) -> UnOp(Add1, Number(5))
                [Sexp::Atom(S(op)), e] if op == "add1" =>
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),

                // Parse unary sub1
                //
                // Example:
                //   (sub1 5) -> UnOp(Sub1, Number(5))
                [Sexp::Atom(S(op)), e] if op == "sub1" =>
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),

                // Parse binary plus
                //
                // Example:
                //   (+ 1 2) -> BinOp(Plus, Number(1), Number(2))
                [Sexp::Atom(S(op)), e1, e2] if op == "+" =>
                    Expr::BinOp(
                        Op2::Plus,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    ),

                // Parse binary minus
                //
                // Example:
                //   (- 8 3) -> BinOp(Minus, Number(8), Number(3))
                [Sexp::Atom(S(op)), e1, e2] if op == "-" =>
                    Expr::BinOp(
                        Op2::Minus,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    ),

                // Parse binary multiplication
                //
                // Example:
                //   (* 4 5) -> BinOp(Times, Number(4), Number(5))
                [Sexp::Atom(S(op)), e1, e2] if op == "*" =>
                    Expr::BinOp(
                        Op2::Times,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    ),

                // Parse let expressions
                //
                // Example:
                //   (let ((x 5) (y 6)) (+ x y))
                [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                    // A let expression must have at least one binding
                    if bindings.is_empty() {
                        panic!("Invalid");
                    }

                    // Parse each binding into a (String, Expr) pair
                    let parsed_binds = bindings.iter().map(parse_bind).collect();

                    Expr::Let(parsed_binds, Box::new(parse_expr(body)))
                }

                _ => panic!("Invalid"),
            }
        }

        _ => panic!("Invalid"),
    }
}

/// Parse a single binding from a let expression
///
/// A binding looks like: (x 5) or (my-var (+ 1 2))
/// Returns a tuple of (variable_name, expression)
///
/// Examples:
///   (x 5)       -> ("x", Number(5))
///   (y (+ 1 2)) -> ("y", BinOp(Plus, ...))
///
/// Error handling:
///   - Invalid binding syntax: panic!("Invalid")
fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        // A binding must be a list of exactly two elements
        Sexp::List(vec) => match &vec[..] {
            // Parse the variable name and binding expression
            [Sexp::Atom(S(name)), e] => {
                // Reject reserved words and invalid identifier forms
                if is_reserved(name) || !is_valid_identifier(name) {
                    panic!("Invalid");
                }

                (name.clone(), parse_expr(e))
            }

            _ => panic!("Invalid"),
        },

        _ => panic!("Invalid"),
    }
}

// ============= Compilation =============

/// Compile an expression to a list of assembly instructions
///
/// Parameters:
///   - e: the expression to compile
///   - si: stack index - the next available stack slot (starts at 2)
///         Stack slots are at [rsp - 8*si], e.g., si=2 means [rsp - 16]
///   - env: environment mapping variable names to stack offsets
///
/// The compiled code should leave its result in RAX.
///
/// Stack layout:
///   [rsp - 8]  : reserved (return address area)
///   [rsp - 16] : first variable (si=2)
///   [rsp - 24] : second variable (si=3)
///   ...
///
/// Examples:
///   Number(5) -> [IMov(Reg(RAX), Imm(5))]
///
///   UnOp(Add1, Number(5)) ->
///     [IMov(Reg(RAX), Imm(5)), IAdd(Reg(RAX), Imm(1))]
///
///   BinOp(Plus, Number(1), Number(2)) ->
///     1. Compile left operand (result in RAX)
///     2. Save RAX to stack at [rsp - 8*si]
///     3. Compile right operand (result in RAX)
///     4. Add stack value to RAX
///
///   Let([(x, 5)], Id(x)) ->
///     1. Compile binding expression (5)
///     2. Store result at stack slot
///     3. Add x -> stack_offset to environment
///     4. Compile body with updated environment
fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>) -> Vec<Instr> {
    match e {
        // Number - move immediate value to RAX
        //
        // Example:
        //   5 -> mov rax, 5
        Expr::Number(n) => {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))]
        }

        // Id - look up variable in environment and load from stack
        //
        // Example:
        //   x -> mov rax, [rsp - 16]
        Expr::Id(name) => {
            // Look up the variable's stack offset in the environment
            let offset = *env
                .get(name)
                .unwrap_or_else(|| panic!("Unbound variable identifier {}", name));

            // Load the variable's value from the stack into RAX
            vec![Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, offset),
            )]
        }

        // UnOp - compile the subexpression, then apply the unary operation
        //
        // Example:
        //   (add1 5)
        Expr::UnOp(Op1::Add1, subexpr) => {
            // Compile the subexpression so its result is in RAX
            let mut instrs = compile_to_instrs(subexpr, si, env);

            // Add 1 to the result in RAX
            instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));

            instrs
        }

        // Example:
        //   (sub1 5)
        Expr::UnOp(Op1::Sub1, subexpr) => {
            // Compile the subexpression so its result is in RAX
            let mut instrs = compile_to_instrs(subexpr, si, env);

            // Subtract 1 from the result in RAX
            instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));

            instrs
        }

        // BinOp - compile both operands using the stack
        //
        // Strategy:
        //   1. Compile left operand
        //   2. Save it on the stack
        //   3. Compile right operand
        //   4. Add saved left operand to RAX
        //
        // Example:
        //   (+ 4 5)
        Expr::BinOp(Op2::Plus, left, right) => {
            // Compute the stack offset for storing the left operand temporarily
            let stack_offset = -8 * si;

            // Compile the left operand so its result is in RAX
            let mut instrs = compile_to_instrs(left, si, env);

            // Save the left operand on the stack
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));

            // Compile the right operand with the next available stack slot
            instrs.extend(compile_to_instrs(right, si + 1, env));

            // Add the saved left operand to the right operand currently in RAX
            instrs.push(Instr::IAdd(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));

            instrs
        }

        // Strategy:
        //   1. Compile left operand
        //   2. Save it on the stack
        //   3. Compile right operand
        //   4. Save the right operand
        //   5. Restore the left operand into RAX
        //   6. Subtract right from left
        //
        // Example:
        //   (- 8 3) should compute 8 - 3
        Expr::BinOp(Op2::Minus, left, right) => {
            // Compute stack offsets for temporary left and right operand storage
            let left_offset = -8 * si;
            let right_offset = -8 * (si + 1);

            // Compile the left operand so its result is in RAX
            let mut instrs = compile_to_instrs(left, si, env);

            // Save the left operand on the stack
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));

            // Compile the right operand with the next available stack slot
            instrs.extend(compile_to_instrs(right, si + 1, env));

            // Save the right operand before overwriting RAX
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, right_offset),
                Val::Reg(Reg::RAX),
            ));

            // Reload the left operand into RAX
            instrs.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, left_offset),
            ));

            // Subtract the right operand from the left operand
            instrs.push(Instr::ISub(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, right_offset),
            ));

            instrs
        }

        // Strategy:
        //   1. Compile left operand
        //   2. Save it on the stack
        //   3. Compile right operand
        //   4. Multiply RAX by saved left operand
        //
        // Example:
        //   (* 4 5)
        Expr::BinOp(Op2::Times, left, right) => {
            // Compute the stack offset for storing the left operand temporarily
            let stack_offset = -8 * si;

            // Compile the left operand so its result is in RAX
            let mut instrs = compile_to_instrs(left, si, env);

            // Save the left operand on the stack
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));

            // Compile the right operand with the next available stack slot
            instrs.extend(compile_to_instrs(right, si + 1, env));

            // Multiply the right operand in RAX by the saved left operand
            instrs.push(Instr::IMul(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, stack_offset),
            ));

            instrs
        }

        // Let - bind variables and compile the body
        //
        // Strategy:
        //   1. Check for duplicate bindings
        //   2. Compile each binding left to right
        //   3. Store each binding on the stack
        //   4. Extend the environment after each binding
        //   5. Compile the body in the extended environment
        //
        // Example:
        //   (let ((x 5) (y (+ x 1))) (* x y))
        Expr::Let(bindings, body) => {
            // Keep track of names already seen in this let
            let mut seen: Vec<String> = Vec::new();

            // Start with the current environment and extend it as bindings are processed
            let mut new_env = env.clone();

            // Collect all generated instructions here
            let mut instrs: Vec<Instr> = Vec::new();

            // Track the current stack index
            let mut curr_si = si;

            for (name, expr) in bindings {
                // Reject duplicate bindings in the same let expression
                if seen.contains(name) {
                    panic!("Duplicate binding");
                }

                // Record that this name has already been used
                seen.push(name.clone());

                // Compile the binding expression using the current environment
                instrs.extend(compile_to_instrs(expr, curr_si, &new_env));

                // Compute the stack offset for this binding
                let offset = -8 * curr_si;

                // Store the binding value from RAX into its stack slot
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, offset),
                    Val::Reg(Reg::RAX),
                ));

                // Extend the environment for later bindings and the body
                new_env = new_env.update(name.clone(), offset);

                // Move to the next available stack slot
                curr_si += 1;
            }

            // Compile the body using the fully extended environment
            instrs.extend(compile_to_instrs(body, curr_si, &new_env));

            instrs
        }
    }
}

// ============= Code Generation =============

/// Convert a Val to its assembly string representation
fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => String::from("rax"),
        Val::Reg(Reg::RSP) => String::from("rsp"),
        Val::Imm(n) => format!("{}", n),

        Val::RegOffset(Reg::RSP, offset) if *offset < 0 => {
            format!("[rsp - {}]", -offset)
        }
        Val::RegOffset(Reg::RSP, offset) => {
            format!("[rsp + {}]", offset)
        }

        Val::RegOffset(Reg::RAX, offset) if *offset < 0 => {
            format!("[rax - {}]", -offset)
        }
        Val::RegOffset(Reg::RAX, offset) => {
            format!("[rax + {}]", offset)
        }
    }
}

/// Convert an Instr to its assembly string representation
fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("mov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("add {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("sub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("imul {}, {}", val_to_str(dst), val_to_str(src)),
    }
}

/// Compile an expression to a complete assembly string
fn compile(e: &Expr) -> String {
    // Start with an empty environment
    let env: HashMap<String, i32> = HashMap::new();

    // Compile starting at stack slot 2
    let instrs = compile_to_instrs(e, 2, &env);

    instrs
        .iter()
        .map(|i| instr_to_str(i))
        .collect::<Vec<String>>()
        .join("\n  ")
}

// ============= Main =============

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        eprintln!("Usage: {} <input.snek> <output.s>", args[0]);
        std::process::exit(1);
    }

    let in_name = &args[1];
    let out_name = &args[2];

    // Read input file
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Parse S-expression from text
    let sexp = parse(&in_contents).unwrap_or_else(|_| panic!("Invalid"));

    // Convert S-expression to our AST
    let expr = parse_expr(&sexp);

    // Generate assembly instructions
    let instrs = compile(&expr);

    // Wrap instructions in assembly program template
    let asm_program = format!(
        "section .text
global our_code_starts_here
our_code_starts_here:
  {}
  ret
",
        instrs
    );

    // Write output assembly file
    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

// ============= TESTS =============
//
// Run with: cargo test
//
// These tests help verify your implementation. Uncomment and add more!

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to parse a string directly
    fn parse_str(s: &str) -> Expr {
        parse_expr(&parse(s).unwrap())
    }

    // ===== Parsing Tests =====

    #[test]
    fn test_parse_number() {
        let expr = parse_str("42");
        match expr {
            Expr::Number(42) => (),
            _ => panic!("Expected Number(42), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_identifier() {
        let expr = parse_str("x");
        match expr {
            Expr::Id(name) => assert_eq!(name, "x"),
            _ => panic!("Expected Id(\"x\"), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_add1() {
        let expr = parse_str("(add1 5)");
        match expr {
            Expr::UnOp(Op1::Add1, _) => (),
            _ => panic!("Expected UnOp(Add1, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_binary_plus() {
        let expr = parse_str("(+ 1 2)");
        match expr {
            Expr::BinOp(Op2::Plus, _, _) => (),
            _ => panic!("Expected BinOp(Plus, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_let_simple() {
        let expr = parse_str("(let ((x 5)) x)");
        match expr {
            Expr::Let(bindings, _) => {
                assert_eq!(bindings.len(), 1);
                assert_eq!(bindings[0].0, "x");
            }
            _ => panic!("Expected Let, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_let_multiple_bindings() {
        let expr = parse_str("(let ((x 5) (y 6)) (+ x y))");
        match expr {
            Expr::Let(bindings, _) => {
                assert_eq!(bindings.len(), 2);
            }
            _ => panic!("Expected Let with 2 bindings, got {:?}", expr),
        }
    }

    // ===== Error Tests =====

    #[test]
    #[should_panic(expected = "Duplicate binding")]
    fn test_duplicate_binding() {
        let expr = parse_str("(let ((x 1) (x 2)) x)");
        let env: HashMap<String, i32> = HashMap::new();
        compile_to_instrs(&expr, 2, &env);
    }

    #[test]
    #[should_panic(expected = "Unbound variable identifier y")]
    fn test_unbound_variable() {
        let expr = parse_str("y");
        let env: HashMap<String, i32> = HashMap::new();
        compile_to_instrs(&expr, 2, &env);
    }

    #[test]
    #[should_panic(expected = "Invalid")]
    fn test_empty_let_invalid() {
        parse_str("(let () 5)");
    }

    #[test]
    #[should_panic(expected = "Invalid")]
    fn test_reserved_binding_invalid() {
        parse_str("(let ((add1 5)) add1)");
    }

    // ===== Compilation Tests =====

    #[test]
    fn test_compile_number() {
        let expr = Expr::Number(42);
        let env: HashMap<String, i32> = HashMap::new();
        let instrs = compile_to_instrs(&expr, 2, &env);
        assert_eq!(instrs.len(), 1);
    }
}