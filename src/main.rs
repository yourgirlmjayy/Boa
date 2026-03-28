// Assignment 2: Boa Compiler - Completed Code
// Assignment 3: Update code with Cobra
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
    Negate,
    IsNum,
    IsBool,
}

/// Binary operators
#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Equal,
}

/// The Cobra expression AST
///
/// Grammar:
///   <expr> := <number>
///           | true | false
///           | input
///           | <identifier>
///           | (let (<binding>+) <expr>)
///           | (add1 <expr>) | (sub1 <expr>) | (negate <expr>)
///           | (+ <expr> <expr>) | (- <expr> <expr>) | (* <expr> <expr>)
///           | (< <expr> <expr>) | (> <expr> <expr>)
///           | (<= <expr> <expr>) | (>= <expr> <expr>) | (= <expr> <expr>)
///           | (isnum <expr>) | (isbool <expr>)
///           | (if <expr> <expr> <expr>)
///           | (block <expr>+)
///           | (loop <expr>)
///           | (break <expr>)
///           | (set! <identifier> <expr>)
///   <binding> := (<identifier> <expr>)
#[derive(Debug)]
enum Expr {
    Number(i32),
    Bool(bool),
    Input,
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
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
    RBX,
    RSP,
    RDI,
}

/// Assembly instructions we generate
#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ICmp(Val, Val),
    IJe(String),
    IJmp(String),
    IAnd(Val, Val),
    IJne(String),
    IJg(String),
    IJge(String),
    IJl(String),
    IJle(String),
    ILabel(String),
}

// ============= Parsing Helpers =============

/// Returns true if the given name is a reserved word
///
/// Reserved words:
///   let, add1, sub1, negate, isnum, isbool,
///   true, false, input, if, block, loop, break, set!
fn is_reserved(name: &str) -> bool {
    matches!(
        name,
        "let"
            | "add1"
            | "sub1"
            | "negate"
            | "isnum"
            | "isbool"
            | "true"
            | "false"
            | "input"
            | "if"
            | "block"
            | "loop"
            | "break"
            | "set!"
    )
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
/// Examples of valid Cobra expressions:
///   42                          -> Number(42)
///   true                        -> Bool(true)
///   false                       -> Bool(false)
///   input                       -> Input
///   x                           -> Id("x")
///   (add1 5)                    -> UnOp(Add1, Number(5))
///   (isnum 5)                   -> UnOp(IsNum, Number(5))
///   (+ 1 2)                     -> BinOp(Plus, Number(1), Number(2))
///   (< 1 2)                     -> BinOp(Less, Number(1), Number(2))
///   (if true 5 10)              -> If(Bool(true), Number(5), Number(10))
///   (block 1 2 3)               -> Block([Number(1), Number(2), Number(3)])
///   (loop (break 5))            -> Loop(Break(Number(5)))
///   (set! x 5)                  -> Set("x", Number(5))
///   (let ((x 5)) x)             -> Let([("x", Number(5))], Id("x"))
///
/// Error handling:
///   - Invalid syntax: panic!("Invalid")
///   - Number out of i32 range: panic!("Invalid")
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(
            i32::try_from(*n).unwrap_or_else(|_| panic!("Invalid"))
        ),

        Sexp::Atom(S(name)) if name == "true" => Expr::Bool(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Bool(false),
        Sexp::Atom(S(name)) if name == "input" => Expr::Input,

        Sexp::Atom(S(name)) => {
            if is_reserved(name) || !is_valid_identifier(name) {
                panic!("Invalid");
            }

            Expr::Id(name.clone())
        }

        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" =>
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),

                [Sexp::Atom(S(op)), e] if op == "sub1" =>
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),

                [Sexp::Atom(S(op)), e] if op == "negate" =>
                    Expr::UnOp(Op1::Negate, Box::new(parse_expr(e))),

                [Sexp::Atom(S(op)), e] if op == "isnum" =>
                    Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),

                [Sexp::Atom(S(op)), e] if op == "isbool" =>
                    Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),

                [Sexp::Atom(S(op)), e1, e2] if op == "+" =>
                    Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                [Sexp::Atom(S(op)), e1, e2] if op == "-" =>
                    Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                [Sexp::Atom(S(op)), e1, e2] if op == "*" =>
                    Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                [Sexp::Atom(S(op)), e1, e2] if op == "<" =>
                    Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                [Sexp::Atom(S(op)), e1, e2] if op == ">" =>
                    Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                [Sexp::Atom(S(op)), e1, e2] if op == "<=" =>
                    Expr::BinOp(Op2::LessEq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                [Sexp::Atom(S(op)), e1, e2] if op == ">=" =>
                    Expr::BinOp(Op2::GreaterEq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                [Sexp::Atom(S(op)), e1, e2] if op == "=" =>
                    Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),

                [Sexp::Atom(S(op)), cond, thn, els] if op == "if" =>
                    Expr::If(
                        Box::new(parse_expr(cond)),
                        Box::new(parse_expr(thn)),
                        Box::new(parse_expr(els)),
                    ),

                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    if exprs.is_empty() {
                        panic!("Invalid");
                    }
                    Expr::Block(exprs.iter().map(parse_expr).collect())
                }

                [Sexp::Atom(S(op)), e] if op == "loop" =>
                    Expr::Loop(Box::new(parse_expr(e))),

                [Sexp::Atom(S(op)), e] if op == "break" =>
                    Expr::Break(Box::new(parse_expr(e))),

                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                    if is_reserved(name) || !is_valid_identifier(name) {
                        panic!("Invalid");
                    }
                    Expr::Set(name.clone(), Box::new(parse_expr(e)))
                }

                [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                    if bindings.is_empty() {
                        panic!("Invalid");
                    }

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

// HELPER FUNCTION
// Generate a unique label name for control flow
fn new_label(label_count: &mut i32, prefix: &str) -> String {
    let current = *label_count;
    *label_count += 1;
    format!("{}_{}", prefix, current)
}

/// Compile an expression to a list of assembly instructions
///
/// Parameters:
///   - e: the expression to compile
///   - si: stack index - the next available stack slot (starts at 2)
///         Stack slots are at [rsp - 8*si], e.g., si=2 means [rsp - 16]
///   - env: environment mapping variable names to stack offsets
///   - label_count: counter used to generate unique labels for control flow
///   - break_target: label for the innermost loop exit
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
///   Number(5) -> [IMov(Reg(RAX), Imm(10))]   // tagged number
///
///   Bool(true) -> [IMov(Reg(RAX), Imm(3))]
///
///   UnOp(Add1, Number(5)) ->
///     [IMov(Reg(RAX), Imm(10)), IAdd(Reg(RAX), Imm(2))]
///
///   BinOp(Plus, Number(1), Number(2)) ->
///     1. Compile left operand (result in RAX)
///     2. Save RAX to stack at [rsp - 8*si]
///     3. Compile right operand (result in RAX)
///     4. Add stack value to RAX
///
///   If(cond, thn, els) ->
///     1. Compile condition
///     2. Compare RAX with 1 (false)
///     3. Jump to else branch if equal
///     4. Compile then branch
///     5. Jump to end
///     6. Compile else branch
///
///   Loop(body) ->
///     1. Create loop start and end labels
///     2. Compile the body with the end label as the break target
///     3. Jump back to the loop start
///
///   Break(expr) ->
///     1. Compile the break expression
///     2. Jump to the innermost loop end label
///
///   Let([(x, 5)], Id(x)) ->
///     1. Compile binding expression (5)
///     2. Store result at stack slot
///     3. Add x -> stack_offset to environment
///     4. Compile body with updated environment
fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, label_count: &mut i32, break_target: &Option<String>) -> Vec<Instr> {

    match e {
        // ASSIGNMENT 3: TASK 2: Bool - move tagged boolean into RAX
        //
        // Example:
        //   true  -> mov rax, 3
        //   false -> mov rax, 1
        Expr::Bool(true) => {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))]
        }

        Expr::Bool(false) => {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))]
        }

        // ASSIGNMENT 3: TASK 3: If - evaluate condition and branch accordingly
        //
        // Strategy:
        //   1. Compile condition
        //   2. Compare RAX with 1 (false)
        //   3. Jump to else if condition is false
        //   4. Compile then branch
        //   5. Jump to end
        //   6. Compile else branch
        Expr::If(cond, thn, els) => {
            let else_label = new_label(label_count, "if_else");
            let end_label = new_label(label_count, "if_end");

            let mut instrs = compile_to_instrs(cond, si, env, label_count, break_target);

            instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::IJe(else_label.clone()));

            instrs.extend(compile_to_instrs(thn, si, env, label_count, break_target));
            instrs.push(Instr::IJmp(end_label.clone()));

            instrs.push(Instr::ILabel(else_label));
            instrs.extend(compile_to_instrs(els, si, env, label_count, break_target));

            instrs.push(Instr::ILabel(end_label));

            instrs
        }

        // ASSIGNMENT 3: TASK 5: Loop - repeatedly evaluate the body until a break exits the loop
        //
        // Strategy:
        //   1. Create loop start and end labels
        //   2. Compile the body with the loop end as the break target
        //   3. Jump back to the loop start
        Expr::Loop(body) => {
            let start_label = new_label(label_count, "loop_start");
            let end_label = new_label(label_count, "loop_end");

            let mut instrs = vec![Instr::ILabel(start_label.clone())];

            let new_break_target = Some(end_label.clone());
            instrs.extend(compile_to_instrs(body, si, env, label_count, &new_break_target));

            instrs.push(Instr::IJmp(start_label));
            instrs.push(Instr::ILabel(end_label));

            instrs
        }

        Expr::Input => {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
        }
        
        Expr::Block(exprs) => {
            let mut instrs = Vec::new();
            for expr in exprs {
                instrs.extend(compile_to_instrs(expr, si, env, label_count, break_target));
            }
            instrs
        }
        
        Expr::Set(name, expr) => {
            let offset = *env
                .get(name)
                .unwrap_or_else(|| panic!("Unbound variable identifier {}", name));
        
            let mut instrs = compile_to_instrs(expr, si, env, label_count, break_target);
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, offset),
                Val::Reg(Reg::RAX),
            ));
            instrs
        }

        // Break - exit the innermost loop with the given value
        //
        // Example:
        //   (break 5)
        Expr::Break(expr) => {
            match break_target {
                Some(label) => {
                    let mut instrs = compile_to_instrs(expr, si, env, label_count, break_target);
                    instrs.push(Instr::IJmp(label.clone()));
                    instrs
                }
                None => panic!("break outside loop"),
            }
        }
        
        // Number - move immediate value to RAX
        //
        // Example:
        //   5 -> mov rax, 5
        Expr::Number(n) => {
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))]
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
        
        // Example:
        //   (negate 5)
        // Example:
        //   (negate 5)
        Expr::UnOp(Op1::Negate, subexpr) => {
            let mut instrs = compile_to_instrs(subexpr, si, env, label_count, break_target);

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::ISub(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));

            instrs
        }

        // Example:
        //   (isnum 5)
        Expr::UnOp(Op1::IsNum, subexpr) => {
            let mut instrs = compile_to_instrs(subexpr, si, env, label_count, break_target);
            let false_label = new_label(label_count, "isnum_false");
            let end_label = new_label(label_count, "isnum_end");

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne(false_label.clone()));

            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            instrs.push(Instr::IJmp(end_label.clone()));

            instrs.push(Instr::ILabel(false_label));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

            instrs.push(Instr::ILabel(end_label));

            instrs
        }

        // Example:
        //   (isbool true)
        Expr::UnOp(Op1::IsBool, subexpr) => {
            let mut instrs = compile_to_instrs(subexpr, si, env, label_count, break_target);
            let false_label = new_label(label_count, "isbool_false");
            let end_label = new_label(label_count, "isbool_end");

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::IJne(false_label.clone()));

            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            instrs.push(Instr::IJmp(end_label.clone()));

            instrs.push(Instr::ILabel(false_label));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

            instrs.push(Instr::ILabel(end_label));

            instrs
        }

        // Example:
        //   (add1 5)
        Expr::UnOp(Op1::Add1, subexpr) => {
            let mut instrs = compile_to_instrs(subexpr, si, env, label_count, break_target);
            instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
            instrs
        }
        
        // Example:
        //   (sub1 5)
        Expr::UnOp(Op1::Sub1, subexpr) => {
            let mut instrs = compile_to_instrs(subexpr, si, env, label_count, break_target);
            instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
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
        //   (< 3 5)
        Expr::BinOp(Op2::Less, left, right) => {
            let left_offset = -8 * si;
            let false_label = new_label(label_count, "less_false");
            let end_label = new_label(label_count, "less_end");

            let mut instrs = compile_to_instrs(left, si, env, label_count, break_target);

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));

            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));

            instrs.extend(compile_to_instrs(right, si + 1, env, label_count, break_target));

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));

            instrs.push(Instr::ICmp(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));
            instrs.push(Instr::IJge(false_label.clone()));

            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            instrs.push(Instr::IJmp(end_label.clone()));

            instrs.push(Instr::ILabel(false_label));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

            instrs.push(Instr::ILabel(end_label));

            instrs
        }

        // Example:
        //   (> 5 3)
        Expr::BinOp(Op2::Greater, left, right) => {
            let left_offset = -8 * si;
            let false_label = new_label(label_count, "greater_false");
            let end_label = new_label(label_count, "greater_end");

            let mut instrs = compile_to_instrs(left, si, env, label_count, break_target);

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));

            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));

            instrs.extend(compile_to_instrs(right, si + 1, env, label_count, break_target));

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));

            instrs.push(Instr::ICmp(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));
            instrs.push(Instr::IJle(false_label.clone()));

            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            instrs.push(Instr::IJmp(end_label.clone()));

            instrs.push(Instr::ILabel(false_label));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

            instrs.push(Instr::ILabel(end_label));

            instrs
        }

                // Example:
        //   (>= 5 5)
        Expr::BinOp(Op2::GreaterEq, left, right) => {
            let left_offset = -8 * si;
            let false_label = new_label(label_count, "greatereq_false");
            let end_label = new_label(label_count, "greatereq_end");

            let mut instrs = compile_to_instrs(left, si, env, label_count, break_target);

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));

            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));

            instrs.extend(compile_to_instrs(right, si + 1, env, label_count, break_target));

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));

            instrs.push(Instr::ICmp(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));
            instrs.push(Instr::IJl(false_label.clone()));

            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            instrs.push(Instr::IJmp(end_label.clone()));

            instrs.push(Instr::ILabel(false_label));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

            instrs.push(Instr::ILabel(end_label));

            instrs
        }

        // Example:
        //   (<= 3 3)
        Expr::BinOp(Op2::LessEq, left, right) => {
            let left_offset = -8 * si;
            let false_label = new_label(label_count, "lesseq_false");
            let end_label = new_label(label_count, "lesseq_end");

            let mut instrs = compile_to_instrs(left, si, env, label_count, break_target);

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));

            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));

            instrs.extend(compile_to_instrs(right, si + 1, env, label_count, break_target));

            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));

            instrs.push(Instr::ICmp(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));
            instrs.push(Instr::IJg(false_label.clone()));

            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            instrs.push(Instr::IJmp(end_label.clone()));

            instrs.push(Instr::ILabel(false_label));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

            instrs.push(Instr::ILabel(end_label));

            instrs
        }

                // Example:
        //   (= 4 4)
        Expr::BinOp(Op2::Equal, left, right) => {
            let left_offset = -8 * si;
            let false_label = new_label(label_count, "equal_false");
            let end_label = new_label(label_count, "equal_end");

            let mut instrs = compile_to_instrs(left, si, env, label_count, break_target);

            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));

            instrs.extend(compile_to_instrs(right, si + 1, env, label_count, break_target));

            // Check that both operands have the same tag
            instrs.push(Instr::IMov(
                Val::Reg(Reg::RBX),
                Val::RegOffset(Reg::RSP, left_offset),
            ));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));

            instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RDI), Val::Imm(1)));

            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI)));
            instrs.push(Instr::IJne("error".to_string()));

            instrs.push(Instr::ICmp(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));
            instrs.push(Instr::IJne(false_label.clone()));

            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            instrs.push(Instr::IJmp(end_label.clone()));

            instrs.push(Instr::ILabel(false_label));
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

            instrs.push(Instr::ILabel(end_label));

            instrs
        }

        // Example:
        //   (+ 4 5)
        Expr::BinOp(Op2::Plus, left, right) => {
            let stack_offset = -8 * si;
        
            let mut instrs = compile_to_instrs(left, si, env, label_count, break_target);
        
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));
        
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
        
            instrs.extend(compile_to_instrs(right, si + 1, env, label_count, break_target));
        
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));
        
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
            let mut instrs = compile_to_instrs(left, si, env, label_count, break_target);
            
            // Check that the left operand is a number
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));
            
            // Save the left operand on the stack
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));

            // Compile the right operand with the next available stack slot
            instrs.extend(compile_to_instrs(right, si + 1, env, label_count, break_target));
            
            // Check that the right operand is a number
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));
           
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
            let mut instrs = compile_to_instrs(left, si, env, label_count, break_target);
            
            // Check that the left operand is a number
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));
           
            // Save the left operand on the stack
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));

            // Compile the right operand with the next available stack slot
            instrs.extend(compile_to_instrs(right, si + 1, env, label_count, break_target));

            // Check that the right operand is a number
            instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
            instrs.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(0)));
            instrs.push(Instr::IJne("error".to_string()));
          
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
                instrs.extend(compile_to_instrs(expr, curr_si, &new_env, label_count, break_target));
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
            instrs.extend(compile_to_instrs(body, curr_si, &new_env, label_count, break_target));
            instrs
        }
    }
}

// ============= Code Generation =============

/// Convert a Val to its assembly string representation
fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => String::from("rax"),
        Val::Reg(Reg::RBX) => String::from("rbx"),
        Val::Reg(Reg::RSP) => String::from("rsp"),
        Val::Reg(Reg::RDI) => String::from("rdi"),
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
        Val::RegOffset(Reg::RBX, offset) if *offset < 0 => {
            format!("[rbx - {}]", -offset)
        }
        Val::RegOffset(Reg::RBX, offset) => {
            format!("[rbx + {}]", offset)
        }
        Val::RegOffset(Reg::RDI, offset) if *offset < 0 => {
            format!("[rdi - {}]", -offset)
        }
        Val::RegOffset(Reg::RDI, offset) => {
            format!("[rdi + {}]", offset)
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
        Instr::ICmp(left, right) => format!("cmp {}, {}", val_to_str(left), val_to_str(right)),
        Instr::IJe(label) => format!("je {}", label),
        Instr::IJmp(label) => format!("jmp {}", label),
        Instr::IAnd(dst, src) => format!("and {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IJne(label) => format!("jne {}", label),
        Instr::IJg(label) => format!("jg {}", label),
        Instr::IJge(label) => format!("jge {}", label),
        Instr::IJl(label) => format!("jl {}", label),
        Instr::IJle(label) => format!("jle {}", label),
        Instr::ILabel(label) => format!("{}:", label),
    }   
}

/// Compile an expression to a complete assembly string
fn compile(e: &Expr) -> String {
    // Start with an empty environment
    let env: HashMap<String, i32> = HashMap::new();
    let mut label_count = 0;
    let break_target = None;

    // Compile starting at stack slot 2
    let instrs = compile_to_instrs(e, 2, &env, &mut label_count, &break_target);

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
    // Wrap instructions in assembly program template
    let asm_program = format!(
        "section .text
    extern snek_error
    global our_code_starts_here
    our_code_starts_here:
    {}
    ret

    error:
    mov rdi, 1
    call snek_error
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
// These tests verify parsing, compilation, and key error cases
// for the Cobra language implementation.

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to parse a string directly
    fn parse_str(s: &str) -> Expr {
        parse_expr(&parse(s).unwrap())
    }

    // Helper to compile an expression with a fresh environment
    fn compile_expr_for_test(expr: &Expr) -> Vec<Instr> {
        let env: HashMap<String, i32> = HashMap::new();
        let mut label_count = 0;
        let break_target = None;
        compile_to_instrs(expr, 2, &env, &mut label_count, &break_target)
    }

    // ===== Parsing Tests =====

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

    #[test]
    fn test_parse_true() {
        let expr = parse_str("true");
        match expr {
            Expr::Bool(true) => (),
            _ => panic!("Expected Bool(true), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_false() {
        let expr = parse_str("false");
        match expr {
            Expr::Bool(false) => (),
            _ => panic!("Expected Bool(false), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_input() {
        let expr = parse_str("input");
        match expr {
            Expr::Input => (),
            _ => panic!("Expected Input, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_if() {
        let expr = parse_str("(if true 5 10)");
        match expr {
            Expr::If(_, _, _) => (),
            _ => panic!("Expected If, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_nested_if() {
        let expr = parse_str("(if true (if false 1 2) 3)");
        match expr {
            Expr::If(_, then_expr, _) => match *then_expr {
                Expr::If(_, _, _) => (),
                _ => panic!("Expected nested If in then branch, got {:?}", then_expr),
            },
            _ => panic!("Expected If, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_block() {
        let expr = parse_str("(block 1 2 3)");
        match expr {
            Expr::Block(exprs) => assert_eq!(exprs.len(), 3),
            _ => panic!("Expected Block, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_loop() {
        let expr = parse_str("(loop (break 5))");
        match expr {
            Expr::Loop(_) => (),
            _ => panic!("Expected Loop, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_break() {
        let expr = parse_str("(break 5)");
        match expr {
            Expr::Break(_) => (),
            _ => panic!("Expected Break, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_set() {
        let expr = parse_str("(set! x 5)");
        match expr {
            Expr::Set(name, _) => assert_eq!(name, "x"),
            _ => panic!("Expected Set, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_isnum() {
        let expr = parse_str("(isnum 5)");
        match expr {
            Expr::UnOp(Op1::IsNum, _) => (),
            _ => panic!("Expected UnOp(IsNum, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_isbool() {
        let expr = parse_str("(isbool true)");
        match expr {
            Expr::UnOp(Op1::IsBool, _) => (),
            _ => panic!("Expected UnOp(IsBool, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_less() {
        let expr = parse_str("(< 1 2)");
        match expr {
            Expr::BinOp(Op2::Less, _, _) => (),
            _ => panic!("Expected BinOp(Less, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_greater() {
        let expr = parse_str("(> 3 2)");
        match expr {
            Expr::BinOp(Op2::Greater, _, _) => (),
            _ => panic!("Expected BinOp(Greater, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_lesseq() {
        let expr = parse_str("(<= 3 3)");
        match expr {
            Expr::BinOp(Op2::LessEq, _, _) => (),
            _ => panic!("Expected BinOp(LessEq, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_greatereq() {
        let expr = parse_str("(>= 4 3)");
        match expr {
            Expr::BinOp(Op2::GreaterEq, _, _) => (),
            _ => panic!("Expected BinOp(GreaterEq, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_equal() {
        let expr = parse_str("(= 4 4)");
        match expr {
            Expr::BinOp(Op2::Equal, _, _) => (),
            _ => panic!("Expected BinOp(Equal, ...), got {:?}", expr),
        }
    }

    // ===== Error Tests =====

    #[test]
    #[should_panic(expected = "Duplicate binding")]
    fn test_duplicate_binding() {
        let expr = parse_str("(let ((x 1) (x 2)) x)");
        let env: HashMap<String, i32> = HashMap::new();
        let mut label_count = 0;
        let break_target = None;
        compile_to_instrs(&expr, 2, &env, &mut label_count, &break_target);
    }

    #[test]
    #[should_panic(expected = "Unbound variable identifier y")]
    fn test_unbound_variable() {
        let expr = parse_str("y");
        let env: HashMap<String, i32> = HashMap::new();
        let mut label_count = 0;
        let break_target = None;
        compile_to_instrs(&expr, 2, &env, &mut label_count, &break_target);
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

    #[test]
    #[should_panic(expected = "break outside loop")]
    fn test_break_outside_loop() {
        let expr = parse_str("(break 5)");
        let env: HashMap<String, i32> = HashMap::new();
        let mut label_count = 0;
        let break_target = None;
        compile_to_instrs(&expr, 2, &env, &mut label_count, &break_target);
    }

    // ===== Compilation Tests =====

    #[test]
    fn test_compile_number() {
        let expr = Expr::Number(42);
        let instrs = compile_expr_for_test(&expr);
        assert_eq!(instrs.len(), 1);

        match &instrs[0] {
            Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(n)) => assert_eq!(*n, 84),
            other => panic!("Expected tagged number move into RAX, got {:?}", other),
        }
    }

    #[test]
    fn test_compile_true() {
        let expr = Expr::Bool(true);
        let instrs = compile_expr_for_test(&expr);
        assert_eq!(instrs.len(), 1);

        match &instrs[0] {
            Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(n)) => assert_eq!(*n, 3),
            other => panic!("Expected true to compile to 3, got {:?}", other),
        }
    }

    #[test]
    fn test_compile_false() {
        let expr = Expr::Bool(false);
        let instrs = compile_expr_for_test(&expr);
        assert_eq!(instrs.len(), 1);

        match &instrs[0] {
            Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(n)) => assert_eq!(*n, 1),
            other => panic!("Expected false to compile to 1, got {:?}", other),
        }
    }

    #[test]
    fn test_compile_if_emits_labels_and_jump() {
        let expr = parse_str("(if true 5 10)");
        let instrs = compile_expr_for_test(&expr);

        let has_conditional_jump = instrs.iter().any(|instr| matches!(instr, Instr::IJe(_)));
        let has_unconditional_jump = instrs.iter().any(|instr| matches!(instr, Instr::IJmp(_)));
        let label_count = instrs.iter().filter(|instr| matches!(instr, Instr::ILabel(_))).count();

        assert!(has_conditional_jump, "Expected if to emit a conditional jump");
        assert!(has_unconditional_jump, "Expected if to emit an unconditional jump");
        assert!(label_count >= 2, "Expected if to emit at least two labels");
    }

    #[test]
    fn test_compile_loop_emits_loop_labels() {
        let expr = parse_str("(loop (break 5))");
        let instrs = compile_expr_for_test(&expr);

        let jump_count = instrs.iter().filter(|instr| matches!(instr, Instr::IJmp(_))).count();
        let label_count = instrs.iter().filter(|instr| matches!(instr, Instr::ILabel(_))).count();

        assert!(jump_count >= 1, "Expected loop to emit a jump");
        assert!(label_count >= 2, "Expected loop to emit start and end labels");
    }

    #[test]
    fn test_compile_set_emits_store_to_stack() {
        let expr = parse_str("(let ((x 5)) (set! x 8))");
        let instrs = compile_expr_for_test(&expr);

        let store_count = instrs
            .iter()
            .filter(|instr| matches!(instr, Instr::IMov(Val::RegOffset(Reg::RSP, _), Val::Reg(Reg::RAX))))
            .count();

        assert!(store_count >= 2, "Expected let and set! to store into stack slots");
    }

    #[test]
    fn test_compile_plus_emits_type_checks() {
        let expr = parse_str("(+ true 5)");
        let instrs = compile_expr_for_test(&expr);

        let has_and = instrs.iter().any(|instr| matches!(instr, Instr::IAnd(_, _)));
        let has_error_jump = instrs.iter().any(|instr| {
            matches!(instr, Instr::IJne(label) if label == "error")
        });

        assert!(has_and, "Expected arithmetic compilation to emit tag checks");
        assert!(has_error_jump, "Expected arithmetic compilation to jump to error on bad types");
    }

    #[test]
    fn test_compile_equal_emits_type_check() {
        let expr = parse_str("(= true 5)");
        let instrs = compile_expr_for_test(&expr);

        let has_error_jump = instrs.iter().any(|instr| {
            matches!(instr, Instr::IJne(label) if label == "error")
        });

        assert!(has_error_jump, "Expected equality compilation to reject mismatched tags");
    }

    #[test]
    fn test_compile_mixed_numeric_boolean_operation_has_error_path() {
        let expr = parse_str("(< true 5)");
        let instrs = compile_expr_for_test(&expr);

        let has_error_jump = instrs.iter().any(|instr| {
            matches!(instr, Instr::IJne(label) if label == "error")
        });

        assert!(has_error_jump, "Expected mixed boolean/number comparison to emit an error jump");
    }
}