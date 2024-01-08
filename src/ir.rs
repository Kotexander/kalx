use std::{ffi::CString, fmt::Display, rc::Rc};

use crate::{
    parser::{self, DeclaredVars},
    tokenizer::{BoolOperation, Id, Operation},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Id(Id),
    Temp(u32),
    Num(u32),
    String(Rc<CString>),
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Id(id) => write!(f, "{id}"),
            Value::Temp(num) => write!(f, "_{num}"),
            Value::Num(num) => write!(f, "{num}"),
            Value::String(string) => write!(f, "{string:?}"),
        }
    }
}

pub type Label = u32;
#[derive(Debug, Clone)]
pub enum Instruction {
    Assign {
        target: Value,
        value: Value,
    },
    Op {
        target: Value,
        lhs: Value,
        op: Operation,
        rhs: Value,
    },
    Function {
        name: Id,
        args: Vec<Value>,
    },
    Block {
        decl_vars: DeclaredVars,
        code: Block,
    },
    Label(Label),
    Goto(Label),
    IfElseGoto {
        lhs: Value,
        op: BoolOperation,
        rhs: Value,
        good: Label,
        bad: Option<Label>,
    },
}
// impl Instruction {
// fn is_goto() {

// }
// }
impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Assign { target, value } => write!(f, "{target} = {value};"),
            Instruction::Op {
                target,
                lhs,
                op,
                rhs,
            } => write!(f, "{target} = {lhs} {op} {rhs};"),
            Instruction::Function { name, args } => {
                if !args.is_empty() {
                    let last = args.len() - 1;
                    write!(f, "{name}(")?;
                    for arg in args[..last].iter() {
                        write!(f, "{arg}, ")?;
                    }
                    write!(f, "{}", args[last])?;
                    write!(f, ");")
                } else {
                    write!(f, "{name}();")
                }
            }
            Instruction::Block {
                decl_vars: vars,
                code,
            } => {
                writeln!(f, "{{")?;
                for (id, typ) in vars.0.iter() {
                    writeln!(f, "\tvar {id}: {typ}")?;
                }
                writeln!(f)?;
                for group in code.0.iter() {
                    let instruction = format!("{group}");
                    for line in instruction.split('\n') {
                        writeln!(f, "\t{line}")?;
                    }
                }
                write!(f, "}}")
            }
            Instruction::Label(l) => {
                write!(f, "L<{l}>:")
            }
            Instruction::Goto(l) => {
                write!(f, "goto<{l}>;")
            }
            Instruction::IfElseGoto {
                lhs,
                op,
                rhs,
                good,
                bad,
            } => {
                let op = Operation::Bool(*op);
                if let Some(bad) = bad {
                    write!(
                        f,
                        "if {lhs} {op} {rhs} {} else {}",
                        Self::Goto(*good),
                        Self::Goto(*bad)
                    )
                }
                else {
                    write!(
                        f,
                        "if {lhs} {op} {rhs} {}",
                        Self::Goto(*good),
                    )
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Instruction>);
impl Block {
    fn new() -> Self {
        Self(vec![])
    }
}
impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction_group in self.0.iter() {
            writeln!(f, "{instruction_group}")?;
        }
        std::fmt::Result::Ok(())
    }
}

fn generate_temp_expr(code: &mut Block, expr: &parser::Expression, temp: &mut u32) -> Value {
    match expr {
        parser::Expression::Number(num) => Value::Num(*num),
        parser::Expression::String(string) => Value::String(string.clone()),
        parser::Expression::Ident(id) => Value::Id(id.clone()),
        parser::Expression::Operation { lhs, op, rhs } => {
            let lhs_value = generate_temp_expr(code, lhs, temp);
            let rhs_value = generate_temp_expr(code, rhs, temp);

            let value = Value::Temp(*temp);
            *temp += 1;
            code.0.push(Instruction::Op {
                target: value.clone(),
                lhs: lhs_value,
                op: *op,
                rhs: rhs_value,
            });
            value
        }
        parser::Expression::Index { expr: _, index: _ } => todo!(),
        parser::Expression::Function { name: _, args: _ } => {
            todo!()
        }
    }
}

struct BoolDest {
    good: Label,
    bad: Option<Label>,
}
fn generate_bool_expr(
    code: &mut Block,
    expr: &parser::Expression,
    bool_dest: &BoolDest,
    temp: &mut u32,
) {
    match expr {
        parser::Expression::Operation { lhs, op, rhs } => {
            if let Operation::Bool(bop) = op {
                let lhs_value = generate_temp_expr(code, lhs, temp);
                let rhs_value = generate_temp_expr(code, rhs, temp);
                code.0.push(Instruction::IfElseGoto {
                    lhs: lhs_value,
                    op: *bop,
                    rhs: rhs_value,
                    good: bool_dest.good,
                    bad: bool_dest.bad,
                });
            } else {
                panic!();
            }
        }
        parser::Expression::Number(_) => todo!(),
        parser::Expression::String(_) => todo!(),
        parser::Expression::Ident(_) => todo!(),
        parser::Expression::Index { .. } => todo!(),
        parser::Expression::Function { .. } => todo!(),
    }
}

fn generate_expr(code: &mut Block, expr: &parser::Expression, target: Value, temp: &mut u32) {
    match expr {
        parser::Expression::Number(num) => {
            code.0.push(Instruction::Assign {
                target,
                value: Value::Num(*num),
            });
        }
        parser::Expression::String(string) => {
            code.0.push(Instruction::Assign {
                target,
                value: Value::String(string.clone()),
            });
        }
        parser::Expression::Ident(id) => {
            code.0.push(Instruction::Assign {
                target,
                value: Value::Id(id.clone()),
            });
        }
        parser::Expression::Operation { lhs, op, rhs } => {
            let lhs_value = generate_temp_expr(code, lhs, temp);
            let rhs_value = generate_temp_expr(code, rhs, temp);
            code.0.push(Instruction::Op {
                target,
                lhs: lhs_value,
                op: *op,
                rhs: rhs_value,
            });
        }
        parser::Expression::Index { expr: _, index: _ } => todo!(),
        parser::Expression::Function { name, args } => {
            let mut arg_values = vec![];
            for arg in args.iter() {
                arg_values.push(generate_temp_expr(code, arg, temp));
            }

            code.0.push(Instruction::Function {
                name: name.clone(),
                args: arg_values,
            });
        }
    }
}

fn generate_instruction(
    code: &mut Block,
    instruction: parser::Instruction,
    temp: &mut u32,
    label: &mut Label,
) {
    match instruction {
        parser::Instruction::Declare { id, value } | parser::Instruction::Assign { id, value } => {
            generate_expr(code, &value, Value::Id(id.clone()), temp);
        }
        parser::Instruction::Expr(expr) => {
            generate_expr(code, &expr, Value::Temp(0), temp);
        }
        parser::Instruction::Loop(block) => {
            let l = *label;
            *label += 1;
            code.0.push(Instruction::Label(l));
            generate_block(code, Rc::into_inner(block).unwrap(), temp, label);
            code.0.push(Instruction::Goto(l));
        }
        parser::Instruction::While { expr, block } => {
            let skip_l = *label;
            *label += 1;
            let good_l = *label;
            *label += 1;
            // let bad_l = *label;
            // *label += 1;

            code.0.push(Instruction::Goto(skip_l));
            code.0.push(Instruction::Label(good_l));
            generate_block(code, Rc::into_inner(block).unwrap(), temp, label);
            code.0.push(Instruction::Label(skip_l));
            generate_bool_expr(
                code,
                &expr,
                &BoolDest {
                    good: good_l,
                    // bad: Some(bad_l),
                    bad: None,
                },
                temp,
            );
            // code.0.push(Instruction::Label(bad_l));
        }
        parser::Instruction::Block(block) => {
            generate_block(code, Rc::into_inner(block).unwrap(), temp, label);
        }
    }
}

fn generate_block(code: &mut Block, block: parser::Block, temp: &mut u32, label: &mut Label) {
    let mut block_code = Block::new();
    for instruction in block.instructions.into_iter() {
        generate_instruction(
            &mut block_code,
            Rc::into_inner(instruction).unwrap(),
            temp,
            label,
        );
    }
    code.0.push(Instruction::Block {
        decl_vars: block.vars,
        code: block_code,
    });
}

pub fn generate(block: parser::Block) -> Block {
    let mut code = Block::new();
    generate_block(&mut code, block, &mut 0, &mut 0);
    code
}
