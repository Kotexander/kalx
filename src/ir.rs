use std::{ffi::CString, fmt::Display, rc::Rc};

use crate::{
    parser::{self, Block, DeclaredVars},
    tokenizer::{Id, Operation},
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
        code: Code,
    },
}
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
        }
    }
}

#[derive(Debug, Clone)]
pub struct Code(pub Vec<Instruction>);
impl Code {
    fn new() -> Self {
        Self(vec![])
    }
}
impl Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction_group in self.0.iter() {
            writeln!(f, "{instruction_group}")?;
        }
        std::fmt::Result::Ok(())
    }
}

fn generate_temp_expr(code: &mut Code, expr: &parser::Expression, temp: &mut u32) -> Value {
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

fn generate_expr(code: &mut Code, expr: &parser::Expression, target: Value, temp: &mut u32) {
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

fn generate_instruction(code: &mut Code, instruction: parser::Instruction, temp: &mut u32) {
    match instruction {
        parser::Instruction::Declare { id, value } | parser::Instruction::Assign { id, value } => {
            generate_expr(code, &value, Value::Id(id.clone()), temp);
        }
        parser::Instruction::Expr(expr) => {
            generate_expr(code, &expr, Value::Temp(0), temp);
        }
        parser::Instruction::Loop(_) => todo!(),
        parser::Instruction::While { expr: _, block: _ } => todo!(),
        parser::Instruction::Block(block) => {
            generate_block(code, Rc::into_inner(block).unwrap(), temp);
        }
    }
}

fn generate_block(code: &mut Code, block: Block, temp: &mut u32) {
    let mut block_code = Code::new();
    for instruction in block.instructions.into_iter() {
        generate_instruction(&mut block_code, Rc::into_inner(instruction).unwrap(), temp);
    }
    code.0.push(Instruction::Block {
        decl_vars: block.vars,
        code: block_code,
    });
}

pub fn generate(block: Block) -> Code {
    let mut code = Code::new();
    generate_block(&mut code, block, &mut 0);
    code
}
