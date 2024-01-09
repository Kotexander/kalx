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
#[derive(Debug, Clone, Copy)]
pub struct LabelGen(u32);
impl LabelGen {
    fn new() -> Self {
        Self(0)
    }
    fn next(&mut self) -> u32 {
        let ret = self.0;
        self.0 += 1;
        ret
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
        code: Block,
    },
    Label(Label),
    Goto(Label),
    IfGoto {
        lhs: Value,
        op: BoolOperation,
        rhs: Value,
        dest: Label,
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
            Instruction::Label(l) => {
                write!(f, "L<{l}>:")
            }
            Instruction::Goto(l) => {
                write!(f, "goto<{l}>;")
            }
            Instruction::IfGoto {
                lhs,
                op,
                rhs,
                dest: good,
            } => {
                let op = Operation::Bool(*op);
                write!(f, "if ({lhs} {op} {rhs}) {}", Self::Goto(*good),)
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

#[derive(Debug, Clone, Copy)]
struct BoolDest {
    good: Label,
    bad: Label,
}
impl BoolDest {
    fn swap(self) -> Self {
        Self {
            good: self.bad,
            bad: self.good,
        }
    }
}

fn generate_bool_expr(
    code: &mut Block,
    expr: &parser::Expression,
    dest: BoolDest,
    temp: &mut u32,
    label: &mut LabelGen,
    not: bool,
    add_bad: bool,
) {
    if let parser::Expression::Operation { lhs, op, rhs } = expr {
        if let Operation::Bool(bop) = op {
            match (lhs.is_bool(), rhs.is_bool()) {
                (true, true) => match bop {
                    BoolOperation::And => {
                        generate_bool_expr(code, lhs, dest, temp, label, true, false);
                        generate_bool_expr(code, rhs, dest, temp, label, true, add_bad);
                    }
                    BoolOperation::Or_ => {
                        let lhs_is_next_bool = lhs.is_next_bool();
                        let dest_lhs = if lhs_is_next_bool {
                            let bad = label.next();
                            BoolDest {
                                good: dest.good,
                                bad,
                            }
                        } else {
                            dest
                        };
                        generate_bool_expr(
                            code,
                            lhs,
                            dest_lhs,
                            temp,
                            label,
                            false,
                            lhs_is_next_bool,
                        );
                        if lhs_is_next_bool {
                            code.0.push(Instruction::Label(dest_lhs.bad));
                        }
                        generate_bool_expr(code, rhs, dest, temp, label, false, rhs.is_next_bool());
                    }
                    BoolOperation::NNd => todo!(),
                    BoolOperation::Nor => todo!(),
                    _ => panic!(),
                },
                (false, false) => {
                    let lhs_value = generate_temp_expr(code, lhs, temp);
                    let rhs_value = generate_temp_expr(code, rhs, temp);
                    let (bop, dest) = if not {
                        (bop.not(), dest.swap())
                    } else {
                        (*bop, dest)
                    };
                    code.0.push(Instruction::IfGoto {
                        lhs: lhs_value,
                        op: bop,
                        rhs: rhs_value,
                        dest: dest.good,
                    });
                    if add_bad {
                        code.0.push(Instruction::Goto(dest.bad));
                    }
                }
                _ => panic!(),
            }
        } else {
            panic!()
        }
    } else {
        panic!()
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
    label: &mut LabelGen,
) {
    match instruction {
        parser::Instruction::Declare { id, value } | parser::Instruction::Assign { id, value } => {
            generate_expr(code, &value, Value::Id(id.clone()), temp);
        }
        parser::Instruction::Expr(expr) => {
            generate_expr(code, &expr, Value::Temp(0), temp);
        }
        parser::Instruction::Loop(block) => {
            let l = label.next();
            code.0.push(Instruction::Label(l));
            generate_block(code, Rc::into_inner(block).unwrap(), temp, label);
            code.0.push(Instruction::Goto(l));
        }
        parser::Instruction::While { expr, block } => {
            let skip_l = label.next();
            let good_l = label.next();
            let bad_l = label.next();

            code.0.push(Instruction::Goto(skip_l));
            code.0.push(Instruction::Label(good_l));
            generate_block(code, Rc::into_inner(block).unwrap(), temp, label);
            code.0.push(Instruction::Label(skip_l));
            generate_bool_expr(
                code,
                &expr,
                BoolDest {
                    good: good_l,
                    bad: bad_l,
                },
                temp,
                label,
                false,
                true,
            );
            code.0.push(Instruction::Label(bad_l));
        }
        parser::Instruction::Block(block) => {
            generate_block(code, Rc::into_inner(block).unwrap(), temp, label);
        }
        parser::Instruction::If { expr, block } => {
            let good_l = label.next();
            let bad_l = label.next();

            generate_bool_expr(
                code,
                &expr,
                BoolDest {
                    good: good_l,
                    bad: bad_l,
                },
                temp,
                label,
                false,
                true,
            );
            code.0.push(Instruction::Label(good_l));
            generate_block(code, Rc::into_inner(block).unwrap(), temp, label);
            code.0.push(Instruction::Label(bad_l));
        },
        
    }
}

fn generate_block(code: &mut Block, block: parser::Block, temp: &mut u32, label: &mut LabelGen) {
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
    let mut label = LabelGen::new();
    generate_block(&mut code, block, &mut 0, &mut label);
    code
}
