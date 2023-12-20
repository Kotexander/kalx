use super::*;

#[derive(Debug, Clone, Copy)]
pub enum Var {
    Global,
    Local(i32),
}

#[derive(Debug, Clone, Copy)]
#[allow(clippy::upper_case_acronyms)]
pub enum Register {
    EAX,
    EBX,
    ECX,
    EDX,
}
#[allow(clippy::from_over_into)]
impl Into<RM32> for Register {
    fn into(self) -> RM32 {
        match self {
            Register::EAX => RM32::EAX,
            Register::EBX => RM32::EBX,
            Register::ECX => RM32::ECX,
            Register::EDX => RM32::EDX,
        }
    }
}
#[allow(clippy::from_over_into)]
impl Into<Reg32> for Register {
    fn into(self) -> Reg32 {
        match self {
            Register::EAX => Reg32::EAX,
            Register::EBX => Reg32::EBX,
            Register::ECX => Reg32::ECX,
            Register::EDX => Reg32::EDX,
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub enum Intent {
    Load,
    Add,
    Sub,
}

pub type VarTypes = HashMap<Rc<String>, Type>;
pub type VarAddresses = HashMap<Rc<String>, Var>;

pub fn analyse_expr(expr: &Expression, vars: &VarTypes) -> Type {
    match expr {
        Expression::Number(_) => Type::U32,
        Expression::String(_) => Type::String,
        Expression::Ident(id) => vars[id],
        Expression::Operation { lhs, op, rhs } => {
            let lhs = analyse_expr(lhs, vars);
            let rhs = analyse_expr(rhs, vars);
            assert_eq!(lhs, rhs);
            match op {
                Operation::GTC | Operation::LTC => Type::Bool,
                _ => lhs,
            }
        }
    }
}
pub fn analyse_instruction(instruction: &Instruction, vars: &mut VarTypes) {
    match instruction {
        Instruction::Declare { id, value } => {
            if vars.contains_key(id) {
                panic!("duplicate declaration");
            } else {
                let typ = analyse_expr(value, vars);
                vars.insert(id.clone(), typ);
            }
        }
        Instruction::Assign { id, value } => {
            if !vars.contains_key(id) {
                panic!("assignment before declaration");
            }
            analyse_expr(value, vars);
        }
        Instruction::Exit(expr) => {
            analyse_expr(expr, vars);
        }
        Instruction::Print(expr) => {
            analyse_expr(expr, vars);
        }
        Instruction::Binary { lhs, rhs } => {
            analyse_instruction(lhs, vars);
            analyse_instruction(rhs, vars);
        }
        Instruction::Loop(block) => {
            if let Some(block) = block {
                analyse_instruction(block, vars);
            }
        }
        Instruction::While { expr, block } => {
            let typ = analyse_expr(expr, vars);
            assert_eq!(typ, Type::Bool);
            if let Some(block) = block {
                analyse_instruction(block, vars);
            }
        }
    }
}

pub fn generate_expr<E: Endian>(
    program: &mut Program<E>,
    intent: Intent,
    register: Register,
    expr: &Expression,
    vars: &VarAddresses,
) {
    match expr {
        Expression::Number(num) => match intent {
            Intent::Load => match register {
                Register::EAX => {
                    program.mov_eax_imm(*num);
                }
                Register::EBX => {
                    program.mov_ebx_imm(*num);
                }
                _ => {
                    todo!()
                }
            },
            Intent::Add => match register {
                Register::EAX => {
                    program.add_eax_imm(*num);
                }
                _ => {
                    program.add_rm_imm(register.into(), *num);
                }
            },
            Intent::Sub => {
                program.sub_rm_imm(register.into(), *num);
            }
        },
        Expression::String(_) => todo!(),
        Expression::Ident(id) => match intent {
            Intent::Load => match vars[id] {
                Var::Global => todo!(),
                Var::Local(addr) => {
                    program.mov_r_rm8(register.into(), RM32::EBP, addr.try_into().unwrap());
                }
            },
            Intent::Add => match vars[id] {
                Var::Global => todo!(),
                Var::Local(addr) => {
                    program.add_r_rm8(register.into(), RM32::EBP, addr.try_into().unwrap());
                }
            },
            Intent::Sub => match vars[id] {
                Var::Global => todo!(),
                Var::Local(addr) => {
                    program.sub_r_rm8(register.into(), RM32::EBP, addr.try_into().unwrap());
                }
            },
        },
        Expression::Operation { lhs, op, rhs } => match intent {
            Intent::Load => {
                generate_expr(program, Intent::Load, register, lhs, vars);
                match op {
                    Operation::Add => {
                        generate_expr(program, Intent::Add, register, rhs, vars);
                    }
                    Operation::Sub => {
                        generate_expr(program, Intent::Sub, register, rhs, vars);
                    }
                    Operation::Mul => todo!(),
                    Operation::Div => todo!(),
                    Operation::GTC => todo!(),
                    Operation::LTC => todo!(),
                }
            }
            Intent::Add => {
                generate_expr(program, Intent::Add, register, lhs, vars);
                match op {
                    Operation::Add => {
                        generate_expr(program, Intent::Add, register, rhs, vars);
                    }
                    Operation::Sub => {
                        generate_expr(program, Intent::Sub, register, rhs, vars);
                    }
                    Operation::Mul => todo!(),
                    Operation::Div => todo!(),
                    Operation::GTC => todo!(),
                    Operation::LTC => todo!(),
                }
            }
            Intent::Sub => {
                generate_expr(program, Intent::Sub, register, lhs, vars);
                match op {
                    Operation::Add => {
                        generate_expr(program, Intent::Sub, register, rhs, vars);
                    }
                    Operation::Sub => {
                        generate_expr(program, Intent::Add, register, rhs, vars);
                    }
                    Operation::Mul => todo!(),
                    Operation::Div => todo!(),
                    Operation::GTC => todo!(),
                    Operation::LTC => todo!(),
                }
            }
        },
    }
}

pub fn generate_instruction<E: Endian>(
    program: &mut Program<E>,
    instruction: &Instruction,
    vars: &VarAddresses,
) {
    match instruction {
        Instruction::Declare { id, value } | Instruction::Assign { id, value } => match vars[id] {
            Var::Global => todo!(),
            Var::Local(addr) => {
                generate_expr(program, Intent::Load, Register::EAX, value, vars);
                program.mov_rm8_r(RM32::EBP, Reg32::EAX, addr.try_into().unwrap());
            }
        },
        Instruction::Exit(expr) => {
            generate_expr(program, Intent::Load, Register::EBX, expr, vars);
            program.exit();
        }
        Instruction::Print(_expr) => {
            todo!()
        }
        Instruction::Binary { lhs, rhs } => {
            generate_instruction(program, lhs, vars);
            generate_instruction(program, rhs, vars);
        }
        Instruction::Loop(block) => {
            program.loop_fn(|program| {
                if let Some(block) = block {
                    generate_instruction(program, block, vars);
                }
            });
        }
        Instruction::While { expr, block } => {
            program.jmp_rel(0);
            let start = program.code.len();
            // body
            if let Some(block) = block {
                generate_instruction(program, block, vars);
            }
            let first_jmp_end = program.code.len();
            let rel: i8 = (first_jmp_end as i32 - start as i32).try_into().unwrap();
            program.code[start - 1] = rel as u8;
            // cmp
            match &**expr {
                Expression::Operation { lhs, op, rhs } => {
                    let r_lhs = Register::EAX;
                    let r_rhs = Register::EBX;
                    generate_expr(program, Intent::Load, r_lhs, lhs, vars);
                    generate_expr(program, Intent::Load, r_rhs, rhs, vars);
                    program.cmp_r_rm(r_lhs.into(), r_rhs.into());
                    match op {
                        Operation::GTC => {
                            program.jg(0);
                        }
                        Operation::LTC => {
                            program.jl(0);
                        }
                        Operation::Add => todo!(),
                        Operation::Sub => todo!(),
                        Operation::Mul => todo!(),
                        Operation::Div => todo!(),
                    }
                    let end = program.code.len();
                    let rel: i8 = (start as i32 - end as i32).try_into().unwrap();
                    program.code[end - 1] = rel as u8;
                }
                _ => {
                    todo!();
                }
            }
        }
    }
}
