use super::*;

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

pub fn mov_reg_imm<E: Endian>(program: &mut Program<E>, reg: Register, num: u32) -> u32 {
    match reg {
        Register::EAX => program.mov_eax_imm(num),
        Register::EBX => program.mov_ebx_imm(num),
        Register::ECX => program.mov_ecx_imm(num),
        Register::EDX => program.mov_edx_imm(num),
    }
}

pub fn generate_expr<E: Endian>(
    program: &mut Program<E>,
    intent: Intent,
    register: Register,
    expr: &Expression,
    vars: &Vars,
    strings: &mut Strings,
) {
    match expr {
        Expression::Number(num) => match intent {
            Intent::Load => {
                mov_reg_imm(program, register, *num);
            }
            Intent::Add => match register {
                Register::EAX => {
                    program.add_eax_imm(*num);
                }
                _ => {
                    // TODO: replace with register specific instructions
                    program.add_rm_imm(register.into(), *num);
                }
            },
            Intent::Sub => {
                // TODO: replace with register specific instructions
                program.sub_rm_imm(register.into(), *num);
            }
        },
        Expression::String(string) => match intent {
            Intent::Load => {
                let (rel_str, _addr) = strings.get_mut(string).unwrap();
                let rel = mov_reg_imm(program, register, 0);
                rel_str.add(rel);
            }
            Intent::Add => todo!(),
            Intent::Sub => todo!(),
        },
        Expression::Ident(id) => {
            let (typ, addr) = vars.get(id).unwrap();
            match typ {
                Type::String => match intent {
                    Intent::Load => {
                        program.mov_r_rm8(register.into(), RM32::EBP, (*addr).try_into().unwrap())
                    }
                    Intent::Add => todo!(),
                    Intent::Sub => todo!(),
                },
                Type::U32 => match intent {
                    Intent::Load => {
                        program.mov_r_rm8(register.into(), RM32::EBP, (*addr).try_into().unwrap());
                    }
                    Intent::Add => {
                        program.add_r_rm8(register.into(), RM32::EBP, (*addr).try_into().unwrap());
                    }
                    Intent::Sub => {
                        program.sub_r_rm8(register.into(), RM32::EBP, (*addr).try_into().unwrap());
                    }
                },
                Type::Bool => todo!(),
                Type::Void => todo!(),
            }
        }
        Expression::Operation { lhs, op, rhs } => match intent {
            Intent::Load => {
                generate_expr(program, Intent::Load, register, lhs, vars, strings);
                match op {
                    Operation::Add => {
                        generate_expr(program, Intent::Add, register, rhs, vars, strings);
                    }
                    Operation::Sub => {
                        generate_expr(program, Intent::Sub, register, rhs, vars, strings);
                    }
                    Operation::Mul => todo!(),
                    Operation::Div => todo!(),
                    Operation::GTC => todo!(),
                    Operation::LTC => todo!(),
                }
            }
            Intent::Add => {
                generate_expr(program, Intent::Add, register, lhs, vars, strings);
                match op {
                    Operation::Add => {
                        generate_expr(program, Intent::Add, register, rhs, vars, strings);
                    }
                    Operation::Sub => {
                        generate_expr(program, Intent::Sub, register, rhs, vars, strings);
                    }
                    Operation::Mul => todo!(),
                    Operation::Div => todo!(),
                    Operation::GTC => todo!(),
                    Operation::LTC => todo!(),
                }
            }
            Intent::Sub => {
                generate_expr(program, Intent::Sub, register, lhs, vars, strings);
                match op {
                    Operation::Add => {
                        generate_expr(program, Intent::Sub, register, rhs, vars, strings);
                    }
                    Operation::Sub => {
                        generate_expr(program, Intent::Add, register, rhs, vars, strings);
                    }
                    Operation::Mul => todo!(),
                    Operation::Div => todo!(),
                    Operation::GTC => todo!(),
                    Operation::LTC => todo!(),
                }
            }
        },
        Expression::Index { expr: _, index: _ } => todo!(),
        Expression::Function { name, args } => {
            match &name[..] {
                "exit" => {
                    generate_expr(program, Intent::Load, Register::EBX, &args[0], vars, strings);
                    program.exit();
                }
                // "print" => {
                //     match &*(args[0]) {
                //         Expression::String(string) => {
                //             let (rel_str, _addr) = strings.get_mut(string).unwrap();
                //             let rel =
                //                 program.write_const(0, string.as_bytes_with_nul().len() as u32);
                //             rel_str.add(rel);
                //         }
                //         Expression::Ident(id) => {
                //             let (typ, addr) = vars.get(id).unwrap();
                //             if let Type::String = typ {
                //                 // program.write_const(0, 1);
                //                 program.write_rm8(RM32::EBP, (*addr).try_into().unwrap(), 5);
                //                 // todo!()
                //             } else {
                //                 todo!()
                //             }
                //         }
                //         Expression::Number(_) => todo!(),
                //         Expression::Operation {
                //             lhs: _,
                //             op: _,
                //             rhs: _,
                //         } => todo!(),
                //         Expression::Index { expr: _, index: _ } => todo!(),
                //         Expression::Function { name: _, args: _ } => todo!(),
                //     }
                // }
                _ => {
                    panic!("unknown function: {name}");
                }
            }
        }
    }
}

pub fn generate_block<E: Endian>(program: &mut Program<E>, block: &Block, vars: &Vars, strings: &mut Strings) {
    for instruction in block.iter() {
        generate_instruction(program, instruction, vars, strings);
    }
}

pub fn generate_instruction<E: Endian>(
    program: &mut Program<E>,
    instruction: &Instruction,
    vars: &Vars,
    strings: &mut Strings,
) {
    match instruction {
        Instruction::Declare { id, value } | Instruction::Assign { id, value } => {
            let (typ, addr) = vars.get(id).unwrap();
            match typ {
                Type::String => {
                    generate_expr(program, Intent::Load, Register::EAX, value, vars, strings);
                    program.mov_rm8_r(RM32::EBP, Reg32::EAX, (*addr).try_into().unwrap());
                }
                Type::U32 => {
                    generate_expr(program, Intent::Load, Register::EAX, value, vars, strings);
                    program.mov_rm8_r(RM32::EBP, Reg32::EAX, (*addr).try_into().unwrap());
                }
                Type::Bool => todo!(),
                Type::Void => todo!(),
            
            }
        }
        Instruction::Expr(expr) => {
            generate_expr(program, Intent::Load, Register::EAX, expr, vars, strings);
        }
        Instruction::Loop(block) => {
            program.loop_fn(|program| {
                for instruction in block.iter() {
                    generate_instruction(program, instruction, vars, strings);
                }
            });
        }
        Instruction::While { expr, block } => {
            program.jmp_rel(0);
            let start = program.code.len();
            // body
            for instruction in block.iter() {
                generate_instruction(program, instruction, vars, strings);
            }
            let first_jmp_end = program.code.len();
            let rel: i8 = (first_jmp_end as i32 - start as i32).try_into().unwrap();
            program.code[start - 1] = rel as u8;
            // cmp
            match &**expr {
                Expression::Operation { lhs, op, rhs } => {
                    let r_lhs = Register::EAX;
                    let r_rhs = Register::EBX;
                    generate_expr(program, Intent::Load, r_lhs, lhs, vars, strings);
                    generate_expr(program, Intent::Load, r_rhs, rhs, vars, strings);
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
