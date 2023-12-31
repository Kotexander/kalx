use super::*;

use std::ffi::CString;

type RelStr = (Rc<CString>, Vec<u32>);
pub struct Strings(pub Vec<RelStr>);
impl Strings {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn contains(&self, string: &Rc<CString>) -> bool {
        self.0.iter().any(|(s, _)| *s == *string)
    }
    pub fn add_str(&mut self, string: &Rc<CString>) {
        if !self.contains(string) {
            self.0.push((string.clone(), vec![]));
        }
    }
    pub fn get_mut(&mut self, string: &Rc<CString>) -> Option<&mut Vec<u32>> {
        self.0
            .iter_mut()
            .find(|(s, _)| *s == *string)
            .map(|(_, x)| x)
    }
    pub fn add_rel(&mut self, string: &Rc<CString>, offset: u32) {
        self.add_str(string);
        let relstr = self.get_mut(string).unwrap();
        relstr.push(offset);
    }
}

pub type RelFun = (Rc<String>, Vec<u32>);
pub struct Functions(pub Vec<RelFun>);
impl Functions {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn contains(&self, function: &Rc<String>) -> bool {
        self.0.iter().any(|(f, _)| *f == *function)
    }
    pub fn add_fun(&mut self, function: &Rc<String>) {
        if !self.contains(function) {
            self.0.push((function.clone(), vec![]));
        }
    }
    pub fn get_mut(&mut self, function: &Rc<String>) -> Option<&mut Vec<u32>> {
        self.0
            .iter_mut()
            .find(|(s, _)| *s == *function)
            .map(|(_, x)| x)
    }
    pub fn add_rel(&mut self, function: &Rc<String>, offset: u32) {
        self.add_fun(function);
        let relfun = self.get_mut(function).unwrap();
        relfun.push(offset);
    }
}
pub struct RelInfo {
    pub strs: Strings,
    pub funs: Functions,
}
impl RelInfo {
    pub fn new() -> Self {
        Self {
            strs: Strings::new(),
            funs: Functions::new(),
        }
    }
}

type Vars = HashMap<Rc<String>, (Type, i32)>;

pub fn generate_info(info: AnalysisInfo) -> (i32, Vars) {
    let mut vars = Vars::new();

    let mut alloc = 0;
    for (id, var) in info.declared_vars {
        alloc -= match var.typ {
            Type::String => 4,
            Type::U32 => 4,
            Type::Bool => 1,
            Type::Void => continue,
        };
        vars.insert(id, (var.typ, alloc));
    }

    (alloc, vars)
}

#[derive(Debug, Clone, Copy)]
#[allow(clippy::upper_case_acronyms)]
pub enum Register {
    EAX,
    EBX,
    ECX,
    EDX,
}
impl Register {
    fn next(&self) -> Self {
        match self {
            Self::EAX => Self::EBX,
            Self::EBX => Self::ECX,
            Self::ECX => Self::EDX,
            Self::EDX =>  panic!("end of the line for usable registers"),
        }
    }
}
#[allow(clippy::from_over_into)]
impl Into<RM32> for Register {
    fn into(self) -> RM32 {
        match self {
            Self::EAX => RM32::EAX,
            Self::EBX => RM32::EBX,
            Self::ECX => RM32::ECX,
            Self::EDX => RM32::EDX,
        }
    }
}
#[allow(clippy::from_over_into)]
impl Into<Reg32> for Register {
    fn into(self) -> Reg32 {
        match self {
            Self::EAX => Reg32::EAX,
            Self::EBX => Reg32::EBX,
            Self::ECX => Reg32::ECX,
            Self::EDX => Reg32::EDX,
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub enum Intent {
    Load,
    Op(Operation),
}

pub fn mov_reg_imm<E: Endian>(program: &mut Program<E>, reg: Register, num: u32) -> u32 {
    match reg {
        Register::EAX => program.mov_eax_imm32(num),
        Register::EBX => program.mov_ebx_imm32(num),
        Register::ECX => program.mov_ecx_imm32(num),
        Register::EDX => program.mov_edx_imm32(num),
    }
}

pub fn generate_expr<E: Endian>(
    program: &mut Program<E>,
    intent: Intent,
    register: Register,
    expr: &Expression,
    vars: &Vars,
    rel_info: &mut RelInfo,
) {
    match expr {
        Expression::Number(num) => match intent {
            Intent::Load => {
                mov_reg_imm(program, register, *num);
            }
            Intent::Op(op) => match op {
                Operation::Add => {
                    if let Register::EAX = register {
                        program.add_eax_imm32(*num);
                    } else {
                        program.add_rm_imm32(register.into(), *num);
                    }
                }
                Operation::Sub => {
                    if let Register::EAX = register {
                        program.sub_eax_imm32(*num);
                    } else {
                        program.sub_rm_imm32(register.into(), *num);
                    }
                }
                Operation::Mul => {
                    program.mul_r_rm_imm32(register.into(), register.into(), *num);
                }
                Operation::Div => {
                    todo!();
                }
                _ => todo!(),
            },
        },
        Expression::String(string) => match intent {
            Intent::Load => {
                let rel = mov_reg_imm(program, register, 0);
                rel_info.strs.add_rel(string, rel);
            }
            Intent::Op(_) => {
                todo!();
            }
        },
        Expression::Ident(id) => {
            let (typ, addr) = vars.get(id).unwrap();
            match typ {
                Type::String => match intent {
                    Intent::Load => {
                        program.mov_r_rm8(register.into(), RM32::EBP, (*addr).try_into().unwrap())
                    }
                    Intent::Op(_op) => {
                        todo!();
                    }
                },
                Type::U32 => match intent {
                    Intent::Load => {
                        program.mov_r_rm8(register.into(), RM32::EBP, (*addr).try_into().unwrap());
                    }
                    Intent::Op(op) => match op {
                        Operation::Add => {
                            program.add_r_rm8(
                                register.into(),
                                RM32::EBP,
                                (*addr).try_into().unwrap(),
                            );
                        }
                        Operation::Sub => {
                            program.sub_r_rm8(
                                register.into(),
                                RM32::EBP,
                                (*addr).try_into().unwrap(),
                            );
                        }
                        Operation::Mul => {
                            program.mul_r_rm8(
                                register.into(),
                                RM32::EBP,
                                (*addr).try_into().unwrap(),
                            );
                        }
                        Operation::Div => {
                            todo!();
                        }

                        _ => todo!(),
                    },
                },
                Type::Bool => todo!(),
                Type::Void => todo!(),
            }
        }
        Expression::Operation { lhs, op, rhs } => {
            let lhs_reg = register;
            let rhs_reg = register.next();
            generate_expr(program, Intent::Load, lhs_reg, lhs, vars, rel_info);
            generate_expr(program, Intent::Load, rhs_reg, rhs, vars, rel_info);
            match op {
                Operation::Add => {
                    program.add_rm_r(lhs_reg.into(), rhs_reg.into());
                }
                Operation::Sub => {
                    program.sub_rm_r(lhs_reg.into(), rhs_reg.into());
                }
                Operation::Mul => {
                    program.mul_r_rm(lhs_reg.into(), rhs_reg.into());
                }
                Operation::Div => todo!(),

                _ => todo!(),
            }
            // match intent {
            //     Intent::Load => {
            //         // let lhs_reg = register;
            //         generate_expr(program, Intent::Load, register, lhs, vars, rel_info);
            //         match op {
            //             Operation::Add | Operation::Sub => {
            //                 generate_expr(program, Intent::Op(*op), register, rhs, vars, rel_info);
            //                 // program.add_rm_r(lhs_reg.into(), rhs_reg.into());
            //             }
            //             Operation::Mul => {
            //                 let rhs_reg = register.next();
            //                 generate_expr(program, Intent::Load, rhs_reg, rhs, vars, rel_info);
            //                 program.mul_r_rm(register.into(), rhs_reg.into());
            //             }
            //             Operation::Div => todo!(),

            //             _ => todo!(),
            //         }
            //     }
            //     Intent::Op(intent_op) => {
            //         generate_expr(program, Intent::Load, register.next(), expr, vars, rel_info);
            //         todo!()
            //     },
            // }
        }
        Expression::Index { expr: _, index: _ } => todo!(),
        Expression::Function { name, args } => {
            match name.as_str() {
                "exit" => {
                    generate_expr(
                        program,
                        Intent::Load,
                        Register::EBX,
                        &args[0],
                        vars,
                        rel_info,
                    );
                    program.exit();
                }
                _ => {
                    // TODO: account for size
                    let mut stack = 0;
                    for arg in args.iter().rev() {
                        generate_expr(program, Intent::Load, Register::EAX, arg, vars, rel_info);
                        program.push_eax();
                        stack += 4;
                    }
                    let rel = program.call_rel32(-4);
                    rel_info.funs.add_rel(name, rel);
                    program.add_esp_imm8(stack);
                }
            }
        }
    }
}

pub fn generate_block<E: Endian>(
    program: &mut Program<E>,
    block: &Block,
    vars: &Vars,
    rel_info: &mut RelInfo,
) {
    for instruction in block.iter() {
        generate_instruction(program, instruction, vars, rel_info);
    }
}

pub fn generate_instruction<E: Endian>(
    program: &mut Program<E>,
    instruction: &Instruction,
    vars: &Vars,
    rel_info: &mut RelInfo,
) {
    match instruction {
        Instruction::Declare { id, value } | Instruction::Assign { id, value } => {
            let (typ, addr) = vars.get(id).unwrap();
            match typ {
                Type::String => {
                    generate_expr(program, Intent::Load, Register::EAX, value, vars, rel_info);
                    program.mov_rm8_r(RM32::EBP, Reg32::EAX, (*addr).try_into().unwrap());
                }
                Type::U32 => {
                    generate_expr(program, Intent::Load, Register::EAX, value, vars, rel_info);
                    program.mov_rm8_r(RM32::EBP, Reg32::EAX, (*addr).try_into().unwrap());
                }
                Type::Bool => todo!(),
                Type::Void => todo!(),
            }
        }
        Instruction::Expr(expr) => {
            generate_expr(program, Intent::Load, Register::EAX, expr, vars, rel_info);
        }
        Instruction::Loop(block) => {
            program.loop_fn(|program| {
                for instruction in block.iter() {
                    generate_instruction(program, instruction, vars, rel_info);
                }
            });
        }
        Instruction::While { expr, block } => {
            let addr_start = program.jmp_rel(0);
            let start = program.code.len();
            // body
            for instruction in block.iter() {
                generate_instruction(program, instruction, vars, rel_info);
            }
            let first_jmp_end = program.code.len();
            let rel: i8 = (first_jmp_end as i32 - start as i32).try_into().unwrap();
            program.code[addr_start as usize] = rel as u8;
            // cmp
            match &**expr {
                Expression::Operation { lhs, op, rhs } => {
                    let r_lhs = Register::EAX;
                    let r_rhs = Register::EBX;
                    generate_expr(program, Intent::Load, r_lhs, lhs, vars, rel_info);
                    generate_expr(program, Intent::Load, r_rhs, rhs, vars, rel_info);
                    program.cmp_r_rm(r_lhs.into(), r_rhs.into());
                    let jmp_rel = match op {
                        Operation::GTC => program.jg(0),
                        Operation::LTC => program.jl(0),
                        Operation::GTE => program.jge(0),
                        _ => todo!(),
                    };
                    let end = program.code.len();
                    let rel: i8 = (start as i32 - end as i32).try_into().unwrap();
                    program.code[jmp_rel as usize] = rel as u8;
                }
                _ => {
                    todo!();
                }
            }
        }
    }
}
