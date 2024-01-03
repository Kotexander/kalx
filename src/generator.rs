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

// TODO: use array instead of hashmap
type Vars = HashMap<Rc<String>, (Type, i32)>;

#[derive(Debug, Clone)]
pub struct AllocInfo {
    pub alloc: i32,
    pub temp_start: i32,
    pub vars: Vars,
}
pub fn generate_info(info: AnalysisInfo) -> AllocInfo {
    let mut vars = Vars::new();

    let mut alloc = 0;
    for (id, var) in info.declared_vars {
        alloc -= match var.typ {
            Type::Void => continue,
            _ => var.typ.size() as i32,
        };
        vars.insert(id, (var.typ, alloc));
    }
    let temp_start = alloc;
    let mut temp_alloc = 4 * (3 - 1); // 3 usable reg but reserve one for division
    for (typ, num) in info.temp_vars.0.iter() {
        temp_alloc -= typ.size() as i32 * (*num as i32);
    }
    temp_alloc = temp_alloc.min(0);

    alloc += temp_alloc;

    AllocInfo {
        alloc,
        vars,
        temp_start,
    }
}
#[derive(Debug, Clone, Copy)]
pub struct TempInfo {
    pub temp_start: i32,
    pub storage: TempStorage,
}
impl TempInfo {
    pub fn new(temp_start: i32) -> Self {
        Self {
            temp_start,
            storage: TempStorage::Register(Register::EBX),
        }
    }
    pub fn next(&self) -> Self {
        Self {
            temp_start: self.temp_start,
            storage: self.storage.next(self.temp_start),
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub enum TempStorage {
    Register(Register),
    Stack(i32),
}
impl TempStorage {
    pub fn next(&self, temp_start: i32) -> Self {
        match self {
            TempStorage::Register(reg) => match reg {
                Register::EAX => TempStorage::Register(Register::ECX),
                Register::ECX => TempStorage::Register(Register::EDX),
                Register::EDX => TempStorage::Register(Register::EBX),
                Register::EBX => TempStorage::Register(Register::ESI),
                Register::ESI => TempStorage::Register(Register::EDI),
                Register::EDI => TempStorage::Stack(temp_start - 4),
            },
            TempStorage::Stack(offset) => TempStorage::Stack(offset - 4),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub enum Register {
    EAX,
    ECX,
    EDX,
    EBX,
    ESI,
    EDI,
}
#[allow(clippy::from_over_into)]
impl Into<RM32> for Register {
    fn into(self) -> RM32 {
        match self {
            Register::EAX => RM32::EAX,
            Register::ECX => RM32::ECX,
            Register::EDX => RM32::EDX,
            Register::EBX => RM32::EBX,
            Register::ESI => RM32::ESI,
            Register::EDI => RM32::EDI,
        }
    }
}
#[allow(clippy::from_over_into)]
impl Into<Reg32> for Register {
    fn into(self) -> Reg32 {
        match self {
            Register::EAX => Reg32::EAX,
            Register::ECX => Reg32::ECX,
            Register::EDX => Reg32::EDX,
            Register::EBX => Reg32::EBX,
            Register::ESI => Reg32::ESI,
            Register::EDI => Reg32::EDI,
        }
    }
}

pub fn generate_block<E: Endian>(
    program: &mut Program<E>,
    block: &Block,
    vars: &Vars,
    rel_info: &mut RelInfo,
    temp: TempInfo,
) {
    for instruction in block.iter() {
        generate_instruction(program, instruction, vars, rel_info, temp);
    }
}
pub fn generate_instruction<E: Endian>(
    program: &mut Program<E>,
    instruction: &Instruction,
    vars: &Vars,
    rel_info: &mut RelInfo,
    temp: TempInfo,
) {
    match instruction {
        Instruction::Declare { id, value } | Instruction::Assign { id, value } => {
            let reg = Register::EAX;
            generate_expr(program, value, Intent::Load, reg, vars, rel_info, temp);
            let (_typ, offset) = &vars[id];
            program.mov_rm8_r(RM32::EBP, reg.into(), (*offset).try_into().unwrap());
        }
        Instruction::Expr(expr) => {
            generate_expr(
                program,
                expr,
                Intent::Load,
                Register::EAX,
                vars,
                rel_info,
                temp,
            );
        }
        Instruction::Loop(block) => {
            program.loop_fn(|program| {
                generate_block(program, block, vars, rel_info, temp);
            });
        }
        Instruction::While { expr: _, block: _ } => {
            todo!()
            // let addr_start = program.jmp_rel(0);
            // let start = program.code.len();

            // generate_block(program, block, vars, rel_info, temp);

            // let first_jmp_end = program.code.len();
            // let rel: i8 = (first_jmp_end as i32 - start as i32).try_into().unwrap();
            // program.code[addr_start as usize] = rel as u8;

            // generate_expr(program, expr, vars, rel_info, temp);
            //         program.cmp_r_rm(Reg32::EAX, RM32::ECX);
            //         let jmp_rel = match op {
            //             Operation::GTC => program.jg(0),
            //             Operation::LTC => program.jl(0),
            //             Operation::GTE => program.jge(0),
            //             _ => todo!(),
            //         };
            //         let end = program.code.len();
            //         let rel: i8 = (start as i32 - end as i32).try_into().unwrap();
            //         program.code[jmp_rel as usize] = rel as u8;
            // cmp
            // match &**expr {
            //     Expression::Operation { lhs, op, rhs } => {
            //         generate_expr(
            //             program,
            //             r_lhs,
            //             lhs,
            //             vars,
            //             rel_info,
            //             temp,
            //         );
            //         generate_expr(
            //             program,
            //             Intent::Load,
            //             r_rhs,
            //             rhs,
            //             vars,
            //             rel_info,
            //             temp,
            //         );
            //     }
            //     _ => {
            //         todo!();
            //     }
            // }
        }
    }
}

pub fn mov_register_imm<E: Endian>(program: &mut Program<E>, register: Register, num: u32) -> u32 {
    match register {
        Register::EAX => program.mov_eax_imm32(num),
        Register::ECX => program.mov_ecx_imm32(num),
        Register::EBX => program.mov_ebx_imm32(num),
        Register::EDX => program.mov_edx_imm32(num),
        Register::ESI => program.mov_esi_imm32(num),
        Register::EDI => program.mov_edi_imm32(num),
    }
}
pub fn add_register_imm<E: Endian>(program: &mut Program<E>, register: Register, num: u32) {
    match register {
        Register::EAX => program.add_eax_imm32(num),
        _ => program.add_rm_imm32(register.into(), num),
    }
}
pub fn sub_register_imm<E: Endian>(program: &mut Program<E>, register: Register, num: u32) {
    match register {
        Register::EAX => program.sub_eax_imm32(num),
        _ => program.sub_rm_imm32(register.into(), num),
    }
}
pub enum Intent {
    Load,
    Op(Operation),
}

fn is_simple_expression(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Number(_) | Expression::String(_) | Expression::Ident(_)
    )
}

fn generate_simple_expr<E: Endian>(
    program: &mut Program<E>,
    expr: &Expression,
    vars: &Vars,
    intent: Intent,
    register: Register,
    rel_info: &mut RelInfo,
    // temp: TempInfo,
) {
    match expr {
        Expression::Number(num) => match intent {
            Intent::Load => {
                mov_register_imm(program, register, *num);
            }
            Intent::Op(op) => match op {
                Operation::Add => {
                    add_register_imm(program, register, *num);
                }
                Operation::Sub => {
                    sub_register_imm(program, register, *num);
                }
                Operation::Mul => {
                    program.mul_r_rm_imm32(register.into(), register.into(), *num);
                }
                Operation::Div => {
                    panic!("division is not a simple operation in x86")
                }

                Operation::GTC => todo!(),
                Operation::LTC => todo!(),
                Operation::GTE => todo!(),
                Operation::LTE => todo!(),
                Operation::And => todo!(),
                Operation::Or_ => todo!(),
                Operation::BND => todo!(),
                Operation::BOR => todo!(),
            },
        },
        Expression::String(string) => match intent {
            Intent::Load => {
                let rel = mov_register_imm(program, register, 0);
                rel_info.strs.add_rel(string, rel);
            }
            Intent::Op(_) => todo!(),
        },
        Expression::Ident(id) => {
            let (_typ, offset) = &vars[id];
            let disp = (*offset).try_into().unwrap();
            match intent {
                Intent::Load => {
                    program.mov_r_rm8(register.into(), RM32::EBP, disp);
                }
                Intent::Op(op) => match op {
                    Operation::Add => {
                        program.add_r_rm8(register.into(), RM32::EBP, disp);
                    }
                    Operation::Sub => {
                        program.sub_r_rm8(register.into(), RM32::EBP, disp);
                    }
                    Operation::Mul => {
                        program.mul_r_rm8(register.into(), RM32::EBP, disp);
                    }
                    Operation::Div => {
                        panic!("division is not a simple operation in x86");
                    }

                    Operation::GTC => todo!(),
                    Operation::LTC => todo!(),
                    Operation::GTE => todo!(),
                    Operation::LTE => todo!(),
                    Operation::And => todo!(),
                    Operation::Or_ => todo!(),
                    Operation::BND => todo!(),
                    Operation::BOR => todo!(),
                },
            }
        }
        _ => {
            panic!("{expr:?} is not a simple expression")
        }
    }
}

fn generate_complex_expr<E: Endian>(
    program: &mut Program<E>,
    expr: &Expression,
    vars: &Vars,
    rel_info: &mut RelInfo,
    temp: TempInfo,
) {
    match expr {
        Expression::Operation { lhs, op, rhs } => {
            generate_expr(
                program,
                lhs,
                Intent::Load,
                Register::EAX,
                vars,
                rel_info,
                temp,
            );
            if is_simple_expression(rhs) && *op != Operation::Div {
                generate_simple_expr(program, rhs, vars, Intent::Op(*op), Register::EAX, rel_info);
            } else {
                // put lhs into storage
                match temp.storage {
                    TempStorage::Register(reg) => {
                        program.mov_rm_r(reg.into(), Reg32::EAX);
                    }
                    TempStorage::Stack(offset) => {
                        program.mov_rm8_r(RM32::EBP, Reg32::EAX, (offset).try_into().unwrap());
                    }
                }
                generate_complex_expr(program, rhs, vars, rel_info, temp.next());
                // move rhs into rhs reg
                program.mov_rm_r(RM32::ECX, Reg32::EAX);
                // recover lhs
                match temp.storage {
                    TempStorage::Register(reg) => {
                        program.mov_rm_r(RM32::EAX, reg.into());
                    }
                    TempStorage::Stack(offset) => {
                        program.mov_r_rm8(Reg32::EAX, RM32::EBP, (offset).try_into().unwrap());
                    }
                }
                match op {
                    Operation::Add => {
                        program.add_rm_r(RM32::EAX, Reg32::ECX);
                    }
                    Operation::Sub => {
                        program.sub_rm_r(RM32::EAX, Reg32::ECX);
                    }
                    Operation::Mul => {
                        program.mul_r_rm(Reg32::EAX, RM32::ECX);
                    }
                    Operation::Div => {
                        program.div_rm(RM32::ECX);
                    }
                    _ => todo!(),
                }
            }
        }
        Expression::Index { expr: _, index: _ } => todo!(),
        Expression::Function { name, args } => {
            match name.as_str() {
                "exit" => {
                    generate_expr(
                        program,
                        &args[0],
                        Intent::Load,
                        Register::EBX,
                        vars,
                        rel_info,
                        temp,
                    );
                    // program.mov_rm_r(RM32::EBX, Reg32::EAX);
                    program.exit();
                }
                _ => {
                    // TODO: account for size
                    let mut stack = 0;
                    for arg in args.iter().rev() {
                        generate_expr(
                            program,
                            arg,
                            Intent::Load,
                            Register::EAX,
                            vars,
                            rel_info,
                            temp,
                        );
                        program.push_eax();
                        stack += 4;
                    }
                    let rel = program.call_rel32(-4);
                    rel_info.funs.add_rel(name, rel);
                    program.add_esp_imm8(stack);
                }
            }
        }
        _ => {
            generate_simple_expr(program, expr, vars, Intent::Load, Register::EAX, rel_info);
        }
    }
}

pub fn generate_expr<E: Endian>(
    program: &mut Program<E>,
    expr: &Expression,
    intent: Intent,
    register: Register,
    vars: &Vars,
    rel_info: &mut RelInfo,
    temp: TempInfo,
) {
    if is_simple_expression(expr) {
        generate_simple_expr(program, expr, vars, intent, register, rel_info);
    } else {
        generate_complex_expr(program, expr, vars, rel_info, temp);
        if register != Register::EAX {
            program.mov_rm_r(register.into(), Reg32::EAX);
        }
    }
}
