use std::{ffi::CString, rc::Rc};

use crate::{
    elf::Endian,
    ir::{IRCode, Instruction, InstructionGroup, Value},
    parser::DeclaredVars,
    tokenizer::{Id, Operation},
    x86_program::{Program, Reg32, RM32},
};

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

pub type RelFun = (Id, Vec<u32>);
pub struct Functions(pub Vec<RelFun>);
impl Functions {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn contains(&self, function: &Id) -> bool {
        self.0.iter().any(|(f, _)| *f == *function)
    }
    pub fn add_fun(&mut self, function: &Id) {
        if !self.contains(function) {
            self.0.push((function.clone(), vec![]));
        }
    }
    pub fn get_mut(&mut self, function: &Id) -> Option<&mut Vec<u32>> {
        self.0
            .iter_mut()
            .find(|(s, _)| *s == *function)
            .map(|(_, x)| x)
    }
    pub fn add_rel(&mut self, function: &Id, offset: u32) {
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

#[derive(Debug, Clone)]
pub struct Vars(Vec<(Id, i32)>);
impl Vars {
    fn new() -> Self {
        Self(vec![])
    }
    fn get(&self, id: &Id) -> Option<i32> {
        self.0
            .iter()
            .find_map(|(vars_id, offset)| if *vars_id == *id { Some(*offset) } else { None })
    }
    pub fn alloc_vars(decl_vars: &DeclaredVars, start: &mut i32) -> Self {
        let mut vars = Self::new();
        for (id, typ) in decl_vars.0.iter() {
            *start -= typ.size() as i32;
            vars.0.push((id.clone(), *start));
        }
        vars
    }
    pub fn add(&mut self, other: &Self) {
        self.0.extend_from_slice(&other.0);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
enum Register {
    EAX,
    ECX,
    EDX,
    EBX,
    ESI,
    EDI,
}
impl Register {
    fn next(self) -> Self {
        match self {
            Register::EAX => Register::ECX,
            Register::ECX => Register::EDX,
            Register::EDX => Register::EBX,
            Register::EBX => Register::ESI,
            Register::ESI => Register::EDI,
            Register::EDI => todo!(),
        }
    }
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

struct TempLocations(Vec<Register>);
impl TempLocations {
    fn new() -> Self {
        Self(vec![Register::ECX])
    }
    fn get(&mut self, temp: u32) -> Register {
        match self.0.get(temp as usize) {
            Some(reg) => *reg,
            None => {
                let reg = self.0.last().unwrap().next();
                self.0.push(reg);
                reg
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Term {
    Imm(u32),
    Reg(Register),
    Stack(i32),
}
impl Term {
    fn is_location(&self) -> bool {
        matches!(self, Self::Reg(_) | Self::Stack(_))
    }
    fn from_value(value: &Value, vars: &Vars, temp: &mut TempLocations) -> Self {
        match value {
            Value::Id(id) => {
                let disp = vars.get(id).unwrap();
                Self::Stack(disp)
            }
            Value::Temp(t) => {
                let reg = temp.get(*t);
                Self::Reg(reg)
            }
            Value::Num(num) => Self::Imm(*num),
            Value::String(_) => todo!(),
        }
    }
}

fn op_t_t<E: Endian>(program: &mut Program<E>, target: Term, op: &Operation, rhs: Term) {
    match target {
        Term::Reg(reg) => {
            match op {
                Operation::Add => {
                    match rhs {
                        Term::Imm(num) => {
                            if let Register::EAX = reg {
                                program.add_eax_imm32(num);
                            } else {
                                program.add_rm_imm32(reg.into(), num);
                            }
                        }
                        Term::Reg(r) => {
                            program.add_rm_r(reg.into(), r.into());
                            // program.add_r_rm(reg.into(), r.into());
                        }
                        Term::Stack(disp) => {
                            program.add_r_rm8(reg.into(), RM32::EBP, disp.try_into().unwrap());
                        }
                    }
                }
                Operation::Sub => {
                    match rhs {
                        Term::Imm(num) => {
                            if let Register::EAX = reg {
                                program.sub_eax_imm32(num);
                            } else {
                                program.sub_rm_imm32(reg.into(), num);
                            }
                        }
                        Term::Reg(r) => {
                            program.sub_rm_r(reg.into(), r.into());
                            // program.sub_r_rm(reg.into(), r.into());
                        }
                        Term::Stack(disp) => {
                            program.sub_r_rm8(reg.into(), RM32::EBP, disp.try_into().unwrap());
                        }
                    }
                }
                Operation::Mul => match rhs {
                    Term::Imm(num) => {
                        program.mul_r_rm_imm32(reg.into(), reg.into(), num);
                    }
                    Term::Reg(r) => {
                        program.mul_r_rm(reg.into(), r.into());
                    }
                    Term::Stack(disp) => {
                        program.mul_r_rm8(reg.into(), RM32::EBP, disp.try_into().unwrap());
                    }
                },
                Operation::Div => todo!(),
                _ => todo!(),
            }
        }
        Term::Stack(disp) => match op {
            Operation::Add => match rhs {
                Term::Imm(num) => {
                    program.add_rm8_imm32(RM32::EBP, disp.try_into().unwrap(), num);
                }
                Term::Reg(reg) => {
                    program.add_rm8_r(RM32::EBP, reg.into(), disp.try_into().unwrap());
                }
                Term::Stack(disp2) => {
                    program.mov_r_rm8(Reg32::EAX, RM32::EBP, disp2.try_into().unwrap());
                    program.add_rm8_r(RM32::EBP, Reg32::EAX, disp.try_into().unwrap());
                }
            },
            Operation::Sub => match rhs {
                Term::Imm(num) => {
                    program.sub_rm8_imm32(RM32::EBP, disp.try_into().unwrap(), num);
                }
                Term::Reg(reg) => {
                    program.sub_rm8_r(RM32::EBP, reg.into(), disp.try_into().unwrap());
                }
                Term::Stack(disp2) => {
                    program.mov_r_rm8(Reg32::EAX, RM32::EBP, disp2.try_into().unwrap());
                    program.sub_rm8_r(RM32::EBP, Reg32::EAX, disp.try_into().unwrap());
                }
            },
            Operation::Mul => match rhs {
                Term::Imm(num) => {
                    program.mul_r_rm8_imm32(Reg32::EAX, RM32::EBP, disp.try_into().unwrap(), num);
                    program.mov_rm8_r(RM32::EBP, Reg32::EAX, disp.try_into().unwrap());
                }
                Term::Reg(reg) => {
                    program.mov_r_rm8(Reg32::EAX, RM32::EBP, disp.try_into().unwrap());
                    program.mul_r_rm(Reg32::EAX, reg.into());
                    program.mov_rm8_r(RM32::EBP, Reg32::EAX, disp.try_into().unwrap());
                }
                Term::Stack(disp2) => {
                    program.mov_r_rm8(Reg32::EAX, RM32::EBP, disp2.try_into().unwrap());
                    program.mul_r_rm8(Reg32::EAX, RM32::EBP, disp.try_into().unwrap());
                    program.mov_rm8_r(RM32::EBP, Reg32::EAX, disp.try_into().unwrap());
                }
            },
            Operation::Div => todo!(),
            _ => todo!(),
        },
        Term::Imm(_) => unreachable!(),
    }
}
fn op_t_t_t<E: Endian>(
    program: &mut Program<E>,
    target: Term,
    lhs: Term,
    op: &Operation,
    rhs: Term,
) {
    if !target.is_location() {
        panic!("target must be a location but got {target:?}");
    }
    if target == lhs {
        op_t_t(program, target, op, rhs);
    } else {
        // TODO: possible optimizations here like LEA, mul
        match target {
            Term::Reg(reg) => match lhs {
                Term::Imm(num) => {
                    program.mov_r_imm32(reg.into(), num);
                }
                Term::Reg(reg2) => {
                    program.mov_rm_r(reg.into(), reg2.into());
                    program.mov_r_rm(reg.into(), reg2.into());
                }
                Term::Stack(disp) => {
                    program.mov_r_rm8(reg.into(), RM32::EBP, disp.try_into().unwrap());
                }
            },
            Term::Stack(disp) => match lhs {
                Term::Imm(num) => {
                    program.mov_rm8_imm32(RM32::EBP, disp.try_into().unwrap(), num);
                }
                Term::Reg(reg) => {
                    program.mov_rm8_r(RM32::EBP, reg.into(), disp.try_into().unwrap());
                }
                Term::Stack(disp2) => {
                    program.mov_r_rm8(Reg32::EAX, RM32::EBP, disp2.try_into().unwrap());
                    program.mov_rm8_r(RM32::EBP, Reg32::EAX, disp.try_into().unwrap());
                }
            },
            Term::Imm(_) => unreachable!(),
        }
        op_t_t(program, target, op, rhs);
    }
}

fn generate_instruction<E: Endian>(
    program: &mut Program<E>,
    instruction: &Instruction,
    start: &mut i32,
    vars: &Vars,
    rel_info: &mut RelInfo,
) {
    let mut temp = TempLocations::new();
    match instruction {
        Instruction::Assign { target, value } => match target {
            Value::Id(id) => {
                let disp = vars.get(id).unwrap();
                match value {
                    Value::Id(value_id) => {
                        let disp2 = vars.get(value_id).unwrap();
                        program.mov_r_rm8(Reg32::EAX, RM32::EBP, disp2.try_into().unwrap());
                        program.mov_rm8_r(RM32::EBP, Reg32::EAX, disp.try_into().unwrap());
                    }
                    Value::Num(num) => {
                        program.mov_rm8_imm32(RM32::EBP, disp.try_into().unwrap(), *num);
                    }
                    Value::String(string) => {
                        let rel = program.mov_rm8_imm32(RM32::EBP, disp.try_into().unwrap(), 0);
                        rel_info.strs.add_rel(string, rel);
                    }
                    _ => panic!("can't load {value} in `{instruction}`"),
                }
            }
            _ => panic!("can't assigning {target} in `{instruction}`"),
        },
        Instruction::Op {
            target,
            lhs,
            op,
            rhs,
        } => {
            // TODO: possible optimizations like worrying less about preservering temps for last instruction
            op_t_t_t(
                program,
                Term::from_value(target, vars, &mut temp),
                Term::from_value(lhs, vars, &mut temp),
                op,
                Term::from_value(rhs, vars, &mut temp),
            )
        }
        Instruction::Function { name, args } => {
            match name.0.as_str() {
                "exit" => {
                    match &args[0] {
                        Value::Id(id) => {
                            let offset = vars.get(id).unwrap();
                            program.mov_r_rm8(Reg32::EBX, RM32::EBP, offset.try_into().unwrap());
                        }
                        Value::Temp(t) => {
                            let reg = temp.get(*t);
                            program.mov_rm_r(RM32::EBX, reg.into());
                            // program.mov_r_rm(Reg32::EDX, reg.into());
                        }
                        Value::Num(num) => {
                            program.mov_r_imm32(Reg32::EBX, *num);
                        }
                        Value::String(_) => todo!(),
                    }
                    program.exit();
                }
                _ => {
                    // // TODO: account for size
                    let mut stack = 0;
                    for arg in args.iter().rev() {
                        match arg {
                            Value::Id(id) => {
                                let disp = vars.get(id).unwrap();
                                program.push_rm8(RM32::EBP, disp.try_into().unwrap());
                            }
                            Value::Temp(t) => {
                                let reg = temp.get(*t);
                                program.push_r(reg.into());
                            }
                            Value::Num(num) => {
                                program.push_imm32(*num);
                            }
                            Value::String(string) => {
                                let rel = program.push_imm32(0);
                                rel_info.strs.add_rel(string, rel);
                            }
                        }
                        stack += 4;
                    }
                    let rel = program.call_rel32(-4);
                    rel_info.funs.add_rel(name, rel);
                    program.add_esp_imm8(stack);
                }
            }
        }
        Instruction::Block { decl_vars, code } => {
            let old_start = *start;
            let mut new_vars = Vars::alloc_vars(decl_vars, start);
            new_vars.add(vars);
            generate_ir(program, code, start, &new_vars, rel_info);
            *start = old_start;
        }
    }
}

pub fn generate<E: Endian>(program: &mut Program<E>, code: &IRCode) -> RelInfo {
    let mut rel_info = RelInfo::new();
    let alloc = x86_analyse_ir(code);
    program.mov_edp_esp();
    program.sub_esp_imm8(alloc.try_into().unwrap());
    generate_ir(program, code, &mut 0, &Vars::new(), &mut rel_info);
    program.mov_esp_edp();
    rel_info
}

fn generate_ir<E: Endian>(
    program: &mut Program<E>,
    code: &IRCode,
    start: &mut i32,
    vars: &Vars,
    rel_info: &mut RelInfo,
) {
    for group in code.0.iter() {
        for instruction in group.0.iter() {
            generate_instruction(program, instruction, start, vars, rel_info);
        }
    }
}

fn analyse_ir_group(group: &InstructionGroup) -> u32 {
    let mut alloc = 0;
    for instruction in group.0.iter() {
        if let Instruction::Block {
            decl_vars: vars,
            code,
        } = instruction
        {
            // alloc += va
            for (_var, typ) in vars.0.iter() {
                alloc += typ.size();
            }

            let mut max_alloc = 0;
            for group in code.0.iter() {
                max_alloc = max_alloc.max(analyse_ir_group(group));
            }
            alloc += max_alloc;
        }
    }
    alloc
}
fn x86_analyse_ir(code: &IRCode) -> u32 {
    let mut alloc = 0;
    for group in code.0.iter() {
        alloc = analyse_ir_group(group);
    }
    alloc
}

fn calc_temp_space_needed(code: &IRCode) {
    let mut max_temp_space = 0;
    for group in code.0.iter() {
        max_temp_space = max_temp_space.max(calc_temp_space_needed_instruction_group(group));
    }
}
fn calc_temp_space_needed_instruction_group(group: &InstructionGroup) -> u32 {
    let mut highest_temp_num = 0;
    for instruction in group.0.iter() {
        if let Instruction::Assign { target, .. } | Instruction::Op { target, .. } = instruction {
            if let Value::Temp(num) = target {
                highest_temp_num = highest_temp_num.max(*num);
            }
        }
    }
    highest_temp_num
}
fn contains_div(group: &InstructionGroup) -> bool {
    for instruction in group.0.iter() {
        if let Instruction::Op { op, .. } = instruction {
            if *op == Operation::Div {
                return true;
            }
        }
    }
    false
}
