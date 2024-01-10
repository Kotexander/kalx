use std::{ffi::CString, rc::Rc, vec};

use crate::{
    elf::Endian,
    ir::{Block, Function, Instruction, Label, Value},
    parser::DeclaredVars,
    tokenizer::{BoolOperation, Id, Operation},
    x86_program::{Program, Reg32, RM32},
};

type RelStr = (Rc<CString>, Vec<u32>);
pub struct RelStrings(pub Vec<RelStr>);
impl RelStrings {
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
pub struct RelFunctions(pub Vec<RelFun>);
impl RelFunctions {
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
    pub strs: RelStrings,
    pub funs: RelFunctions,
}
impl RelInfo {
    pub fn new() -> Self {
        Self {
            strs: RelStrings::new(),
            funs: RelFunctions::new(),
        }
    }
    // pub fn clear(&mut self) {
    //     self.strs.0.clear();
    //     self.funs.0.clear();
    // }
    pub fn merge(&mut self, other: Self) {
        for (string, rels) in other.strs.0 {
            for rel in rels {
                self.strs.add_rel(&string, rel)
            }
        }
        for (name, rels) in other.funs.0 {
            for rel in rels {
                self.funs.add_rel(&name, rel)
            }
        }
    }
    pub fn add_offset(&mut self, offset: u32) {
        for (_, rels) in self.strs.0.iter_mut() {
            for rel in rels {
                *rel += offset;
            }
        }
        for (_, rels) in self.funs.0.iter_mut() {
            for rel in rels {
                *rel += offset;
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum LabelRelocType {
    Rel8(u32),
    // Rel32(u32)
}

#[derive(Debug, Clone)]
struct LabelInfo {
    labels: Vec<(Label, u32)>,
    /// (location, type)
    reloc: Vec<(Label, Vec<(u32, LabelRelocType)>)>,
}
impl LabelInfo {
    fn new() -> Self {
        Self {
            labels: vec![],
            reloc: vec![],
        }
    }
    fn add_label(&mut self, label: Label, addr: u32) {
        match self
            .labels
            .iter()
            .find_map(|(l, a)| if *l == label { Some(*a) } else { None })
        {
            Some(a) => {
                if a != addr {
                    panic!("duplicate declaration of `L<{label}>` first: {a}, other: {addr}");
                }
            }
            None => {
                self.labels.push((label, addr));
            }
        }
    }
    fn add_rel(&mut self, label: &Label, location: u32, typ: LabelRelocType) {
        match self
            .reloc
            .iter_mut()
            .find_map(|(l, a)| if *l == *label { Some(a) } else { None })
        {
            Some(rels) => {
                rels.push((location, typ));
            }
            None => {
                self.reloc.push((*label, vec![(location, typ)]));
            }
        }
    }
    fn get(&self, label: &Label) -> Option<u32> {
        self.labels
            .iter()
            .find_map(|(l, a)| if *l == *label { Some(*a) } else { None })
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

struct TempMap(Vec<Vec<u32>>);
impl TempMap {
    fn new() -> Self {
        Self(vec![])
    }
    fn get(&self, t: &u32) -> Register {
        match self.0.iter().position(|rank| rank.contains(t)).unwrap() {
            0 => Register::ECX,
            1 => Register::EDX,
            2 => Register::EBX,
            3 => Register::ESI,
            4 => Register::EDI,
            _ => todo!(),
        }
    }
}

fn calc_temp_value(value: &Value, temps: &mut TempMap) {
    if let Value::Temp(t) = value {
        let mut found = None;
        let mut promoted_temps = vec![];

        for (rank, rank_temps) in temps.0.iter_mut().enumerate() {
            if let Some(i) = rank_temps.iter().position(|tt| *tt == *t) {
                let i = i + 1;
                promoted_temps = rank_temps[i..].to_vec();
                rank_temps.truncate(i);
                found = Some(rank);
                break;
            }
        }
        match found {
            Some(rank) => match temps.0.get_mut(rank + 1) {
                Some(rank) => {
                    rank.extend_from_slice(&promoted_temps);
                }
                None => {
                    if !promoted_temps.is_empty() {
                        temps.0.push(promoted_temps);
                    }
                }
            },
            None => match temps.0.first_mut() {
                Some(temps) => {
                    temps.push(*t);
                }
                None => {
                    temps.0.push(vec![*t]);
                }
            },
        }
    }
}
fn calc_temp_code(code: &Block, temps: &mut TempMap) {
    for instruction in code.0.iter() {
        match instruction {
            Instruction::Assign { target, value } => {
                calc_temp_value(value, temps);
                calc_temp_value(target, temps);
            }
            Instruction::Op {
                target,
                lhs,
                op: _,
                rhs,
            } => {
                calc_temp_value(lhs, temps);
                calc_temp_value(rhs, temps);
                calc_temp_value(target, temps);
            }
            Instruction::FunctionCall { name: _, args } => {
                for arg in args.iter() {
                    calc_temp_value(arg, temps);
                }
            }
            Instruction::Block { decl_vars: _, code } => {
                calc_temp_code(code, temps);
            }
            Instruction::Label(_) => {}
            Instruction::Goto(_) => {}
            Instruction::IfGoto { lhs, rhs, .. } => {
                calc_temp_value(lhs, temps);
                calc_temp_value(rhs, temps);
            }
        }
    }
}
fn calc_temp(code: &Block) -> TempMap {
    let mut temps = TempMap::new();
    calc_temp_code(code, &mut temps);
    temps
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
    fn from_value(value: &Value, vars: &Vars, temps: &TempMap) -> Self {
        match value {
            Value::Id(id) => {
                let disp = vars.get(id).unwrap();
                Self::Stack(disp)
            }
            Value::Temp(t) => {
                let reg = temps.get(t);
                Self::Reg(reg)
            }
            Value::Num(num) => Self::Imm(*num),
            Value::String(_) => todo!(),
            Value::Deref(_) => todo!(),
            Value::Index { base:_, index:_ } => todo!(),
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

fn cmp<E: Endian>(program: &mut Program<E>, lhs: Term, rhs: Term) {
    match (lhs, rhs) {
        (Term::Imm(_), Term::Imm(_)) => todo!(),
        (Term::Imm(_), Term::Reg(_)) => todo!(),
        (Term::Imm(_), Term::Stack(_)) => todo!(),
        (Term::Reg(r), Term::Stack(disp)) => {
            program.cmp_r_rm8(r.into(), RM32::EBP, disp.try_into().unwrap());
        }
        (Term::Reg(r), Term::Imm(n)) => {
            program.cmp_rm_imm32(r.into(), n);
        }
        (Term::Reg(r), Term::Reg(r2)) => {
            program.cmp_rm_r(r.into(), r2.into());
            // program.cmp_r_rm(r.into(), r2.into());
        }
        (Term::Stack(disp), Term::Imm(n)) => {
            program.cmp_rm8_imm32(RM32::EBP, disp.try_into().unwrap(), n);
        }
        (Term::Stack(disp), Term::Reg(r)) => {
            program.cmp_rm8_r(RM32::EBP, r.into(), disp.try_into().unwrap());
        }
        (Term::Stack(disp), Term::Stack(disp2)) => {
            program.mov_r_rm8(Reg32::EAX, RM32::EBP, disp.try_into().unwrap());
            program.cmp_r_rm8(Reg32::EAX, RM32::EBP, disp2.try_into().unwrap());
        }
    }
}

fn generate_instruction<E: Endian>(
    program: &mut Program<E>,
    instruction: &Instruction,
    alloc_end: i32,
    vars: &Vars,
    temps: &TempMap,
    rel_info: &mut RelInfo,
    label_info: &mut LabelInfo,
) {
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
                Term::from_value(target, vars, temps),
                Term::from_value(lhs, vars, temps),
                op,
                Term::from_value(rhs, vars, temps),
            );
        }
        Instruction::FunctionCall { name, args } => {
            match name.0.as_str() {
                "exit" => {
                    match &args[0] {
                        Value::Id(id) => {
                            let offset = vars.get(id).unwrap();
                            program.mov_r_rm8(Reg32::EBX, RM32::EBP, offset.try_into().unwrap());
                        }
                        Value::Temp(t) => {
                            let reg = temps.get(t);
                            program.mov_rm_r(RM32::EBX, reg.into());
                            // program.mov_r_rm(Reg32::EDX, reg.into());
                        }
                        Value::Num(num) => {
                            program.mov_r_imm32(Reg32::EBX, *num);
                        }
                        Value::String(_) => todo!(),
                        Value::Deref(_) => todo!(),
                        Value::Index { base:_, index:_ } => todo!(),
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
                                let reg = temps.get(t);
                                program.push_r(reg.into());
                            }
                            Value::Num(num) => {
                                program.push_imm32(*num);
                            }
                            Value::String(string) => {
                                let rel = program.push_imm32(0);
                                rel_info.strs.add_rel(string, rel);
                            }
                            Value::Deref(_) => todo!(),
                            Value::Index { base:_, index:_ } => todo!(),
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
            let mut new_alloc_end = alloc_end;
            let mut new_vars = Vars::alloc_vars(decl_vars, &mut new_alloc_end);
            new_vars.add(vars);
            generate_code(
                program,
                code,
                new_alloc_end,
                &new_vars,
                temps,
                rel_info,
                label_info,
            );
        }
        Instruction::Label(l) => {
            label_info.add_label(*l, program.addr());
        }
        Instruction::Goto(l) => {
            let location = program.jmp_rel8(0);
            let reloc = program.addr();
            label_info.add_rel(l, location, LabelRelocType::Rel8(reloc));
        }
        Instruction::IfGoto {
            lhs,
            op,
            rhs,
            dest: good,
        } => {
            cmp(
                program,
                Term::from_value(lhs, vars, temps),
                Term::from_value(rhs, vars, temps),
            );
            let location = match op {
                BoolOperation::GTC => program.jg(0),
                BoolOperation::LTC => program.jl(0),
                BoolOperation::GTE => program.jge(0),
                BoolOperation::LTE => program.jle(0),
                _ => todo!(),
            };
            let jmp_reloc = program.addr();
            label_info.add_rel(good, location, LabelRelocType::Rel8(jmp_reloc));
        }
    }
}

fn generate_code<E: Endian>(
    program: &mut Program<E>,
    code: &Block,
    alloc_end: i32,
    vars: &Vars,
    temps: &TempMap,
    rel_info: &mut RelInfo,
    label_info: &mut LabelInfo,
) {
    for instruction in code.0.iter() {
        generate_instruction(
            program,
            instruction,
            alloc_end,
            vars,
            temps,
            rel_info,
            label_info,
        );
    }
}
pub struct FunDecl {
    pub name: Id,
    pub entry: u32,
    pub size: u32,
}
pub fn generate<E: Endian>(functions: &[Function]) -> (Program<E>, RelInfo, Vec<FunDecl>) {
    let mut program = Program::<E>::new();
    let mut rel_info = RelInfo::new();
    let mut funs = vec![];

    for function in functions {
        let mut p = Program::<E>::new();
        let mut vars = Vars::new();
        let mut disp = 4; // 4 for ebp and 4 for ret addr
        for (id, typ) in function.args.iter() {
            disp += typ.size() as i32;
            vars.0.push((id.clone(), disp));
        }

        let temps = calc_temp(&function.block);
        let mut label_info = LabelInfo::new();
        let mut r_info = RelInfo::new();

        let alloc = calc_alloc(&function.block);
        // program.push_r(Reg32::EBX);
        // program.push_r(Reg32::ESI);
        // program.push_r(Reg32::EDI);
        p.push_r(Reg32::EBP);
        p.mov_rm_r(RM32::EBP, Reg32::ESP);
        p.sub_esp_imm8(alloc.try_into().unwrap());
        generate_code(
            &mut p,
            &function.block,
            0,
            &vars,
            &temps,
            &mut r_info,
            &mut label_info,
        );

        for (label, rels) in label_info.reloc.iter() {
            let label_addr = label_info.get(label).unwrap();
            for (location, typ) in rels.iter() {
                match typ {
                    LabelRelocType::Rel8(addr) => {
                        let rel: i8 = (label_addr as i32 - *addr as i32).try_into().unwrap();
                        p.replace_u8(*location, rel as u8);
                    }
                }
            }
        }

        p.mov_r_imm32(Reg32::EAX, 0);
        // program.pull_r(Reg32::EBX);
        // program.pull_r(Reg32::ESI);
        // program.pull_r(Reg32::EDI);

        p.mov_rm_r(RM32::SIB, Reg32::EBP);
        p.pop_r(Reg32::EBP);
        // program.leave();
        p.ret();

        let entry = program.addr();
        funs.push(FunDecl {
            name: function.name.clone(),
            entry: program.addr(),
            size: p.addr(),
        });

        r_info.add_offset(entry);
        rel_info.merge(r_info);
        program.code.extend_from_slice(&p.code);
    }
    (program, rel_info, funs)
}

fn calc_alloc(code: &Block) -> u32 {
    let mut alloc = 0;
    for instruction in code.0.iter() {
        if let Instruction::Block {
            decl_vars: vars,
            code,
        } = instruction
        {
            let mut block_alloc = 0;
            for (_var, typ) in vars.0.iter() {
                block_alloc += typ.size();
            }

            block_alloc += calc_alloc(code);

            alloc = alloc.max(block_alloc);
        }
    }
    alloc
}
