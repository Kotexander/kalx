use std::ffi::CString;

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

#[derive(Debug, Clone)]
pub struct Vars {
    alloc: i32,
    vars: HashMap<Rc<String>, (Type, i32)>,
}
impl Vars {
    pub fn new() -> Self {
        Self {
            alloc: 0,
            vars: HashMap::new(),
        }
    }
    pub fn add(&mut self, id: Rc<String>, typ: Type) {
        match typ {
            Type::String => self.alloc -= 4,
            Type::U32 => self.alloc -= 4,
            Type::Bool => self.alloc -= 1,
        }
        self.vars.insert(id, (typ, self.alloc));
    }
    pub fn contains_str(&self, s: &Rc<String>) -> bool {
        self.vars.contains_key(s)
    }
    pub fn allocated(&self) -> i32 {
        self.alloc
    }
    pub fn get(&self, s: &Rc<String>) -> Option<&(Type, i32)> {
        self.vars.get(s)
    }
}
// impl std::ops::Index<&Rc<String>> for Vars {
//     type Output = (Type, i32);

//     fn index(&self, index: &Rc<String>) -> &Self::Output {
//         &self.vars[index]
//     }
// }

#[derive(Debug, Clone)]
pub struct RelString {
    pub sym: Rc<String>,
    pub rels: Vec<u32>,
}
impl RelString {
    pub fn new(sym: Rc<String>) -> Self {
        Self { sym, rels: vec![] }
    }
    pub fn add(&mut self, rel: u32) {
        self.rels.push(rel);
    }
}

#[derive(Debug, Clone)]
pub struct Strings {
    strs: HashMap<Rc<CString>, (RelString, u32)>,
    index: u32,
}
impl Strings {
    pub fn new() -> Self {
        Self {
            strs: HashMap::new(),
            index: 0,
        }
    }
    pub fn add(&mut self, s: &Rc<CString>) {
        if !self.strs.contains_key(s) {
            let rel_str = RelString::new(Rc::new(format!("str{}", self.strs.len())));
            self.strs.insert(s.clone(), (rel_str, self.index));
            self.index += s.as_bytes_with_nul().len() as u32;
        }
    }
    pub fn get_mut(&mut self, s: &Rc<CString>) -> Option<&mut (RelString, u32)> {
        self.strs.get_mut(s)
    }
    pub fn values(&self) -> Vec<&(RelString, u32)> {
        let mut v: Vec<&(RelString, u32)> = self.strs.values().collect();
        v.sort_by(|(_, a1), (_, a2)| a1.cmp(a2));
        v
    }
    pub fn values_mut(&mut self) -> Vec<&mut (RelString, u32)> {
        let mut v: Vec<&mut (RelString, u32)> = self.strs.values_mut().collect();
        v.sort_by(|(_, a1), (_, a2)| a1.cmp(a2));
        v
    }
    pub fn keys(&self) -> Vec<&Rc<CString>> {
        let mut v: Vec<(&Rc<CString>, &(RelString, u32))> = self.strs.iter().collect();
        v.sort_by(|(_, (_, a1)), (_, (_, a2))| a1.cmp(a2));

        v.into_iter().map(|(k, _)| k).collect()
    }
}

pub fn analyse_expr(expr: &Expression, vars: &Vars, strings: &mut Strings) -> Result<Type, String> {
    match expr {
        Expression::Number(_) => Ok(Type::U32),
        Expression::String(string) => {
            strings.add(string);
            Ok(Type::String)
        }
        Expression::Ident(id) => match vars.get(id) {
            Some((typ, _)) => Ok(*typ),
            None => Err(format!("undefined variable: {id}")),
        },
        Expression::Operation { lhs, op, rhs } => {
            let lhs = analyse_expr(lhs, vars, strings)?;
            let rhs = analyse_expr(rhs, vars, strings)?;

            if lhs == rhs {
                Ok(match op {
                    Operation::GTC | Operation::LTC => Type::Bool,
                    _ => lhs,
                })
            } else {
                Err(format!("tried invalid operation: {lhs} {op} {rhs}"))
            }
        }
        Expression::Index { expr, index } => {
            let typ = analyse_expr(expr, vars, strings)?;
            let i = analyse_expr(index, vars, strings)?;

            if i == Type::U32 {
                Ok(typ)
            } else {
                Err(format!("index must have type of u32 but got: {i}"))
            }
        }
        Expression::Function { name: _, arg } => analyse_expr(arg, vars, strings),
    }
}
pub fn analyse_instruction(
    instruction: &Instruction,
    vars: &mut Vars,
    strings: &mut Strings,
) -> Result<(), String> {
    match instruction {
        Instruction::Declare { id, value } => {
            if !vars.contains_str(id) {
                let typ = analyse_expr(value, vars, strings)?;
                vars.add(id.clone(), typ);
                Ok(())
            } else {
                Err(format!("duplicate declaration of {id}"))
            }
        }
        Instruction::Assign { id, value } => {
            if vars.contains_str(id) {
                analyse_expr(value, vars, strings)?;
                Ok(())
            } else {
                Err(format!("assignment of {id} before declaration"))
            }
        }
        Instruction::Expr(expr) => {
            analyse_expr(expr, vars, strings)?;
            Ok(())
        }
        Instruction::Binary { lhs, rhs } => {
            analyse_instruction(lhs, vars, strings)?;
            analyse_instruction(rhs, vars, strings)?;
            Ok(())
        }
        Instruction::Loop(block) => {
            if let Some(block) = block {
                analyse_instruction(block, vars, strings)?;
            }
            Ok(())
        }
        Instruction::While { expr, block } => {
            let typ = analyse_expr(expr, vars, strings)?;
            assert_eq!(typ, Type::Bool);
            if let Some(block) = block {
                analyse_instruction(block, vars, strings)?;
            }
            Ok(())
        }
    }
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
                let (rel_str, addr) = strings.get_mut(string).unwrap();
                let rel = mov_reg_imm(program, register, *addr);
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
        Expression::Function { name, arg } => {
            match &name[..] {
                "exit" => {
                    generate_expr(program, Intent::Load, Register::EBX, arg, vars, strings);
                    program.exit();
                }
                "print" => {
                    match &**arg {
                        Expression::String(string) => {
                            let (rel_str, addr) = strings.get_mut(string).unwrap();
                            let rel =
                                program.write_const(*addr, string.as_bytes_with_nul().len() as u32);
                            rel_str.add(rel);
                        }
                        Expression::Ident(id) => {
                            let (typ, addr) = vars.get(id).unwrap();
                            if let Type::String = typ {
                                // program.write_const(0, 1);
                                program.write_rm8(RM32::EBP, (*addr).try_into().unwrap(), 5);
                                // todo!()
                            } else {
                                todo!()
                            }
                        }
                        Expression::Number(_) => todo!(),
                        Expression::Operation {
                            lhs: _,
                            op: _,
                            rhs: _,
                        } => todo!(),
                        Expression::Index { expr: _, index: _ } => todo!(),
                        Expression::Function { name: _, arg: _ } => todo!(),
                    }
                }
                _ => {
                    panic!("unknown function: {name}");
                }
            }
        }
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
            }
        }
        Instruction::Expr(expr) => {
            generate_expr(program, Intent::Load, Register::EAX, expr, vars, strings);
        }
        Instruction::Binary { lhs, rhs } => {
            generate_instruction(program, lhs, vars, strings);
            generate_instruction(program, rhs, vars, strings);
        }
        Instruction::Loop(block) => {
            program.loop_fn(|program| {
                if let Some(block) = block {
                    generate_instruction(program, block, vars, strings);
                }
            });
        }
        Instruction::While { expr, block } => {
            program.jmp_rel(0);
            let start = program.code.len();
            // body
            if let Some(block) = block {
                generate_instruction(program, block, vars, strings);
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
