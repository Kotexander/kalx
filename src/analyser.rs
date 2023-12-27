use std::ffi::CString;

use super::*;

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
            Type::Void => {return;},
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

#[derive(Debug, Clone)]
pub struct RelString {
    pub sym: Rc<String>,
    pub sym_index: u32,
    pub rels: Vec<u32>,
}
impl RelString {
    pub fn new(sym: Rc<String>) -> Self {
        Self {
            sym,
            rels: vec![],
            sym_index: 0,
        }
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
            None => Err(format!("undefined variable `{id}`")),
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
                Err(format!("tried invalid operation `{lhs} {op} {rhs}`"))
            }
        }
        Expression::Index { expr, index } => {
            let typ = analyse_expr(expr, vars, strings)?;
            let i = analyse_expr(index, vars, strings)?;

            if i == Type::U32 {
                Ok(typ)
            } else {
                Err(format!("index must have type of `u32` but got `{i}`"))
            }
        }
        Expression::Function { name, args } => {
            match name.as_str() {
                "exit" => {
                    if args.len() != 1 {
                        return Err(format!("wrong number of arguments for `{name}`"));
                    }
                }
                // "print" => {
                //     if args.len() != 1 {
                //         return Err(format!("wrong number of arguments for {name}"));
                //     }
                // }
                _ => {
                    return Err(format!("unkown function `{name}`"));
                }
            }

            for expr in args.iter() {
                analyse_expr(expr, vars, strings)?;
            }
            Ok(Type::Void)
        },
    }
}

pub fn analyse_block(block: &Block, vars: &mut Vars, strings: &mut Strings) -> Result<(), String> {
    for instruction in block.iter() {
        analyse_instruction(instruction, vars, strings)?;
    }
    Ok(())
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
                if typ == Type::Void {
                    Err(format!("can't declare `{id}` as having type of `void`"))
                }
                else {
                    vars.add(id.clone(), typ);
                    Ok(())
                }
            } else {
                Err(format!("duplicate declaration of `{id}`"))
            }
        }
        Instruction::Assign { id, value } => {
            if vars.contains_str(id) {
                analyse_expr(value, vars, strings)?;
                Ok(())
            } else {
                Err(format!("assignment of `{id}` before declaration"))
            }
        }
        Instruction::Expr(expr) => {
            analyse_expr(expr, vars, strings)?;
            Ok(())
        }
        Instruction::Loop(block) => {
            analyse_block(block, vars, strings)
        }
        Instruction::While { expr, block } => {
            let typ = analyse_expr(expr, vars, strings)?;
            assert_eq!(typ, Type::Bool);
            for instruction in block.iter() {
                analyse_instruction(instruction, vars, strings)?;
            }
            Ok(())
        }
    }
}
