use std::{collections::HashMap, rc::Rc};

use crate::{
    parser::{Block, Expression, Instruction},
    tokenizer::{Id, Type},
};

#[derive(Debug, Clone)]
pub struct DeclaredVar {
    pub typ: Type,
}
#[derive(Debug, Clone)]
pub enum ArgLayout {
    List(Vec<Type>),
    Variable(Vec<Type>),
}
impl ArgLayout {
    fn args(&self) -> &[Type] {
        match self {
            ArgLayout::List(args) => args,
            ArgLayout::Variable(args) => args,
        }
    }
}
#[derive(Debug, Clone)]
pub struct DeclaredFun {
    pub args: ArgLayout,
    pub ret: Type,
}
impl DeclaredFun {
    fn args_match(&self, args: &[Type]) -> bool {
        if let ArgLayout::List(list) = &self.args {
            if list.len() != args.len() {
                return false;
            }
        }
        for i in 0..self.args.args().len() {
            if self.args.args()[i] != args[i] {
                return false;
            }
        }
        true
    }
}

#[derive(Debug, Clone)]
pub struct TempVar(pub Vec<(Type, usize)>);
impl TempVar {
    fn new() -> Self {
        Self(vec![])
    }
    fn get(&self, typ: &Type) -> Option<usize> {
        self.0
            .iter()
            .find_map(|(t, n)| if *t == *typ { Some(*n) } else { None })
    }
    fn get_mut(&mut self, typ: &Type) -> Option<&mut usize> {
        self.0
            .iter_mut()
            .find_map(|(t, n)| if *t == *typ { Some(n) } else { None })
    }
    fn add(&mut self, typ: Type) {
        self.add_num(typ, 1);
    }
    fn add_num(&mut self, typ: Type, num: usize) {
        match self.get_mut(&typ) {
            Some(n) => {
                *n += num;
            }
            None => {
                self.0.push((typ, num));
            }
        }
    }
    fn merge(&self, other: &Self) -> Self {
        let mut types = vec![];
        for (typ, _) in self.0.iter() {
            if !types.contains(typ) {
                types.push(*typ);
            }
        }
        for (typ, _) in other.0.iter() {
            if !types.contains(typ) {
                types.push(*typ);
            }
        }

        let mut result = Self::new();
        for typ in types {
            let num1 = self.get(&typ).unwrap_or(0);
            let num2 = other.get(&typ).unwrap_or(0);
            let num = num1.max(num2);
            assert!(num != 0, "number of types should not equal 0");
            result.0.push((typ, num));
        }

        result
    }
}
#[derive(Debug, Clone)]
pub struct AnalysisInfo {
    pub declared_vars: HashMap<Id, DeclaredVar>,
    pub declared_funs: HashMap<Id, DeclaredFun>,
    pub temp_vars: TempVar,
}
impl AnalysisInfo {
    pub fn new() -> Self {
        let declared_vars = HashMap::new();
        let mut declared_funs = HashMap::new();
        let printf = DeclaredFun {
            args: ArgLayout::Variable(vec![Type::String]),
            ret: Type::Void,
        };
        let exit = DeclaredFun {
            args: ArgLayout::List(vec![Type::U32]),
            ret: Type::Void,
        };
        declared_funs.insert(Rc::new(String::from("printf")).into(), printf);
        declared_funs.insert(Rc::new(String::from("exit")).into(), exit);

        let temp_vars = TempVar::new();
        Self {
            declared_vars,
            declared_funs,
            temp_vars,
        }
    }
}

pub fn analyse(block: &Block) -> Result<AnalysisInfo, String> {
    let mut info = AnalysisInfo::new();
    let mut largest_temp = info.temp_vars.clone();
    for instruction in block.0.iter() {
        info.temp_vars.0.clear();
        analyse_instruction(instruction, &mut info)?;
        largest_temp = largest_temp.merge(&info.temp_vars);
    }
    info.temp_vars = largest_temp;
    Ok(info)
}

pub fn analyse_expr(expr: &Expression, info: &mut AnalysisInfo) -> Result<Type, String> {
    match expr {
        Expression::Number(_) => Ok(Type::U32),
        Expression::String(_string) => {
            // strings.add(string);
            Ok(Type::String)
        }
        Expression::Ident(id) => match info.declared_vars.get(id) {
            Some(dec) => Ok(dec.typ),
            None => Err(format!("undefined variable `{id}`")),
        },
        Expression::Operation { lhs, op, rhs } => {
            let lhs_type = analyse_expr(lhs, info)?;
            let rhs_type = analyse_expr(rhs, info)?;
            if rhs.is_recursive() {
                info.temp_vars.add(lhs_type);
            }

            if lhs_type == rhs_type {
                let typ = if op.is_comparator() {
                    Type::Bool
                } else {
                    lhs_type
                };
                Ok(typ)
            } else {
                Err(format!(
                    "tried invalid operation `{lhs_type} {op} {rhs_type}`"
                ))
            }
        }
        Expression::Index { expr, index } => {
            let typ = analyse_expr(expr, info)?;
            let i = analyse_expr(index, info)?;

            if i == Type::U32 {
                Ok(typ)
            } else {
                Err(format!("index must have type of `u32` but got `{i}`"))
            }
        }
        Expression::Function { name, args } => {
            let mut arg_typs = vec![];
            for expr in args.iter() {
                let typ = analyse_expr(expr, info)?;
                arg_typs.push(typ);
            }
            match info.declared_funs.get(name) {
                Some(fun) => {
                    if !fun.args_match(&arg_typs) {
                        return Err(format!(
                            "function `{name}` expects `{:?}` but got `{:?}`",
                            fun.args, args
                        ));
                    }
                }
                None => {
                    return Err(format!("unkown function `{name}`"));
                }
            }
            Ok(Type::Void)
        }
    }
}

pub fn analyse_block(block: &Block, info: &mut AnalysisInfo) -> Result<(), String> {
    for instruction in block.0.iter() {
        analyse_instruction(instruction, info)?;
    }
    Ok(())
}

pub fn analyse_instruction(
    instruction: &Instruction,
    info: &mut AnalysisInfo,
) -> Result<(), String> {
    match instruction {
        Instruction::Declare { id, value } => {
            if !info.declared_vars.contains_key(id) {
                let typ = analyse_expr(value, info)?;
                if typ == Type::Void {
                    Err(format!("can't declare `{id}` as having type of `void`"))
                } else {
                    info.declared_vars.insert(id.clone(), DeclaredVar { typ });
                    Ok(())
                }
            } else {
                Err(format!("duplicate declaration of `{id}`"))
            }
        }
        Instruction::Assign { id, value } => {
            if info.declared_vars.contains_key(id) {
                analyse_expr(value, info)?;
                Ok(())
            } else {
                Err(format!("assignment of `{id}` before declaration"))
            }
        }
        Instruction::Expr(expr) => {
            analyse_expr(expr, info)?;
            Ok(())
        }
        Instruction::Loop(block) => analyse_block(block, info),
        Instruction::While { expr, block } => {
            let typ = analyse_expr(expr, info)?;
            if typ != Type::Bool {
                return Err(format!("while loop expects type `bool` by got `{typ}`"));
            }
            for instruction in block.0.iter() {
                analyse_instruction(instruction, info)?;
            }
            Ok(())
        }
    }
}
