use std::rc::Rc;

use crate::{
    parser::{Block, DeclaredVars, Expression, Instruction},
    tokenizer::{Id, Type},
};

// #[derive(Debug, Clone)]
// pub struct DeclaredVars(Vec<(Id, Type)>);
// impl DeclaredVars {
//     fn new() -> Self {
//         Self(vec![])
//     }
//     fn get(&self, id: &Id) -> Option<Type> {
//         self.0
//             .iter()
//             .rev()
//             .find_map(|(vars_id, typ)| if *vars_id == *id { Some(*typ) } else { None })
//     }
//     fn contains(&self, id: &Id) -> bool {
//     self.0.iter().rev().any(|(var_id, _)| *var_id == *id)
//     }
//     fn add(&mut self, id: Id, typ: Type) {
//         // if self.contains(&id) {
//         // panic!("Vars already has `{id}` of type `{typ}`");
//         // }
//         self.0.push((id, typ))
//     }
// }

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
    args: ArgLayout,
    ret: Type,
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
pub struct DeclaredFuns(Vec<(Id, DeclaredFun)>);
impl DeclaredFuns {
    fn new() -> Self {
        let mut funs = vec![];
        let printf = DeclaredFun {
            args: ArgLayout::Variable(vec![Type::String]),
            ret: Type::Void,
        };
        let exit = DeclaredFun {
            args: ArgLayout::List(vec![Type::U32]),
            ret: Type::Void,
        };
        funs.push((Rc::new(String::from("printf")).into(), printf));
        funs.push((Rc::new(String::from("exit")).into(), exit));

        Self(funs)
    }
    fn get(&self, id: &Id) -> Option<&DeclaredFun> {
        self.0.iter().find_map(
            |(fun_id, fun)| {
                if *fun_id == *id {
                    Some(fun)
                } else {
                    None
                }
            },
        )
    }
}

struct AllVars<'a> {
    vars: Vec<&'a DeclaredVars>,
}
impl<'a> AllVars<'a> {
    fn new() -> Self {
        Self { vars: vec![] }
    }
    fn get(&self, id: &Id) -> Option<Type> {
        for vars in self.vars.iter().rev() {
            let typ = vars.get(id);
            if typ.is_some() {
                return typ;
            }
        }
        None
    }
    fn contains(&self, id: &Id) -> bool {
        for vars in self.vars.iter().rev() {
            if vars.contains(id) {
                return true;
            }
        }
        false
    }
    fn extend(&'a self, vars: &'a DeclaredVars) -> Self {
        let mut all_vars = self.vars.clone();
        all_vars.push(vars);
        Self { vars: all_vars }
    }
}

fn analyse_expr(
    expr: &Expression,
    funs: &DeclaredFuns,
    all_vars: &AllVars<'_>,
) -> Result<Type, String> {
    match expr {
        Expression::Number(_) => Ok(Type::U32),
        Expression::String(_string) => Ok(Type::String),
        Expression::Ident(id) => match all_vars.get(id) {
            Some(typ) => Ok(typ),
            None => Err(format!("undefined variable `{id}`")),
        },
        Expression::Operation { lhs, op, rhs } => {
            let lhs_type = analyse_expr(lhs, funs, all_vars)?;
            let rhs_type = analyse_expr(rhs, funs, all_vars)?;

            if lhs_type == rhs_type {
                let typ = if op.is_bool_op() {
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
            let typ = analyse_expr(expr, funs, all_vars)?;
            let i = analyse_expr(index, funs, all_vars)?;

            if i == Type::U32 {
                Ok(typ)
            } else {
                Err(format!("index must have type of `u32` but got `{i}`"))
            }
        }
        Expression::Function { name, args } => {
            let mut arg_typs = vec![];
            for expr in args.iter() {
                let typ = analyse_expr(expr, funs, all_vars)?;
                arg_typs.push(typ);
            }
            match funs.get(name) {
                Some(fun) => {
                    if !fun.args_match(&arg_typs) {
                        Err(format!(
                            "function `{name}` expects `{:?}` but got `{:?}`",
                            fun.args, args
                        ))
                    } else {
                        Ok(fun.ret)
                    }
                }
                None => Err(format!("unkown function `{name}`")),
            }
        }
    }
}

fn analyse_instruction(
    instruction: &mut Instruction,
    funs: &DeclaredFuns,
    all_vars: &AllVars<'_>,
    vars: &mut DeclaredVars,
) -> Result<(), String> {
    let all_vars = all_vars.extend(vars);
    match instruction {
        Instruction::Declare { id, value } => {
            if !all_vars.contains(id) {
                let typ = analyse_expr(value, funs, &all_vars)?;
                if typ == Type::Void {
                    Err(format!("can't declare `{id}` as having type of `void`"))
                } else {
                    vars.add(id.clone(), typ);
                    Ok(())
                }
            } else {
                Err(format!(
                    "variable `{id}` is defined multiple times in the same scope"
                ))
            }
        }
        Instruction::Assign { id, value } => {
            if all_vars.contains(id) {
                analyse_expr(value, funs, &all_vars)?;
                Ok(())
            } else {
                Err(format!("assignment of `{id}` before declaration"))
            }
        }
        Instruction::Expr(expr) => {
            analyse_expr(expr, funs, &all_vars)?;
            Ok(())
        }
        Instruction::Loop(block) => analyse_block(Rc::get_mut(block).unwrap(), funs, &all_vars),
        Instruction::While { expr, block } => {
            let typ = analyse_expr(expr, funs, &all_vars)?;
            if typ != Type::Bool {
                return Err(format!("while loop expects type `bool` by got `{typ}`"));
            }
            analyse_block(Rc::get_mut(block).unwrap(), funs, &all_vars)
        }
        Instruction::Block(block) => analyse_block(Rc::get_mut(block).unwrap(), funs, &all_vars),
        Instruction::If { expr, block } => {
            let typ = analyse_expr(expr, funs, &all_vars)?;
            if typ != Type::Bool {
                return Err(format!("while loop expects type `bool` by got `{typ}`"));
            }
            analyse_block(Rc::get_mut(block).unwrap(), funs, &all_vars)
        },
        
    }
}

fn analyse_block(
    block: &mut Block,
    funs: &DeclaredFuns,
    all_vars: &AllVars<'_>,
) -> Result<(), String> {
    let mut vars = DeclaredVars::new();
    for instruction in block.instructions.iter_mut() {
        analyse_instruction(Rc::get_mut(instruction).unwrap(), funs, all_vars, &mut vars)?;
    }
    block.vars = vars;
    Ok(())
}

pub fn analyse(block: &mut Block) -> Result<(), String> {
    let funs = DeclaredFuns::new();
    let all_vars = AllVars::new();
    analyse_block(block, &funs, &all_vars)
}
