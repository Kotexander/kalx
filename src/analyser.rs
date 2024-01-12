use std::rc::Rc;

use crate::{
    parser::{Block, DeclaredVars, Expression, Function, Instruction},
    tokenizer::{Id, Type},
};

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
    fn add(&mut self, name: Id, args: ArgLayout, ret: Type) {
        if self.get(&name).is_some() {
            panic!()
        } else {
            self.0.push((name, DeclaredFun { args, ret }));
        }
    }
    fn get(&self, name: &Id) -> Option<&DeclaredFun> {
        self.0.iter().find_map(
            |(fun_id, fun)| {
                if *fun_id == *name {
                    Some(fun)
                } else {
                    None
                }
            },
        )
    }
}

#[derive(Debug, Clone)]
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
    expr: &mut Expression,
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
            let lhs_type = analyse_expr(Rc::get_mut(lhs).unwrap(), funs, all_vars)?;
            let rhs_type = analyse_expr(Rc::get_mut(rhs).unwrap(), funs, all_vars)?;

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
        Expression::Index { base: expr, index, size } => {
            let typ = analyse_expr(Rc::get_mut(expr).unwrap(), funs, all_vars)?;
            if let Type::Array(typ) = typ {
                let i = analyse_expr(Rc::get_mut(index).unwrap(), funs, all_vars)?;
    
                if i == Type::U32 {
                    *size = typ.size();
                    Ok(*typ)
                } else {
                    Err(format!("index must have type of `u32` but got `{i}`"))
                }
            }
            else {
                Err(format!("tried indexing `{typ}`"))
            }
        }
        Expression::FunctionCall { name, args } => {
            let mut arg_typs = vec![];
            for expr in args.iter_mut() {
                let typ = analyse_expr(Rc::get_mut(expr).unwrap(), funs, all_vars)?;
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
                        Ok(fun.ret.clone())
                    }
                }
                None => Err(format!("unkown function `{name}`")),
            }
        }
        Expression::Deref(expr) => {
            let typ = analyse_expr(Rc::get_mut(expr).unwrap(), funs, all_vars)?;
            if let Type::Ptr(typ) = typ {
                Ok(*typ)
            }
            else {
                Err(format!("tried dereferencing a `{typ}`"))
            }
        },
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
                let typ = analyse_expr(Rc::get_mut(value).unwrap(), funs, &all_vars)?;
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
            if let Some(var) = all_vars.get(id) {
                let typ = analyse_expr(Rc::get_mut(value).unwrap(), funs, &all_vars)?;
                if var == typ {
                    Ok(())
                } else {
                    Err(format!(
                        "tried assigning `{typ}` to `{id}` which is of type `{var}`"
                    ))
                }
            } else {
                Err(format!("assignment of `{id}` before declaration"))
            }
        }
        Instruction::Expr(expr) => {
            analyse_expr(Rc::get_mut(expr).unwrap(), funs, &all_vars)?;
            Ok(())
        }
        Instruction::Loop(block) => analyse_block(Rc::get_mut(block).unwrap(), funs, &all_vars),
        Instruction::While { expr, block } => {
            let typ = analyse_expr(Rc::get_mut(expr).unwrap(), funs, &all_vars)?;
            if typ != Type::Bool {
                return Err(format!("while loop expects type `bool` but got `{typ}`"));
            }
            analyse_block(Rc::get_mut(block).unwrap(), funs, &all_vars)
        }
        Instruction::Block(block) => analyse_block(Rc::get_mut(block).unwrap(), funs, &all_vars),
        Instruction::If { expr, block } => {
            let typ = analyse_expr(Rc::get_mut(expr).unwrap(), funs, &all_vars)?;
            if typ != Type::Bool {
                return Err(format!("if statement expects type `bool` but got `{typ}`"));
            }
            analyse_block(Rc::get_mut(block).unwrap(), funs, &all_vars)
        }
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

pub fn analyse(functions: &mut [Function]) -> Result<(), String> {
    let mut funs = DeclaredFuns::new();
    for function in functions.iter() {
        let args = ArgLayout::List(function.args.iter().map(|(_, typ)| typ.clone()).collect());
        funs.add(
            function.name.clone(),
            args,
            function.ret.clone(),
        );
    }
    for function in functions.iter_mut() {
        let mut function_args = DeclaredVars::new();
        for (name, typ) in function.args.iter() {
            function_args.add(name.clone(), typ.clone());
        }
        let mut all_vars = AllVars::new();
        all_vars.vars.push(&function_args);
        analyse_block(Rc::get_mut(&mut function.block).unwrap(), &funs, &all_vars)?;
    }

    Ok(())
}
