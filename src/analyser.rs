use super::*;

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
pub struct AnalysisInfo {
    pub declared_vars: HashMap<Rc<String>, DeclaredVar>,
    pub declared_funs: HashMap<Rc<String>, DeclaredFun>,
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
        declared_funs.insert(Rc::new(String::from("printf")), printf);
        declared_funs.insert(Rc::new(String::from("exit")), exit);

        Self {
            declared_vars,
            declared_funs,
        }
    }
}

pub fn analyse(block: &Block) -> Result<AnalysisInfo, String> {
    let mut info = AnalysisInfo::new();
    match analyse_block(block, &mut info) {
        Ok(_) => Ok(info),
        Err(e) => Err(e),
    }
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
            let lhs = analyse_expr(lhs, info)?;
            let rhs = analyse_expr(rhs, info)?;

            if lhs == rhs {
                Ok(if op.is_comparator() { Type::Bool } else { lhs })
            } else {
                Err(format!("tried invalid operation `{lhs} {op} {rhs}`"))
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
    for instruction in block.iter() {
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
            for instruction in block.iter() {
                analyse_instruction(instruction, info)?;
            }
            Ok(())
        }
    }
}
