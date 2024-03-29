use std::{ffi::CString, fmt::Display, rc::Rc};

use crate::tokenizer::{Id, Operation, Token, Tokenizer, Type};

#[derive(Debug, Clone)]
pub struct DeclaredVars(pub Vec<(Id, Type)>);
impl DeclaredVars {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn get(&self, id: &Id) -> Option<Type> {
        self.0.iter().rev().find_map(|(vars_id, typ)| {
            if *vars_id == *id {
                Some(typ.clone())
            } else {
                None
            }
        })
    }
    pub fn contains(&self, id: &Id) -> bool {
        self.0.iter().rev().any(|(var_id, _)| *var_id == *id)
    }
    pub fn add(&mut self, id: Id, typ: Type) {
        if self.contains(&id) {
            panic!("Vars already has `{id}` of type `{typ}`");
        }
        self.0.push((id, typ))
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub vars: DeclaredVars,
    pub instructions: Vec<Rc<Instruction>>,
}

impl Block {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            vars: DeclaredVars::new(),
        }
    }
}
impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for (id, typ) in self.vars.0.iter() {
            writeln!(f, "\t// var {id}: {typ}")?;
        }
        if !self.vars.0.is_empty() {
            writeln!(f)?;
        }
        for instruction in self.instructions.iter() {
            let instruction = format!("{instruction}");
            for line in instruction.split('\n') {
                writeln!(f, "\t{line}")?;
            }
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Declare {
        id: Id,
        value: Rc<Expression>,
    },
    Assign {
        id: Id,
        value: Rc<Expression>,
    },
    Expr(Rc<Expression>),
    Block(Rc<Block>),
    Loop(Rc<Block>),
    While {
        expr: Rc<Expression>,
        block: Rc<Block>,
    },
    If {
        expr: Rc<Expression>,
        block: Rc<Block>,
    },
}
impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Declare { id, value } => write!(f, "var {id} = {value};"),
            Instruction::Assign { id, value } => write!(f, "{id} = {value};"),
            Instruction::Expr(expr) => write!(f, "{expr};"),
            Instruction::Loop(block) => write!(f, "loop\n{block}"),
            Instruction::While { expr, block } => write!(f, "while {expr}\n{block}"),
            Instruction::Block(block) => write!(f, "{block}"),
            Instruction::If { expr, block } => write!(f, "if {expr}\n{block}"),
        }
    }
}
#[derive(Debug, Clone)]
pub enum Expression {
    Number(u32),
    String(Rc<CString>),
    Ident(Id),
    Operation {
        lhs: Rc<Self>,
        op: Operation,
        rhs: Rc<Self>,
    },
    Index {
        base: Rc<Self>,
        index: Rc<Self>,
        size: u32
    },
    FunctionCall {
        name: Id,
        args: Vec<Rc<Self>>,
    },
    Deref(Rc<Self>),
}
impl Expression {
    pub fn is_recursive(&self) -> bool {
        matches!(
            self,
            Expression::Operation { .. }
                | Expression::Index { .. }
                | Expression::FunctionCall { .. }
        )
    }
    pub fn is_const(&self) -> bool {
        matches!(self, Expression::Number(_))
    }
    pub fn is_bool(&self) -> bool {
        matches!(
            self,
            Expression::Operation {
                op: Operation::Bool(_),
                ..
            }
        )
    }
    pub fn is_next_bool(&self) -> bool {
        match self {
            Expression::Operation { lhs, op: _, rhs } => lhs.is_bool() && rhs.is_bool(),
            _ => false,
        }
    }
}
impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Number(n) => write!(f, "{n}"),
            Expression::String(string) => write!(f, "{string:?}"),
            Expression::Ident(id) => write!(f, "{id}"),
            Expression::Operation { lhs, op, rhs } => write!(f, "({lhs} {op} {rhs})"),
            Expression::Index { base, index, size} => write!(f, "{base}[{index}] // size: {size}"),
            Expression::FunctionCall { name, args } => {
                if !args.is_empty() {
                    let last = args.len() - 1;
                    write!(f, "{name}(")?;
                    for arg in args[..last].iter() {
                        write!(f, "{arg}, ")?;
                    }
                    write!(f, "{})", args[last])
                } else {
                    write!(f, "{name}()")
                }
            }
            Expression::Deref(expr) => {
                write!(f, "@{expr}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Id,
    pub args: Vec<(Id, Type)>,
    pub block: Rc<Block>,
    pub ret: Type,
}
impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}(", self.name.0)?;
        if self.args.is_empty() {
            write!(f, ")")?;
        } else {
            let last = self.args.len() - 1;
            for (id, typ) in &self.args[..last] {
                write!(f, "{id}: {typ}, ")?;
            }
            let (id, typ) = &self.args[last];
            write!(f, "{id}: {typ})")?;
        }
        // if self.ret != Type::Void {
        write!(f, " -> {}", self.ret)?;
        // }
        write!(f, "\n{}", self.block)
    }
}

#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub enum AST {
    Instruction(Rc<Instruction>),
    Block(Rc<Block>),
    Expression(Rc<Expression>),
    Function(Function),
}
impl Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AST::Instruction(i) => write!(f, "{i}"),
            AST::Block(b) => write!(f, "{b}"),
            AST::Expression(e) => write!(f, "{e}"),
            AST::Function(fun) => write!(f, "{fun}"),
        }
    }
}
impl From<Rc<Block>> for AST {
    fn from(value: Rc<Block>) -> Self {
        Self::Block(value)
    }
}
impl From<Rc<Instruction>> for AST {
    fn from(value: Rc<Instruction>) -> Self {
        Self::Instruction(value)
    }
}
impl From<Rc<Expression>> for AST {
    fn from(value: Rc<Expression>) -> Self {
        Self::Expression(value)
    }
}
impl From<Function> for AST {
    fn from(value: Function) -> Self {
        Self::Function(value)
    }
}

#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub enum Node {
    Token(Token),
    AST(AST),
}
impl From<Token> for Node {
    fn from(value: Token) -> Self {
        Self::Token(value)
    }
}
impl From<AST> for Node {
    fn from(value: AST) -> Self {
        Self::AST(value)
    }
}
#[derive(Debug, Clone)]
struct Nodes(Vec<Node>);
impl Nodes {
    fn new() -> Self {
        Self(Vec::new())
    }
    fn reduce(&mut self, offset: usize) {
        self.0.truncate(self.0.len() - offset);
    }
    fn push(&mut self, node: impl Into<Node>) {
        let node = node.into();
        self.0.push(node);
    }
}
impl Display for Nodes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for node in self.0.iter() {
            match node {
                Node::Token(t) => {
                    writeln!(f, "Token: {t}")?;
                }
                Node::AST(a) => {
                    writeln!(f, "{a}")?;
                }
            }
        }
        std::fmt::Result::Ok(())
    }
}

fn parse_block(nodes: &mut Nodes) -> bool {
    let len = nodes.0.len();
    if len < 2 {
        return false;
    }

    let last = len - 1;
    // block end
    if let Node::Token(Token::BClose) = nodes.0[last] {
        let mut i = last - 1;
        let mut block = Block::new();
        loop {
            match &nodes.0[i] {
                // block start
                Node::Token(Token::BOpen) => {
                    block.instructions.reverse();
                    let node: AST = Rc::new(block).into();
                    nodes.reduce(last - i + 1);
                    nodes.push(node);
                    return true;
                }
                // instruction
                Node::AST(AST::Instruction(instruction)) => {
                    block.instructions.push(instruction.clone());
                }
                Node::Token(Token::BClose) => {
                    // panic!();
                    return false;
                }
                // error
                _ => {
                    return false;
                }
            }
            // reached end so error
            if i == 0 {
                return false;
            }
            i -= 1;
        }
    } else {
        #[allow(clippy::needless_return)]
        return false;
    }
}

fn parse_function(nodes: &mut Nodes) -> bool {
    let len = nodes.0.len();
    if len < 4 {
        return false;
    }
    let last = len - 1;
    // end of function
    if let Node::Token(Token::PClose) = nodes.0[last] {
        let mut i = last - 1;
        let mut args = vec![];

        if let Node::AST(AST::Expression(expr)) = &nodes.0[i] {
            i -= 1;
            args.push(expr.clone());
        }

        loop {
            match &nodes.0[i] {
                Node::Token(Token::Comma) => {
                    if let Some(Node::AST(AST::Expression(expr))) = &nodes.0.get(i - 1) {
                        args.push(expr.clone());
                    } else {
                        return false;
                    }
                }
                Node::Token(Token::POpen) => {
                    i -= 1;
                    break;
                }
                _ => {
                    return false;
                }
            }

            if i <= 1 {
                return false;
            }
            i -= 2;
        }

        if let Some(Node::Token(Token::Ident(id))) = &nodes.0.get(i) {
            args.reverse();
            let node: AST = Rc::new(Expression::FunctionCall {
                name: id.clone(),
                args,
            })
            .into();
            nodes.reduce(last - i + 1);
            nodes.push(node);
            true
        } else {
            false
        }
    } else {
        false
    }
}

fn get_arg(nodes: &Nodes, from: usize) -> Option<(Id, Type)> {
    if let Some(Node::Token(Token::Type(typ))) = &nodes.0.get(from) {
        if let Some(Node::Token(Token::Colon)) = &nodes.0.get(from - 1) {
            if let Some(Node::Token(Token::Ident(id))) = &nodes.0.get(from - 2) {
                return Some((id.clone(), typ.clone()));
            }
        }
    }
    None
}

fn parse_function_declaration(nodes: &mut Nodes) -> bool {
    let len = nodes.0.len();
    if len < 6 {
        return false;
    }
    let last = len - 1;
    let mut ret = Type::Void;
    // end of function (block)
    if let Node::AST(AST::Block(block)) = &nodes.0[last] {
        let mut i = last - 1;
        if let Node::Token(Token::Type(typ)) = &nodes.0[i] {
            i -= 1;
            if let Node::Token(Token::Arrow) = nodes.0[i] {
                i -= 1;
                ret = typ.clone();
            } else {
                return false;
            }
        }

        if let Node::Token(Token::PClose) = nodes.0[i] {
            i -= 1;
            let mut args = vec![];

            if let Some(arg) = get_arg(nodes, i) {
                i -= 3;
                args.push(arg);
            }

            if let Node::Token(Token::Type(Type::Void)) = &nodes.0[i] {
                i -= 1;
                if let Node::Token(Token::POpen) = &nodes.0[i] {
                    i -= 1;
                } else {
                    return false;
                }
            } else {
                loop {
                    match &nodes.0[i] {
                        Node::Token(Token::Comma) => {
                            if let Some(arg) = get_arg(nodes, i - 1) {
                                i -= 3;
                                args.push(arg);
                            } else {
                                return false;
                            }
                        }
                        Node::Token(Token::POpen) => {
                            i -= 1;
                            break;
                        }
                        _ => {
                            return false;
                        }
                    }
                    if i == 0 {
                        return false;
                    }
                    i -= 1;
                }
            }
            if let Some(Node::Token(Token::Ident(id))) = &nodes.0.get(i) {
                i -= 1;
                if let Some(Node::Token(Token::Fun)) = &nodes.0.get(i) {
                    args.reverse();
                    let node: AST = Function {
                        name: id.clone(),
                        args,
                        block: block.clone(),
                        ret,
                    }
                    .into();
                    nodes.reduce(last - i + 1);
                    nodes.push(node);
                }
            }

            false
        } else {
            false
        }
    } else {
        false
    }
}
// use Expression::*;
// use Instruction::*;
// use Token::*;
// use Node::*;
// use AST::*;
pub fn parse(code: &str) -> Result<Vec<Function>, String> {
    let mut tokenizer = Tokenizer::new(code).peekable();

    let mut nodes = Nodes::new();

    while let Some(token) = tokenizer.next() {
        nodes.push(token);

        loop {
            let repeat = if parse_function(&mut nodes)
                || parse_block(&mut nodes)
                || parse_function_declaration(&mut nodes)
            {
                true
            } else {
                match &nodes.0[..] {
                    // string -> expr
                    [.., Node::Token(Token::String(string))] => {
                        let node: AST = Rc::new(Expression::String(Rc::new(
                            CString::new(string.as_bytes()).unwrap(),
                        )))
                        .into();
                        nodes.reduce(1);
                        nodes.push(node);
                        true
                    }
                    // number -> expr
                    [.., Node::Token(Token::Number(num))] => {
                        let node: AST = Rc::new(Expression::Number(*num)).into();
                        nodes.reduce(1);
                        nodes.push(node);
                        true
                    }
                    // ident -> expr
                    [.., Node::Token(Token::Ident(id))] => {
                        if let Some(Token::Colon | Token::Equal | Token::POpen) = tokenizer.peek() {
                            false
                        } else {
                            let node: AST = Rc::new(Expression::Ident(id.clone())).into();
                            nodes.reduce(1);
                            nodes.push(node);
                            true
                        }
                    }
                    // [typ] -> typ
                    [.., Node::Token(Token::SOpen), Node::Token(Token::Type(typ)), Node::Token(Token::SClose)] =>
                    {
                        let node = Token::Type(Type::Array(Box::new(typ.clone())));
                        nodes.reduce(3);
                        nodes.push(node);
                        true
                    }
                    // #typ -> typ
                    [.., Node::Token(Token::Ref), Node::Token(Token::Type(typ))] => {
                        let node = Token::Type(Type::Ptr(Box::new(typ.clone())));
                        nodes.reduce(2);
                        nodes.push(node);
                        true
                    }
                    // #expr -> expr
                    [.., Node::Token(Token::Ref), Node::AST(AST::Expression(_expr))] => {
                        todo!()
                    }
                    // @expr -> expr
                    [.., Node::Token(Token::Deref), Node::AST(AST::Expression(expr))] => {
                        let node: AST = Rc::new(Expression::Deref(expr.clone())).into();
                        nodes.reduce(2);
                        nodes.push(node);
                        true
                    }
                    // expr op expr
                    [.., Node::AST(AST::Expression(lhs)), Node::Token(Token::Operation(op)), Node::AST(AST::Expression(rhs))] =>
                    {
                        let mut valid = true;
                        if let Some(Token::Operation(next_op)) = tokenizer.peek() {
                            if op.precedence() < next_op.precedence() {
                                valid = false;
                            }
                        }
                        if valid {
                            let node: AST = Rc::new(Expression::Operation {
                                lhs: lhs.clone(),
                                op: *op,
                                rhs: rhs.clone(),
                            })
                            .into();
                            nodes.reduce(3);
                            nodes.push(node);
                            true
                        } else {
                            false
                        }
                    }
                    // expr[expr]
                    [.., Node::AST(AST::Expression(expr)), Node::Token(Token::SOpen), Node::AST(AST::Expression(index)), Node::Token(Token::SClose)] =>
                    {
                        let node: AST = Rc::new(Expression::Index {
                            base: expr.clone(),
                            index: index.clone(),
                            size:0,
                        })
                        .into();
                        nodes.reduce(4);
                        nodes.push(node);
                        true
                    }
                    // (expr) -> expr
                    [.., Node::Token(Token::POpen), Node::AST(AST::Expression(expr)), Node::Token(Token::PClose)] =>
                    {
                        let node: AST = expr.clone().into();
                        nodes.reduce(3);
                        nodes.push(node);
                        true
                    }
                    // declare
                    [.., Node::Token(Token::Var), Node::Token(Token::Ident(id)), Node::Token(Token::Equal), Node::AST(AST::Expression(expr)), Node::Token(Token::Semicolon)] =>
                    {
                        let node: AST = Rc::new(Instruction::Declare {
                            id: id.clone(),
                            value: expr.clone(),
                        })
                        .into();
                        nodes.reduce(5);
                        nodes.push(node);
                        true
                    }
                    // assign
                    [.., Node::Token(Token::Ident(id)), Node::Token(Token::Equal), Node::AST(AST::Expression(expr)), Node::Token(Token::Semicolon)] =>
                    {
                        let node: AST = Rc::new(Instruction::Assign {
                            id: id.clone(),
                            value: expr.clone(),
                        })
                        .into();
                        nodes.reduce(4);
                        nodes.push(node);
                        true
                    }
                    // empty block
                    [.., Node::Token(Token::BOpen), Node::Token(Token::BClose)] => {
                        let node: AST = Rc::new(Block::new()).into();
                        nodes.reduce(2);
                        nodes.push(node);
                        true
                    }
                    // while
                    [.., Node::Token(Token::While), Node::AST(AST::Expression(expr)), Node::AST(AST::Block(block))] =>
                    {
                        let node: AST = Rc::new(Instruction::While {
                            expr: expr.clone(),
                            block: block.clone(),
                        })
                        .into();
                        nodes.reduce(3);
                        nodes.push(node);
                        true
                    }
                    // if
                    [.., Node::Token(Token::If), Node::AST(AST::Expression(expr)), Node::AST(AST::Block(block))] =>
                    {
                        let node: AST = Rc::new(Instruction::If {
                            expr: expr.clone(),
                            block: block.clone(),
                        })
                        .into();
                        nodes.reduce(3);
                        nodes.push(node);
                        true
                    }
                    // loop
                    [.., Node::Token(Token::Loop), Node::AST(AST::Block(block))] => {
                        let node: AST = Rc::new(Instruction::Loop(block.clone())).into();
                        nodes.reduce(2);
                        nodes.push(node);
                        true
                    }

                    // expr;
                    [.., Node::AST(AST::Expression(expr)), Node::Token(Token::Semicolon)] => {
                        let node: AST = Rc::new(Instruction::Expr(expr.clone())).into();
                        nodes.reduce(2);
                        nodes.push(node);
                        true
                    }
                    // block -> expr
                    [.., Node::AST(AST::Block(block))] => {
                        if tokenizer.peek().is_some() {
                            let node: AST = Rc::new(Instruction::Block(block.clone())).into();
                            nodes.reduce(1);
                            nodes.push(node);
                            true
                        } else {
                            false
                        }
                    }
                    _ => false,
                }
            };
            if !repeat {
                break;
            }
        }
    }
    let err = format!("not a list of functions:\n{nodes}");
    nodes
        .0
        .into_iter()
        .try_fold(vec![], |mut acc, function| {
            if let Node::AST(AST::Function(fun)) = function {
                acc.push(fun);
                Ok(acc)
            } else {
                Err(())
            }
        })
        .map_err(|_| err)
}
