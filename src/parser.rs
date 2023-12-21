use std::{ffi::CString, rc::Rc};

use super::tokenizer::*;

type Block = Option<Rc<Instruction>>;

#[derive(Debug, Clone)]
pub enum Instruction {
    Declare {
        id: Rc<String>,
        value: Rc<Expression>,
    },
    Assign {
        id: Rc<String>,
        value: Rc<Expression>,
    },
    Expr(Rc<Expression>),
    Binary {
        lhs: Rc<Self>,
        rhs: Rc<Self>,
    },
    Loop(Block),
    While {
        expr: Rc<Expression>,
        block: Block,
    },
}
#[derive(Debug, Clone)]
pub enum Expression {
    Number(u32),
    String(Rc<CString>),
    Ident(Rc<String>),
    Operation {
        lhs: Rc<Expression>,
        op: Operation,
        rhs: Rc<Expression>,
    },
    Index {
        expr: Rc<Expression>,
        index: Rc<Expression>,
    },
    Function {
        name: Rc<String>,
        arg: Rc<Expression>,
    },
}

#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub enum AST {
    Instruction(Rc<Instruction>),
    Block(Block),
    Expression(Rc<Expression>),
}
impl From<Block> for AST {
    fn from(value: Block) -> Self {
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
struct Nodes {
    nodes: Vec<Node>,
}
impl Nodes {
    fn new() -> Self {
        Self { nodes: Vec::new() }
    }
    fn reduce(&mut self, offset: usize) {
        self.nodes.truncate(self.nodes.len() - offset);
    }
    fn push(&mut self, node: impl Into<Node>) {
        let node = node.into();
        self.nodes.push(node);
    }
}

// use Expression::*;
// use Instruction::*;
// use Node::*;
// use Token::*;
// use AST::*;
pub fn parse(code: &str) -> Result<Rc<Instruction>, String> {
    let mut tokenizer = Tokenizer::new(code).peekable();

    let mut nodes = Nodes::new();

    while let Some(token) = tokenizer.next() {
        nodes.push(token);

        loop {
            let repeat = match &nodes.nodes[..] {
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
                [.., Node::AST(AST::Expression(expr)), Node::Token(Token::SOpen), Node::AST(AST::Expression(index)), Node::Token(Token::BClose)] =>
                {
                    let node: AST = Rc::new(Expression::Index {
                        expr: expr.clone(),
                        index: index.clone(),
                    })
                    .into();
                    nodes.reduce(4);
                    nodes.push(node);
                    true
                }
                // exit(expr) -> expr
                [.., Node::Token(Token::Exit), Node::Token(Token::POpen), Node::AST(AST::Expression(expr)), Node::Token(Token::PClose)] =>
                {
                    // let node: AST = Rc::new(Instruction::Exit(expr.clone())).into();
                    let node: AST = Rc::new(Expression::Function {
                        name: Rc::new(String::from("exit")),
                        arg: expr.clone(),
                    })
                    .into();
                    nodes.reduce(4);
                    nodes.push(node);
                    true
                }
                // print(expr) -> expr
                [.., Node::Token(Token::Print), Node::Token(Token::POpen), Node::AST(AST::Expression(expr)), Node::Token(Token::PClose)] =>
                {
                    // let node: AST = Rc::new(Instruction::Print(expr.clone())).into();
                    let node: AST = Rc::new(Expression::Function {
                        name: Rc::new(String::from("print")),
                        arg: expr.clone(),
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
                // combine instructions
                [.., Node::AST(AST::Instruction(lhs)), Node::AST(AST::Instruction(rhs))] => {
                    let node: AST = Rc::new(Instruction::Binary {
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    })
                    .into();
                    nodes.reduce(2);
                    nodes.push(node);
                    true
                }
                // empty block
                [.., Node::Token(Token::BOpen), Node::Token(Token::BClose)] => {
                    let node: AST = None.into();
                    nodes.reduce(2);
                    nodes.push(node);
                    true
                }
                // block
                [.., Node::Token(Token::BOpen), Node::AST(AST::Instruction(instruction)), Node::Token(Token::BClose)] =>
                {
                    let node: AST = Some(instruction.clone()).into();
                    nodes.reduce(3);
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
                _ => false,
            };
            if !repeat {
                break;
            }
        }
    }

    if nodes.nodes.len() != 1 {
        let nodes: Vec<&Node> = nodes.nodes.iter().skip(1).collect();
        Err(format!("Node list is not 1! Nodes not reduced: {nodes:#?}"))
    } else if let Node::AST(AST::Instruction(instructions)) = nodes.nodes.pop().unwrap() {
        Ok(instructions)
    } else {
        Err(format!("last node is not an instructions: {nodes:#?}"))
    }
}
