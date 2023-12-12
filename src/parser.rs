use std::rc::Rc;

use super::tokenizer::*;

type Block = Option<Rc<Instruction>>;

#[derive(Debug, Clone)]
pub enum Instruction {
    Assign {
        id: Rc<String>,
        value: Rc<Expression>,
        typ: Option<Type>,
    },
    Exit(Rc<Expression>),
    Print(Rc<Expression>),
    Binary {
        lhs: Rc<Self>,
        rhs: Rc<Self>,
    },
    Loop(Block),
}
#[derive(Debug, Clone)]
pub enum Expression {
    Number(u32),
    String(Rc<String>),
    Ident(Rc<String>),
}

#[derive(Debug, Clone)]
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

pub fn parse(code: &str) -> Result<Rc<Instruction>, String> {
    let mut tokenizer = Tokenizer::new(code).peekable();

    let mut nodes = Nodes::new();

    while let Some(token) = tokenizer.next() {
        nodes.push(token);

        loop {
            let repeat = match &nodes.nodes[..] {
                // string -> expr
                [.., Node::Token(Token::String(string))] => {
                    let node: AST = Rc::new(Expression::String(string.clone())).into();
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
                    if let Some(Token::Colon | Token::Equal) = tokenizer.peek() {
                        false
                    } else {
                        let node: AST = Rc::new(Expression::Ident(id.clone())).into();
                        nodes.reduce(1);
                        nodes.push(node);
                        true
                    }
                }
                // exit
                [.., Node::Token(Token::Exit), Node::AST(AST::Expression(expr)), Node::Token(Token::Semicolon)] =>
                {
                    let node: AST = Rc::new(Instruction::Exit(expr.clone())).into();
                    nodes.reduce(3);
                    nodes.push(node);
                    true
                }
                // print
                [.., Node::Token(Token::Print), Node::AST(AST::Expression(expr)), Node::Token(Token::Semicolon)] =>
                {
                    let node: AST = Rc::new(Instruction::Print(expr.clone())).into();
                    nodes.reduce(3);
                    nodes.push(node);
                    true
                }
                // assign
                [.., Node::Token(Token::Var), Node::Token(Token::Ident(id)), Node::Token(Token::Equal), Node::AST(AST::Expression(expr)), Node::Token(Token::Semicolon)] =>
                {
                    let node: AST = Rc::new(Instruction::Assign {
                        id: id.clone(),
                        value: expr.clone(),
                        typ: None,
                    })
                    .into();
                    nodes.reduce(5);
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
                [.., Node::Token(Token::Open), Node::Token(Token::Close)] => {
                    let node: AST = None.into();
                    nodes.reduce(2);
                    nodes.push(node);
                    true
                }
                // block
                [.., Node::Token(Token::Open), Node::AST(AST::Instruction(instruction)), Node::Token(Token::Close)] =>
                {
                    let node: AST = Some(instruction.clone()).into();
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
                _ => false,
            };
            if !repeat {
                break;
            }
        }
    }

    if nodes.nodes.len() != 1 {
        Err(format!("node list is not 1: {nodes:#?}"))
    } else {
        if let Node::AST(AST::Instruction(instructions)) = nodes.nodes.pop().unwrap() {
            Ok(instructions)
        } else {
            Err(format!("last node is not an instructions: {nodes:#?}"))
        }
    }
}
