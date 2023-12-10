use std::rc::Rc;

use super::tokenizer::*;

type Block = Option<Rc<Instruction>>;

#[derive(Debug, Clone)]
pub enum Instruction {
    Assign { id: Rc<String>, value: Rc<String> },
    Exit { num: u32 },
    Print { id: Rc<String> },
    Binary { lhs: Rc<Self>, rhs: Rc<Self> },
    Loop { block: Block },
}

#[derive(Debug, Clone)]
pub enum AST {
    Instruction(Rc<Instruction>),
    Block(Block),
}
impl From<Block> for AST {
    fn from(value: Block) -> Self {
        Self::Block(value)
    }
}
impl From<Rc<Instruction>> for AST {
    fn from(value: Rc<Instruction>) -> Self {
        Self::Instruction(value.clone())
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
    let tokenizer = Tokenizer::new(code);

    let mut nodes = Nodes::new();

    for token in tokenizer {
        nodes.push(token);

        loop {
            let repeat = match &nodes.nodes[..] {
                [.., Node::Token(Token::Exit), Node::Token(Token::Number(num)), Node::Token(Token::Semicolon)] =>
                {
                    let node: AST = Rc::new(Instruction::Exit { num: *num }).into();
                    nodes.reduce(3);
                    nodes.push(node);
                    true
                }
                [.., Node::Token(Token::Print), Node::Token(Token::Ident(id)), Node::Token(Token::Semicolon)] =>
                {
                    let node: AST = Rc::new(Instruction::Print { id: id.clone() }).into();
                    nodes.reduce(3);
                    nodes.push(node);
                    true
                }
                [.., Node::Token(Token::Var), Node::Token(Token::Ident(id)), Node::Token(Token::Equal), Node::Token(Token::String(string)), Node::Token(Token::Semicolon)] =>
                {
                    let node: AST = Rc::new(Instruction::Assign {
                        id: id.clone(),
                        value: string.clone(),
                    })
                    .into();
                    nodes.reduce(5);
                    nodes.push(node);
                    true
                }
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
                [.., Node::Token(Token::Open), Node::Token(Token::Close)] => {
                    let node: AST = None.into();
                    nodes.reduce(2);
                    nodes.push(node);
                    true
                }
                [.., Node::Token(Token::Open), Node::AST(AST::Instruction(instruction)), Node::Token(Token::Close)] =>
                {
                    let node: AST = Some(instruction.clone()).into();
                    nodes.reduce(3);
                    nodes.push(node);
                    true
                }
                [.., Node::Token(Token::Loop), Node::AST(AST::Block(block))] => {
                    let node: AST = Rc::new(Instruction::Loop {
                        block: block.clone(),
                    })
                    .into();
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
        Err(format!("node list is not 1:\n{nodes:#?}"))
    } else {
        if let Node::AST(AST::Instruction(instructions)) = nodes.nodes.pop().unwrap() {
            Ok(instructions)
        } else {
            Err(format!("last node is not an instructions:\n{nodes:#?}"))
        }
    }
}
