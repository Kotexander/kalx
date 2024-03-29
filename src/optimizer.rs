use std::{mem::swap, rc::Rc};

use crate::{
    parser::{Block, Expression, Instruction, Function},
    tokenizer::Operation,
};

fn try_combine(lhs: &Expression, op: &Operation, rhs: &Expression) -> Option<Expression> {
    if let (Expression::Number(lhs), Expression::Number(rhs)) = (lhs, rhs) {
        let new = match op {
            Operation::Bool(_bop) => todo!(),
            Operation::BND => Expression::Number(lhs & rhs),
            Operation::BOR => Expression::Number(lhs | rhs),
            Operation::Add => Expression::Number(lhs + rhs),
            Operation::Sub => Expression::Number(lhs - rhs),
            Operation::Mul => Expression::Number(lhs * rhs),
            Operation::Div => Expression::Number(lhs / rhs),
        };
        Some(new)
    } else {
        None
    }
}

fn optimize_expr_and_replace(expr: &mut Rc<Expression>) {
    if let Some(new_expr) = optimize_expr(Rc::get_mut(expr).unwrap()) {
        *expr = Rc::new(new_expr)
    }
}

pub fn optimize_expr(expr: &mut Expression) -> Option<Expression> {
    match expr {
        Expression::Number(_) => None,
        Expression::String(_) => None,
        Expression::Ident(_) => None,
        Expression::Operation { lhs, op, rhs } => {
            optimize_expr_and_replace(lhs);
            optimize_expr_and_replace(rhs);

            let comb = try_combine(lhs, op, rhs);
            if comb.is_some() {
                return comb;
            }

            if op.is_commutative() {
                if !lhs.is_recursive() && rhs.is_recursive() {
                    swap(lhs, rhs);
                }
                if lhs.is_const() {
                    swap(lhs, rhs);
                }
            }

            None
        }
        Expression::Index { base: expr, index, size:_ } => {
            optimize_expr_and_replace(expr);
            optimize_expr_and_replace(index);
            None
        }
        Expression::FunctionCall { name: _, args } => {
            for arg in args.iter_mut() {
                optimize_expr_and_replace(arg);
            }
            None
        }
        Expression::Deref(expr) => {
            optimize_expr_and_replace(expr);
            None
        },
        
    }
}

pub fn optimize_instruction(instruction: &mut Instruction) {
    match instruction {
        Instruction::Declare { id: _, value } | Instruction::Assign { id: _, value } => {
            optimize_expr_and_replace(value);
        }
        Instruction::Expr(expr) => {
            optimize_expr_and_replace(expr);
        }
        Instruction::Loop(block) => {
            optimize_block(Rc::get_mut(block).unwrap());
        }
        Instruction::While { expr, block } => {
            optimize_expr_and_replace(expr);
            optimize_block(Rc::get_mut(block).unwrap());
        }
        Instruction::Block(block) => optimize_block(Rc::get_mut(block).unwrap()),
        Instruction::If { expr, block } => {
            optimize_expr_and_replace(expr);
            optimize_block(Rc::get_mut(block).unwrap());
        },
        
    }
}

pub fn optimize_block(block: &mut Block) {
    for instruction in block.instructions.iter_mut() {
        optimize_instruction(Rc::get_mut(instruction).unwrap());
    
    }
}

pub fn optimize(functions: &mut [Function]) {
    for function in functions.iter_mut() {
        optimize_block(Rc::get_mut(&mut function.block).unwrap());
    }
}

