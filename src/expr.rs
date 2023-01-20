use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;
use std::fmt;

#[derive(pest_derive::Parser)]
#[grammar = "calculator.pest"]
pub struct CalculatorParser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            // Addition and subtract have equal precedence
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left) | Op::infix(modulo, Left))
            .op(Op::infix(power, Left))
            .op(Op::prefix(unary_minus))
    };
}

#[derive(Debug)]
pub enum Expr {
    Number(f64),
    Variable(String),
    UnaryMinus(Box<Expr>),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => Expr::Number(primary.as_str().parse::<f64>().unwrap()),
            Rule::variable => Expr::Variable(primary.as_str().to_string()),
            Rule::expr => parse_expr(primary.into_inner()),
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::add => Op::Add,
                Rule::subtract => Op::Subtract,
                Rule::multiply => Op::Multiply,
                Rule::divide => Op::Divide,
                Rule::modulo => Op::Modulo,
                Rule::power => Op::Power,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };
            Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::unary_minus => Expr::UnaryMinus(Box::new(rhs)),
            _ => unreachable!(),
        })
        .parse(pairs)
}

#[derive(Debug)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Subtract => write!(f, "-"),
            Op::Multiply => write!(f, "*"),
            Op::Divide => write!(f, "/"),
            Op::Modulo => write!(f, "%"),
            Op::Power => write!(f, "^")
        }
    }
}

pub fn serialize_expression(expr: &Expr) -> String {
    match expr {
        Expr::Number(n) => n.to_string(),
        Expr::Variable(x) => x.to_string(),
        Expr::UnaryMinus(sub_expr) => "-".to_owned() + &serialize_expression(&*sub_expr),
        Expr::BinOp { lhs, op, rhs } => {
            format!(
                "({} {} {})",
                serialize_expression(&*lhs),
                &format!("{}", op),
                &serialize_expression(&*rhs)
            )
        }
    }
}