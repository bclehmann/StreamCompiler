extern crate pest;

#[macro_use]
extern crate pest_derive;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{PrattParser, Op, Assoc};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct StreamCompilerParser;

#[derive(Debug)]
enum ClauseType {
    Filter,
    Map,
}

#[derive(Debug)]
struct Clause<'a> {
    clause_type: ClauseType,
    expression: Expr<'a>,
}

type Value = f64; // This should maybe be an enum with an i64 variant

#[derive(Debug)]
pub enum Expr<'a> {
    Value(Value),
    Identifier(&'a str),
    BinaryOperation(BinaryOperation<'a>),
}

#[derive(Debug)]
pub struct BinaryOperation<'a> {
    left: Box<Expr<'a>>,
    right: Box<Expr<'a>>,
    operator: BinaryOperator
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LogicalAnd,
    LogicalOr,
    Equals,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Modulo,
}

fn parse_expression(pairs: Pairs<Rule>) -> Result<Expr, &'static str> {
    // FWIW, I think prat means something rude in British English
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::PLUS_SIGN, Assoc::Left) | Op::infix(Rule::MINUS_SIGN, Assoc::Left))
        .op(Op::infix(Rule::STAR_SIGN, Assoc::Left) | Op::infix(Rule::FORWARD_SLASH, Assoc::Left) | Op::infix(Rule::MODULO_OPERATOR, Assoc::Left))
        .op(Op::infix(Rule::LESS_THAN, Assoc::Left) | Op::infix(Rule::LESS_EQUAL, Assoc::Left) | Op::infix(Rule::GREATER_THAN, Assoc::Left) | Op::infix(Rule::GREATER_EQUAL, Assoc::Left))
        .op(Op::infix(Rule::EQUALS_OPERATOR, Assoc::Left) | Op::infix(Rule::NOT_EQUALS_OPERATOR, Assoc::Left))
        .op(Op::infix(Rule::DOUBLE_AMP, Assoc::Left))
        .op(Op::infix(Rule::DOUBLE_PIPE, Assoc::Left));

    let expr = pratt
        .map_primary(|p| match p.as_rule() {
            Rule::number => {
                let s = p.as_str();
                return Expr::Value(s.parse::<f64>().unwrap_or_else(|e| panic!("Could not parse number: {:?}", e)));
            }
            Rule::identifier => Expr::Identifier(p.as_str()),
            Rule::expr => parse_expression(p.into_inner()).expect("Failed to parse inner expression"),
            rule => unreachable!("Invalid rule encountered: {:?}", rule)
        })
        .map_infix(|left, op, right| {
            let operator = match op.as_rule() {
                Rule::PLUS_SIGN => BinaryOperator::Add,
                Rule::MINUS_SIGN => BinaryOperator::Subtract,
                Rule::STAR_SIGN => BinaryOperator::Multiply,
                Rule::FORWARD_SLASH => BinaryOperator::Divide,
                Rule::MODULO_OPERATOR => BinaryOperator::Modulo,
                Rule::DOUBLE_AMP => BinaryOperator::LogicalAnd,
                Rule::DOUBLE_PIPE => BinaryOperator::LogicalOr,
                Rule::EQUALS_OPERATOR => BinaryOperator::Equals,
                Rule::NOT_EQUALS_OPERATOR => BinaryOperator::NotEqual,
                Rule::LESS_THAN => BinaryOperator::LessThan,
                Rule::LESS_EQUAL => BinaryOperator::LessEqual,
                Rule::GREATER_THAN => BinaryOperator::GreaterThan,
                Rule::GREATER_EQUAL => BinaryOperator::GreaterEqual,
                rule => unreachable!("Invalid rule encountered: {:?}", rule)
            };

            Expr::BinaryOperation(BinaryOperation {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator
            })
        })
        .parse(pairs);

    Ok(expr)
}

fn parse_clause(pair: Pair<Rule>) -> Result<Clause, &'static str> {
    let inner_pairs = pair.into_inner();
    if inner_pairs.len() != 1 {
        return Err("Expected exactly one inner pair for clause");
    }

    let inner = inner_pairs.into_iter().next().expect("Expected one inner pair");

    let clause_type = match inner.as_rule() {
        Rule::filter_clause => ClauseType::Filter,
        Rule::map_clause => ClauseType::Map,
        _ => return Err("Unexpected rule in clause"),
    };

    return Ok(Clause {
        clause_type,
        expression: parse_expression(inner.into_inner()).expect("Could not parse clause expression")
    });
}

fn parse_program(program: Pair<Rule>) -> Result<Vec<Clause>, &'static str> {
    let mut clauses = vec![];

    for pair in program.into_inner() {
        match pair.as_rule() {
            Rule::clause => {
                let clause = parse_clause(pair);
                match clause {
                    Ok(c) => clauses.push(c),
                    Err(e) => return Err(e),
                }
            }
            _ => {
                return Err("Unnexpected rule in program");
            }
        }
    }
    Ok(clauses)
}

fn interpret_expression(expr: &Expr, input: Value) -> Value {
    match expr {
        Expr::Value(v) => *v,
        Expr::Identifier("x") => input, // The only valid identifier, the input value
        Expr::Identifier(_) => panic!("Unknown identifier encountered"),
        Expr::BinaryOperation(op) => {
            let left = interpret_expression(&op.left, input);
            let right = interpret_expression(&op.right, input);
            match op.operator {
                BinaryOperator::Add => left + right,
                BinaryOperator::Subtract => left - right,
                BinaryOperator::Multiply => left * right,
                BinaryOperator::Divide => left / right,
                BinaryOperator::Modulo => left % right,
                BinaryOperator::LogicalAnd => if left != 0.0 && right != 0.0 { 1.0 } else { 0.0 },
                BinaryOperator::LogicalOr => if left != 0.0 || right != 0.0 { 1.0 } else { 0.0 },
                BinaryOperator::Equals => if left == right { 1.0 } else { 0.0 },
                BinaryOperator::NotEqual => if left != right { 1.0 } else { 0.0 },
                BinaryOperator::LessThan => if left < right { 1.0 } else { 0.0 },
                BinaryOperator::LessEqual => if left <= right { 1.0 } else { 0.0 },
                BinaryOperator::GreaterThan => if left > right { 1.0 } else { 0.0 },
                BinaryOperator::GreaterEqual => if left >= right { 1.0 } else { 0.0 },
            }
        },
    }
}

fn interpret_program_on_input(program: &[Clause], input: f64) -> Option<f64> {
    let mut result = input;

    for clause in program {
        match clause.clause_type {
            ClauseType::Filter => {
                if interpret_expression(&clause.expression, result) == 0.0 {
                    return None; // Filter condition not met
                }
            }
            ClauseType::Map => {
                result = interpret_expression(&clause.expression, result);
            }
        }
    }

    Some(result)
}

fn main() {
    let stdin = std::io::stdin();
    let input_str= std::io::read_to_string(stdin)
        .expect("Failed to read from stdin");
    let input_lines = input_str
        .lines();
    let input = input_lines
        .map(|line| line.parse::<f64>().expect("Could not parse input"));
    let program_text = std::env::args().nth(1).expect("No program text provided");

    println!("Input program text: {}", program_text);

    if let Ok(mut pairs) = StreamCompilerParser::parse(Rule::program, &program_text) {
        println!("{:?}", pairs.as_str());
        if pairs.len() != 1 {
            panic!("Expected exactly one program, got {}", pairs.len());
        }
        let inner = pairs.next().expect("Expected one program pair");

        let program = match inner.as_rule() {
            Rule::program => parse_program(inner),
            _ => panic!("Unexpected rule {}", inner.as_str()),
        };

        match program {
            Ok(program) => {
                for input in input {
                    match interpret_program_on_input(&program, input) {
                        Some(result) => println!("{}", result),
                        None => {}
                    }
                }
            }
            Err(e) => panic!("Failed to parse program: {}", e),
        }
    } else {
        panic!("Failed to parse the program text");
    }
}
