#[derive(Parser)]
#[grammar = "grammar.pest"]
struct StreamCompilerParser;

use pest::{iterators::{Pair, Pairs}, pratt_parser::{Assoc, Op, PrattParser}, Parser};

#[derive(Debug)]
pub enum ClauseType {
    Filter,
    Map,
}

#[derive(Debug)]
pub struct Clause<'a> {
    pub clause_type: ClauseType,
    pub expression: Expr<'a>,
}

pub type Value = f64; // This should maybe be an enum with an i64 variant

#[derive(Debug)]
pub enum Expr<'a> {
    Value(Value),
    Identifier(&'a str),
    BinaryOperation(BinaryOperation<'a>),
}

#[derive(Debug)]
pub struct BinaryOperation<'a> {
    pub left: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,
    pub operator: BinaryOperator
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
    // Precedence is *lowest to highest*
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::DOUBLE_PIPE, Assoc::Left))
        .op(Op::infix(Rule::DOUBLE_AMP, Assoc::Left))
        .op(Op::infix(Rule::EQUALS_OPERATOR, Assoc::Left) | Op::infix(Rule::NOT_EQUALS_OPERATOR, Assoc::Left))
        .op(Op::infix(Rule::LESS_THAN, Assoc::Left) | Op::infix(Rule::LESS_EQUAL, Assoc::Left) | Op::infix(Rule::GREATER_THAN, Assoc::Left) | Op::infix(Rule::GREATER_EQUAL, Assoc::Left))
        .op(Op::infix(Rule::PLUS_SIGN, Assoc::Left) | Op::infix(Rule::MINUS_SIGN, Assoc::Left))
        .op(Op::infix(Rule::STAR_SIGN, Assoc::Left) | Op::infix(Rule::FORWARD_SLASH, Assoc::Left) | Op::infix(Rule::MODULO_OPERATOR, Assoc::Left));

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

pub fn lex_and_parse(program_text: &str) -> Result<Vec<Clause>, &'static str> {
    if let Ok(mut pairs) = StreamCompilerParser::parse(Rule::program, program_text) {
        if pairs.len() != 1 {
            return Err("Expected exactly one program, got {}");
        }
        let inner = pairs.next().expect("Expected one program pair");

        match inner.as_rule() {
            Rule::program => parse_program(inner),
            _ => Err("Unexpected rule"),
        }

    } else {
        Err("Failed to parse the program text")
    }
}
