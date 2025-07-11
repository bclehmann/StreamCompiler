use inkwell::values::{VectorValue, FloatValue, IntValue};
use inkwell::types::{BasicTypeEnum};

use crate::{compiler::CodeGen, parser::{Expr}};

#[derive(Debug)]
pub enum ExprResult<'cg> {
    Double(FloatValue<'cg>),
    DoubleVec(VectorValue<'cg>),
    Boolean(IntValue<'cg>),
    BooleanVec(VectorValue<'cg>),
}

impl <'cg> From<FloatValue<'cg>> for ExprResult<'cg> {
    fn from(value: FloatValue<'cg>) -> Self {
        ExprResult::Double(value)
    }
}

impl <'cg> From<VectorValue<'cg>> for ExprResult<'cg> {
    fn from(value: VectorValue<'cg>) -> Self {
        let element_type: BasicTypeEnum = value.get_type().get_element_type();
        match element_type {
            BasicTypeEnum::FloatType(_) => ExprResult::DoubleVec(value),
            BasicTypeEnum::IntType(_) => ExprResult::BooleanVec(value),
            _ => panic!("Unsupported vector type: {:?}", value.get_type()),
        }
    }
}

impl <'cg> From<IntValue<'cg>> for ExprResult<'cg> {
    fn from(value: IntValue<'cg>) -> Self {
        ExprResult::Boolean(value)
    }
}

impl <'cg> From<ExprResult<'cg>> for FloatValue<'cg> {
    fn from(value: ExprResult<'cg>) -> Self {
        match value {
            ExprResult::Double(v) => v,
            _ => panic!("Expected a FloatValue, got {:?}", value),
        }
    }
}

impl <'cg> From<ExprResult<'cg>> for VectorValue<'cg> {
    fn from(value: ExprResult<'cg>) -> Self {
        match value {
            ExprResult::DoubleVec(v) => v,
            ExprResult::BooleanVec(v) => v,
            _ => panic!("Expected a VectorValue, got {:?}", value),
        }
    }
}

impl <'cg> From<ExprResult<'cg>> for IntValue<'cg> {
    fn from(value: ExprResult<'cg>) -> Self {
        match value {
            ExprResult::Boolean(v) => v,
            _ => panic!("Expected a IntValue, got {:?}", value),
        }
    }
}

pub trait ExprCompiler {
    fn compile_expression<'cg>(self: &'cg Self, codegen: &'cg CodeGen, expr: &Expr, input: ExprResult<'cg>) -> Option<ExprResult<'cg>>;
    fn float_as_bool<'cg>(self: &'cg Self, codegen: &'cg CodeGen, value: ExprResult<'cg>) -> ExprResult<'cg>;
}
