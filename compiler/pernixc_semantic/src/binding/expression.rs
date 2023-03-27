use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};

use super::statement::StatementBinding;
use crate::{
    symbol::ty::{PrimitiveType, Type},
    SourceSpan,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpressionType {
    pub ty: Type,
    pub category: ExpressionCategory,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ExpressionCategory {
    RValue,
    LValue { is_mutable: bool },
}

pub trait ExpressionBindingTrait {
    fn span(&self) -> &SourceSpan;
    fn expression_type(&self) -> ExpressionType;
}

#[derive(Debug, Clone, PartialEq, From)]
pub enum ExpressionBinding {
    NumericLiteralBinding(NumericLiteralBinding),
    BooleanLiteralBinding(BooleanLiteralBinding),
    BinaryExpressionBinding(BinaryExpressionBinding),
    PrefixExpressionBinding(PrefixExpressionBinding),
    IdentifierExpressionBinding(IdentifierExpressionBinding),
    StructLiteralBinding(StructLiteralBinding),
    FunctionCallBinding(FunctionCallBinding),
    MemberAccessBinding(MemberAccessBinding),
    ExpressBinding(ExpressBinding),
    CastBinding(CastBinding),
    BlockBinding(BlockBinding),
    IfElseBinding(IfElseBinding),
    LoopBinding(LoopBinding),
}

impl ExpressionBindingTrait for ExpressionBinding {
    fn span(&self) -> &SourceSpan {
        match self {
            ExpressionBinding::NumericLiteralBinding(binding) => binding.span(),
            ExpressionBinding::BooleanLiteralBinding(binding) => binding.span(),
            ExpressionBinding::BinaryExpressionBinding(binding) => binding.span(),
            ExpressionBinding::PrefixExpressionBinding(binding) => binding.span(),
            ExpressionBinding::IdentifierExpressionBinding(binding) => binding.span(),
            ExpressionBinding::StructLiteralBinding(binding) => binding.span(),
            ExpressionBinding::FunctionCallBinding(binding) => binding.span(),
            ExpressionBinding::MemberAccessBinding(binding) => binding.span(),
            ExpressionBinding::ExpressBinding(binding) => binding.span(),
            ExpressionBinding::CastBinding(binding) => binding.span(),
            ExpressionBinding::BlockBinding(binding) => binding.span(),
            ExpressionBinding::IfElseBinding(binding) => binding.span(),
            ExpressionBinding::LoopBinding(binding) => binding.span(),
        }
    }

    fn expression_type(&self) -> ExpressionType {
        match self {
            ExpressionBinding::NumericLiteralBinding(binding) => binding.expression_type(),
            ExpressionBinding::BooleanLiteralBinding(binding) => binding.expression_type(),
            ExpressionBinding::BinaryExpressionBinding(binding) => binding.expression_type(),
            ExpressionBinding::PrefixExpressionBinding(binding) => binding.expression_type(),
            ExpressionBinding::IdentifierExpressionBinding(binding) => binding.expression_type(),
            ExpressionBinding::StructLiteralBinding(binding) => binding.expression_type(),
            ExpressionBinding::FunctionCallBinding(binding) => binding.expression_type(),
            ExpressionBinding::MemberAccessBinding(binding) => binding.expression_type(),
            ExpressionBinding::ExpressBinding(binding) => binding.expression_type(),
            ExpressionBinding::CastBinding(binding) => binding.expression_type(),
            ExpressionBinding::BlockBinding(binding) => binding.expression_type(),
            ExpressionBinding::IfElseBinding(binding) => binding.expression_type(),
            ExpressionBinding::LoopBinding(binding) => binding.expression_type(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumericLiteralBinding {
    pub value: NumericLiteralValue,
    pub span: SourceSpan,
}

impl ExpressionBindingTrait for NumericLiteralBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType {
        ExpressionType {
            ty: match &self.value {
                NumericLiteralValue::Float64(..) => Type::Primitive(PrimitiveType::Float64),
                NumericLiteralValue::Float32(..) => Type::Primitive(PrimitiveType::Float32),
                NumericLiteralValue::Int8(..) => Type::Primitive(PrimitiveType::Int8),
                NumericLiteralValue::Int16(..) => Type::Primitive(PrimitiveType::Int16),
                NumericLiteralValue::Int32(..) => Type::Primitive(PrimitiveType::Int32),
                NumericLiteralValue::Int64(..) => Type::Primitive(PrimitiveType::Int64),
                NumericLiteralValue::Uint8(..) => Type::Primitive(PrimitiveType::Uint8),
                NumericLiteralValue::Uint16(..) => Type::Primitive(PrimitiveType::Uint16),
                NumericLiteralValue::Uint32(..) => Type::Primitive(PrimitiveType::Uint32),
                NumericLiteralValue::Uint64(..) => Type::Primitive(PrimitiveType::Uint64),
            },
            category: ExpressionCategory::RValue,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanLiteralBinding {
    pub value: bool,
    pub span: SourceSpan,
}

impl ExpressionBindingTrait for BooleanLiteralBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType {
        ExpressionType {
            ty: Type::Primitive(PrimitiveType::Bool),
            category: ExpressionCategory::RValue,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Assign,
    CompoundAdd,
    CompoundSubtract,
    CompoundMultiply,
    CompoundDivide,
    CompoundModulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpressionBinding {
    pub left: Box<ExpressionBinding>,
    pub right: Box<ExpressionBinding>,
    pub operator: BinaryOperator,
    pub expression_type: ExpressionType,
    pub span: SourceSpan,
}

impl ExpressionBindingTrait for BinaryExpressionBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType { self.expression_type }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOperator {
    LogicalNot,
    Negate,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpressionBinding {
    pub operand: Box<ExpressionBinding>,
    pub operator: PrefixOperator,
    pub span: SourceSpan,
    pub expression_type: ExpressionType,
}

impl ExpressionBindingTrait for PrefixExpressionBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType { self.expression_type }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IdentifierExpressionBindingKind {
    LocalVariableIndex {
        local_variable_index: usize,
    },
    LocalArgumentIndex {
        local_argument_index: usize,
    },
    EnumVariantIndex {
        enum_symbol_index: usize,
        variant_number: usize,
    },
}

#[derive(Debug, Clone, PartialEq, CopyGetters)]
pub struct IdentifierExpressionBinding {
    pub identifier_expression_binding_kind: IdentifierExpressionBindingKind,
    pub span: SourceSpan,
    pub expression_type: ExpressionType,
}

impl ExpressionBindingTrait for IdentifierExpressionBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType { self.expression_type }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCallBinding {
    pub function_symbol_index: usize,
    pub arguments: Vec<ExpressionBinding>,
    pub span: SourceSpan,
    pub expression_type: ExpressionType,
}

impl ExpressionBindingTrait for FunctionCallBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType { self.expression_type }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteralBinding {
    pub struct_symbol_index: usize,
    pub field_index_expression_initializer_pairs: Vec<(usize, ExpressionBinding)>,
    pub span: SourceSpan,
    pub expression_type: ExpressionType,
}

impl ExpressionBindingTrait for StructLiteralBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType { self.expression_type }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccessBinding {
    pub field_index: usize,
    pub struct_expression: Box<ExpressionBinding>,
    pub expression_type: ExpressionType,
}

impl ExpressionBindingTrait for MemberAccessBinding {
    fn span(&self) -> &SourceSpan { self.struct_expression.span() }

    fn expression_type(&self) -> ExpressionType { self.expression_type }
}

#[derive(Debug, Clone, PartialEq, CopyGetters)]
pub struct ContinueBinding {
    pub target_loop_control_id: usize,
    pub span: SourceSpan,
}

impl ExpressionBindingTrait for ContinueBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType {
        ExpressionType {
            ty: Type::Primitive(PrimitiveType::Void),
            category: ExpressionCategory::RValue,
        }
    }
}

#[derive(Debug, Clone, PartialEq, CopyGetters, Getters)]
pub struct BreakBinding {
    pub target_loop_control_id: usize,
    pub break_expression: Option<Box<ExpressionBinding>>,
    pub span: SourceSpan,
}

impl ExpressionBindingTrait for BreakBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType {
        ExpressionType {
            ty: Type::Primitive(PrimitiveType::Void),
            category: ExpressionCategory::RValue,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressBinding {
    pub expression: Option<Box<ExpressionBinding>>,
    pub target_block_control_id: usize,
    pub span: SourceSpan,
}

impl ExpressionBindingTrait for ExpressBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType {
        ExpressionType {
            ty: Type::Primitive(PrimitiveType::Void),
            category: ExpressionCategory::RValue,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastBinding {
    pub expression: Box<ExpressionBinding>,
    pub target_type: Type,
    pub span: SourceSpan,
}

impl ExpressionBindingTrait for CastBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType {
        ExpressionType {
            ty: self.target_type,
            category: ExpressionCategory::RValue,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockBinding {
    pub statements: Vec<StatementBinding>,
    pub block_control_id: Option<usize>,
    pub span: SourceSpan,
    pub expression_type: ExpressionType,
}

impl ExpressionBindingTrait for BlockBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType { self.expression_type }
}

#[derive(Debug, Clone, PartialEq, Getters)]
pub struct IfElseBinding {
    pub condition: Box<ExpressionBinding>,
    pub then_expression: Box<ExpressionBinding>,
    pub else_expression: Option<Box<ExpressionBinding>>,
    pub expression_type: ExpressionType,
    pub span: SourceSpan,
}

impl ExpressionBindingTrait for IfElseBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType { self.expression_type }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopBinding {
    pub expression: Box<ExpressionBinding>,
    pub loop_control_id: Option<usize>,
    pub expression_type: ExpressionType,
    pub span: SourceSpan,
}

impl ExpressionBindingTrait for LoopBinding {
    fn span(&self) -> &SourceSpan { &self.span }

    fn expression_type(&self) -> ExpressionType { self.expression_type }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum NumericLiteralValue {
    Float64(f64),
    Float32(f32),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
}
