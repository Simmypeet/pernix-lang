//! Contains the binding tree of the syntax tree.
//!
//! The binding process is the process of converting the syntax tree into a binding tree. The
//! binding tree is similar to the syntax tree, but it contains more information about the semantics
//! of the program such as the type of an expression, the type of a variable, etc.

use derive_more::From;
use enum_as_inner::EnumAsInner;

use crate::{
    control_flow_graph::BasicBlockID,
    symbol::{
        ty::{PrimitiveType, Type, TypeBinding},
        EnumID, FieldID, FunctionID, StructID, VariableID,
    },
    SourceSpan,
};

/// Contains the type of the expression and its category.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueType {
    /// The type of the expression.
    pub ty: Type,

    /// The value category of the expression.
    pub category: ValueCategory,
}

/// Is a struct containing the information of an l-value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LValue {
    /// The address of the lvalue.
    pub address: Address,

    /// The type of the lvalue.
    pub is_mutable: bool,
}

/// Represents how the expression's value is used.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum ValueCategory {
    /// The expression's value does not have a location.
    RValue,

    /// The expression's value is stored in a location.
    LValue(LValue),
}

/// Is a trait that all expression bindings implement.
pub trait Binding {
    /// Returns the [`SourceSpan`] of the binding.
    fn span(&self) -> &SourceSpan;

    /// Returns the [`ValueType`] of the binding.
    fn value_type(&self) -> ValueType;
}

/// Is an enumeration of all possible expression bindings.
#[derive(Debug, Clone, PartialEq, From)]

pub enum Expression {
    NumericLiteral(NumericLiteral),
    BooleanLiteral(BooleanLiteral),
    Binary(Binary),
    Prefix(Prefix),
    EnumLiteral(EnumLiteral),
    StructLiteral(StructLiteral),
    FunctionCall(FunctionCall),
    MemberAccess(MemberAccess),
    TemporaryLoad(TemporaryLoad),
    PhiNodeShortCircuit(PhiNodeShortCircuit),
    NamedLoad(NamedLoad),
    Cast(Cast),
}

impl Binding for Expression {
    fn span(&self) -> &SourceSpan {
        match self {
            Self::NumericLiteral(binding) => binding.span(),
            Self::BooleanLiteral(binding) => binding.span(),
            Self::Binary(binding) => binding.span(),
            Self::Prefix(binding) => binding.span(),
            Self::EnumLiteral(binding) => binding.span(),
            Self::StructLiteral(binding) => binding.span(),
            Self::FunctionCall(binding) => binding.span(),
            Self::MemberAccess(binding) => binding.span(),
            Self::TemporaryLoad(binding) => binding.span(),
            Self::NamedLoad(binding) => binding.span(),
            Self::Cast(binding) => binding.span(),
            Self::PhiNodeShortCircuit(binding) => binding.span(),
        }
    }

    fn value_type(&self) -> ValueType {
        match self {
            Self::NumericLiteral(binding) => binding.value_type(),
            Self::BooleanLiteral(binding) => binding.value_type(),
            Self::Binary(binding) => binding.value_type(),
            Self::Prefix(binding) => binding.value_type(),
            Self::EnumLiteral(binding) => binding.value_type(),
            Self::StructLiteral(binding) => binding.value_type(),
            Self::FunctionCall(binding) => binding.value_type(),
            Self::MemberAccess(binding) => binding.value_type(),
            Self::TemporaryLoad(binding) => binding.value_type(),
            Self::Cast(binding) => binding.value_type(),
            Self::PhiNodeShortCircuit(binding) => binding.value_type(),
            Self::NamedLoad(binding) => binding.value_type(),
        }
    }
}

/// Represents an address to a local variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalVariableAddress {
    /// The index of the local variable in the symbol table.
    pub local_variable_symbol_index: usize,
}

/// Represents an address to a local argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalArgumentAddress {
    /// The index of the local argument in the symbol table.
    pub variable_id: VariableID,
}

/// Represents an address to a struct field.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructFieldAddress {
    /// The address of the struct.
    pub struct_address: Box<Address>,

    /// The id of the field in the struct.
    pub field_id: FieldID,
}

/// Represents an address of a particular l-value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From)]

pub enum Address {
    LocalVariableAddress(LocalVariableAddress),
    LocalArgumentAddress(LocalArgumentAddress),
    StructFieldAddress(StructFieldAddress),
}

/// Represents a numeric literal binding.
#[derive(Debug, Clone, PartialEq)]
pub struct NumericLiteral {
    /// The value of the numeric literal.
    pub value: NumericLiteralValue,

    /// The [`SourceSpan`] of the binding.
    pub span: SourceSpan,
}

/// Represents a load to the local variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalVariableLoad {
    /// The index of the local variable in the control flow graph binding.
    pub local_variable_index: usize,

    /// The type binding of the local variable.
    pub type_binding: TypeBinding,
}

/// Represents a load to the local argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalArgumentLoad {
    /// The id of the local argument.
    pub variable_id: VariableID,

    /// The type binding of the local argument.
    pub ty: Type,
}

/// Is an enumeration of how a named load can be performed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]

pub enum NamedLoadKind {
    LocalVariableLoad(LocalVariableLoad),
    LocalArgumentLoad(LocalArgumentLoad),
}

/// Represents a load to a named value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamedLoad {
    /// The name of the value to load.
    pub kind: NamedLoadKind,

    /// The [`SourceSpan`] of the binding.
    pub span: SourceSpan,
}

impl Binding for NamedLoad {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType {
        match &self.kind {
            NamedLoadKind::LocalVariableLoad(load) => ValueType {
                ty: load.type_binding.ty,
                category: LValue {
                    address: LocalVariableAddress {
                        local_variable_symbol_index: load.local_variable_index,
                    }
                    .into(),
                    is_mutable: load.type_binding.is_mutable,
                }
                .into(),
            },
            NamedLoadKind::LocalArgumentLoad(load) => ValueType {
                ty: load.ty,
                category: LValue {
                    address: LocalArgumentAddress {
                        variable_id: load.variable_id,
                    }
                    .into(),
                    is_mutable: false,
                }
                .into(),
            },
        }
    }
}

impl Binding for NumericLiteral {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType {
        ValueType {
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
            category: ValueCategory::RValue,
        }
    }
}

/// Represents a boolean literal binding.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanLiteral {
    /// The value of the boolean literal.
    pub value: bool,

    /// The [`SourceSpan`] of the binding.
    pub span: SourceSpan,
}

impl Binding for BooleanLiteral {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType {
        ValueType {
            ty: Type::Primitive(PrimitiveType::Bool),
            category: ValueCategory::RValue,
        }
    }
}

/// Is an enumeration of all binary operators.
///
/// `LogicalAnd` and `LogicalOr` are not included because they are short-circuiting and are handled
/// separately. See [`PhiNodeShortCircuit`]
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
}

impl BinaryOperator {
    /// Converts a [`BinaryOperator`] from a
    /// [`pernixc_syntax::syntax_tree::expression::BinaryOperator`].
    #[must_use]
    pub fn from_syntax_tree(
        binary_operator: &pernixc_syntax::syntax_tree::expression::BinaryOperator,
    ) -> Option<Self> {
        Some(match binary_operator {
            pernixc_syntax::syntax_tree::expression::BinaryOperator::Add(..) => Self::Add,
            pernixc_syntax::syntax_tree::expression::BinaryOperator::Subtract(..) => Self::Subtract,
            pernixc_syntax::syntax_tree::expression::BinaryOperator::Multiply(..) => Self::Multiply,
            pernixc_syntax::syntax_tree::expression::BinaryOperator::Divide(..) => Self::Divide,
            pernixc_syntax::syntax_tree::expression::BinaryOperator::Modulo(..) => Self::Modulo,
            pernixc_syntax::syntax_tree::expression::BinaryOperator::Assign(..) => Self::Assign,
            pernixc_syntax::syntax_tree::expression::BinaryOperator::CompoundAdd(..) => {
                Self::CompoundAdd
            }
            pernixc_syntax::syntax_tree::expression::BinaryOperator::CompoundSubtract(..) => {
                Self::CompoundSubtract
            }
            pernixc_syntax::syntax_tree::expression::BinaryOperator::CompoundMultiply(..) => {
                Self::CompoundMultiply
            }
            pernixc_syntax::syntax_tree::expression::BinaryOperator::CompoundDivide(..) => {
                Self::CompoundDivide
            }
            pernixc_syntax::syntax_tree::expression::BinaryOperator::CompoundModulo(..) => {
                Self::CompoundModulo
            }
            pernixc_syntax::syntax_tree::expression::BinaryOperator::Equal(..) => Self::Equal,
            pernixc_syntax::syntax_tree::expression::BinaryOperator::NotEqual(..) => Self::NotEqual,
            pernixc_syntax::syntax_tree::expression::BinaryOperator::LessThan(..) => Self::LessThan,
            pernixc_syntax::syntax_tree::expression::BinaryOperator::LessThanOrEqual(..) => {
                Self::LessThanOrEqual
            }
            pernixc_syntax::syntax_tree::expression::BinaryOperator::GreaterThan(..) => {
                Self::GreaterThan
            }
            pernixc_syntax::syntax_tree::expression::BinaryOperator::GreaterThanOrEqual(..) => {
                Self::GreaterThanOrEqual
            }
            _ => return None,
        })
    }
}

/// Represents a binary expression binding.
#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    /// The left-hand side expression of the binary expression.
    pub left: Box<Expression>,

    /// The right-hand side expression of the binary expression.
    pub right: Box<Expression>,

    /// The operator of the binary expression.
    pub operator: BinaryOperator,

    /// The value type that the binary expression evaluates to.
    pub value_type: ValueType,

    /// The [`SourceSpan`] of the binding.
    pub span: SourceSpan,
}

impl Binding for Binary {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType { self.value_type.clone() }
}

/// Is an enumeration of prefix operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]

pub enum PrefixOperator {
    LogicalNot,
    Negate,
}

/// Represents a prefix expression binding.
#[derive(Debug, Clone, PartialEq)]
pub struct Prefix {
    /// The operand of the prefix expression.
    pub operand: Box<Expression>,

    /// The operator of the prefix expression.
    pub operator: PrefixOperator,

    /// The [`SourceSpan`] of the binding.
    pub span: SourceSpan,

    /// The value type that the prefix expression evaluates to.
    pub value_type: ValueType,
}

impl Binding for Prefix {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType { self.value_type.clone() }
}

/// Represents an enumeration literal binding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumLiteral {
    /// The id of the enum symbol in the [`crate::symbol::Table`].
    pub enum_symbol_id: EnumID,

    /// The index of the variant symbol in the [`crate::symbol::Table`].
    pub variant_symbol_index: usize,

    /// The [`SourceSpan`] of the binding.
    pub span: SourceSpan,
}

impl Binding for EnumLiteral {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType {
        ValueType {
            ty: Type::TypedID(self.enum_symbol_id.into()),
            category: ValueCategory::RValue,
        }
    }
}

/// Represents a block expression binding.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    /// The index of the function symbol in the [`crate::symbol::Table`].
    pub function_id: FunctionID,

    /// The arguments of the function call.
    pub arguments: Vec<Expression>,

    /// The [`SourceSpan`] of the binding.
    pub span: SourceSpan,

    /// The value type that the function call evaluates to.
    pub value_type: ValueType,
}

impl Binding for FunctionCall {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType { self.value_type.clone() }
}

/// Represents a struct literal binding.
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    /// The id of the struct symbol in the [`crate::symbol::Table`].
    pub struct_id: StructID,

    /// The initializers of the struct literal.
    ///
    /// The first element of the tuple is the index of the field in the struct symbol table.
    /// The second element is the expression that initializes the field.
    pub field_index_expression_initializer_pairs: Vec<(usize, Expression)>,

    /// The [`SourceSpan`] of the binding.
    pub span: SourceSpan,
}

impl Binding for StructLiteral {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType {
        ValueType {
            ty: Type::TypedID(self.struct_id.into()),
            category: ValueCategory::RValue,
        }
    }
}

/// Represents a member access binding.
#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccess {
    /// Is the field id of the struct.
    pub field_id: FieldID,

    /// The id of the struct symbol in the [`crate::symbol::Table`].
    pub struct_id: StructID,

    /// The expression that evaluates to the struct.
    pub struct_expression: Box<Expression>,

    /// The [`SourceSpan`] of the binding.
    pub span: SourceSpan,

    /// The type of the field.
    pub field_type: Type,
}

impl Binding for MemberAccess {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType {
        let struct_expression_value_type = self.struct_expression.value_type();
        match struct_expression_value_type.category {
            ValueCategory::RValue => ValueType {
                ty: self.field_type,
                category: ValueCategory::RValue,
            },
            ValueCategory::LValue(lvalue) => ValueType {
                ty: self.field_type,
                category: LValue {
                    address: StructFieldAddress {
                        struct_address: Box::new(lvalue.address),
                        field_id: self.field_id,
                    }
                    .into(),
                    is_mutable: lvalue.is_mutable,
                }
                .into(),
            },
        }
    }
}

/// Represents a cast binding.
#[derive(Debug, Clone, PartialEq)]
pub struct Cast {
    /// The expression that is casted.
    pub expression: Box<Expression>,

    /// The type that the expression is casted to.
    pub target_type: Type,

    /// The [`SourceSpan`] of the binding.
    pub span: SourceSpan,
}

impl Binding for Cast {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType {
        ValueType {
            ty: self.target_type,
            category: ValueCategory::RValue,
        }
    }
}

/// Represents a temporary load binding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemporaryLoad {
    /// The index of the temporary variable in the
    /// [`crate::control_flow_graph::ControlFlowGraph::temporary_variables()``]
    pub temporary_variable_symbol_index: usize,

    /// The [`Type`] of the temporary variable.
    pub ty: Type,

    /// The [`SourceSpan`] of the binding (the span of the imperative expression).
    pub span: SourceSpan,
}

impl Binding for TemporaryLoad {
    fn span(&self) -> &SourceSpan { &self.span }

    fn value_type(&self) -> ValueType {
        ValueType {
            ty: self.ty,
            category: ValueCategory::RValue,
        }
    }
}

/// Represents the value of a numeric literal.
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

/// Is an enumeration of either an `And` short circuit or an `Or` short circuit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ShortCircuit {
    /// `and` short circuit
    ///
    /// # Control Flow Graph
    /// The control flow graph of the `and` short circuit is as follows:
    ///
    /// ``` txt
    /// preBlock:
    ///     %lhsValue = evaluate `lhs`
    ///     conditionalJump %lhsValue rhsBlock endBlock
    ///
    /// rhsBlock:
    ///     %rhsValue = evaluate `rhs`
    ///  
    /// endBlock:
    ///     %phiValue = phi [ false, preBlock ], [ %rhsValue, rhsBlock ]
    /// ```
    And,

    /// `or` short circuit
    ///
    /// # Control Flow Graph
    /// The control flow graph of the `or` short circuit is as follows:
    ///
    /// ``` txt
    /// preBlock:
    ///     %lhsValue = evaluate `lhs`
    ///     conditionalJump %lhsValue endBlock rhsBlock
    ///
    /// rhsBlock:
    ///     %rhsValue = evaluate `rhs`
    ///
    /// endBlock:
    ///     %phiValue = phi [ true, preBlock ], [ %rhsValue, rhsBlock ]
    /// ```
    Or,
}

/// Is an value that will be yielded by a [`PhiNodeShortCircuit`].
#[derive(Debug, Clone, PartialEq, EnumAsInner, From)]
pub enum ShortCircuitValue {
    /// Constant `true` value.
    True,

    /// Constant `false` value.
    False,

    /// A value that is yielded by a [`PhiNodeShortCircuit`].
    Expression(Box<Expression>),
}

/// Is an edge of a [`PhiNodeShortCircuit`].
#[derive(Debug, Clone, PartialEq)]
pub struct PhiEdge {
    /// The predecessor block of the edge.
    pub predecessor: BasicBlockID,

    /// The value of the edge.
    pub value: ShortCircuitValue,
}

/// Is a PHI instruction that is used to implement short circuiting.
#[derive(Debug, Clone, PartialEq)]
pub struct PhiNodeShortCircuit {
    /// The first edge of the PHI node.
    pub first_edge: PhiEdge,

    /// The second edge of the PHI node.
    pub second_edge: PhiEdge,

    /// The type of the short circuit.
    pub short_circuit: ShortCircuit,

    /// The source span of the binary operator that is short circuited.
    pub binary_short_circuit_expression_span: SourceSpan,
}

impl Binding for PhiNodeShortCircuit {
    fn span(&self) -> &SourceSpan { &self.binary_short_circuit_expression_span }

    fn value_type(&self) -> ValueType {
        ValueType {
            ty: PrimitiveType::Bool.into(),
            category: ValueCategory::RValue,
        }
    }
}
