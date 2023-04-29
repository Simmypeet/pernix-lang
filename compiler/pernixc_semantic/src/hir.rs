//! Contains code related to the high-level intermediate representation of Pernix.

use std::{collections::HashMap, ops::Index, sync::Arc};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_lexical::token::{
    Identifier as IdentifierToken, NumericLiteral as NumericLiteralToken,
};
use pernixc_source::source_file::Span;

use self::{binding::Binding, instruction::Backend};
use crate::{
    cfg::ControlFlowGraph,
    infer::{InferableType, InferenceContext, InferenceID},
    symbol::{
        implements_data_with_id,
        table::Table,
        ty::{PrimitiveType, Type},
        EnumVariantID, FieldID, ModuleID, OverloadID, OverloadSetID,
    },
};

pub mod binding;
pub mod builder;
pub mod errors;
pub mod instruction;

/// The high-level intermediate representation of Pernix.
#[derive(Debug, Getters)]
pub struct Hir {
    #[get = "pub"]
    control_flow_graph: ControlFlowGraph<Backend>,
    variables_by_id: HashMap<SsaVariableID, SsaVariable>,
    allocas_by_id: HashMap<AllocaID, Alloca>,
    inference_context: InferenceContext,
    table: Arc<Table>,
    overload_id: OverloadID,
    parent_overload_set_id: OverloadSetID,
    parent_module_id: ModuleID,
}

impl Hir {
    /// Gets the [`InferableType`] of the given value.
    #[must_use]
    pub fn get_inferable_type(&self, id: ValueTypeID) -> InferableType {
        match id {
            ValueTypeID::Inferring(id) => self.inference_context.get_inferable_type(id),
            ValueTypeID::Type(ty) => InferableType::Type(ty),
        }
    }

    /// Gets the [`BoundType`] of the given value.
    #[must_use]
    pub fn get_bound_type<T: ?Sized>(&self, value: &T) -> BoundType
    where
        Self: Inspectable<T>,
    {
        let inferable_type = self.get_inferable_type(self.get_value_type_id(value));
        let reachability = self.get_reachability(value);

        BoundType {
            inferable_type,
            reachability,
        }
    }

    /// Creates a new empty [`Hir`].
    fn new(table: Arc<Table>, overload_id: OverloadID) -> Self {
        let parent_overload_set_id = table[overload_id].parent_overload_set_id();
        let parent_module_id = table[parent_overload_set_id].parent_module_id();

        Self {
            control_flow_graph: ControlFlowGraph::new(),
            variables_by_id: HashMap::new(),
            allocas_by_id: HashMap::new(),
            inference_context: InferenceContext::new(),
            table,
            overload_id,
            parent_module_id,
            parent_overload_set_id,
        }
    }
}

impl Index<SsaVariableID> for Hir {
    type Output = SsaVariable;

    fn index(&self, index: SsaVariableID) -> &Self::Output { &self.variables_by_id[&index] }
}

impl Index<AllocaID> for Hir {
    type Output = Alloca;

    fn index(&self, index: AllocaID) -> &Self::Output { &self.allocas_by_id[&index] }
}

/// Since some types are not known until the type being inferred, values can store the
/// [`ValueTypeID`], which can be used to get the inferred type later, instead of storing the
/// [`Type`] directly.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum ValueTypeID {
    /// The type of the value is being inferred.
    Inferring(InferenceID),

    /// The type of the value is always determined and known.
    Type(Type),
}

/// Represents an address offset that points to a particular field member of the struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldAddress {
    /// The address of the struct.
    struct_address: Box<Address>,

    /// The [`FieldID`] of the field member (can think of it as the offset).
    field_id: FieldID,

    /// Specifies the location in the source code where the field address is created.
    span: Span,
}

impl Inspectable<FieldAddress> for Hir {
    fn get_value_type_id(&self, value: &FieldAddress) -> ValueTypeID {
        self.table[value.field_id].ty().into()
    }

    fn get_reachability(&self, _: &FieldAddress) -> Reachability { Reachability::Reachable }

    fn get_span(&self, value: &FieldAddress) -> Span { value.span.clone() }
}

/// Represents a stack allocated memory that satisfies the need of user-defined variable declaration
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AllocaAddress {
    /// The address of the alloca.
    alloca_id: AllocaID,

    /// Specifies the location in the source code where the alloca address is created.
    span: Span,
}

impl Inspectable<AllocaAddress> for Hir {
    fn get_value_type_id(&self, value: &AllocaAddress) -> ValueTypeID {
        match &*self.allocas_by_id[&value.alloca_id] {
            AllocaData::UserAlloca(user_alloca) => user_alloca.ty,
            AllocaData::CompilerAlloca(compiler_alloca) => compiler_alloca.ty.into(),
        }
    }

    fn get_reachability(&self, _: &AllocaAddress) -> Reachability { Reachability::Reachable }

    fn get_span(&self, value: &AllocaAddress) -> Span { value.span.clone() }
}

/// Is an address pointing to a particular memory location.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Address {
    AllocaAddress(AllocaAddress),
    FieldAddress(FieldAddress),
}

impl Inspectable<Address> for Hir {
    fn get_value_type_id(&self, value: &Address) -> ValueTypeID {
        match value {
            Address::AllocaAddress(alloca_address) => self.get_value_type_id(alloca_address),
            Address::FieldAddress(field_address) => self.get_value_type_id(field_address),
        }
    }

    fn get_reachability(&self, value: &Address) -> Reachability {
        match value {
            Address::AllocaAddress(alloca_address) => self.get_reachability(alloca_address),
            Address::FieldAddress(field_address) => self.get_reachability(field_address),
        }
    }

    fn get_span(&self, value: &Address) -> Span {
        match value {
            Address::AllocaAddress(alloca_address) => self.get_span(alloca_address),
            Address::FieldAddress(field_address) => self.get_span(field_address),
        }
    }
}

/// Represents the reachability of a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, EnumAsInner, From)]
pub enum Reachability {
    Reachable,
    Unreachable,
}

/// Represents a type of a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundType {
    /// The type of the value.
    pub inferable_type: InferableType,

    /// The reachability of the value.
    pub reachability: Reachability,
}

implements_data_with_id! {
    /// Represents a single distinct variable in the SSA form.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct SsaVariable {
        /// The value that is assigned to the variable.
        pub binding: Binding,
    }
}

/// Represents a stack allocated memory that satisfies the need of user-defined variable declaration
/// statements.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UserAlloca {
    /// The type binding of the alloca.
    pub ty: ValueTypeID,

    /// Whether the declaration statement is mutable or not.
    pub is_mutable: bool,

    /// The identifier of the variable declaration statement that this alloca is associated with.
    pub identifier: IdentifierToken,
}

/// Represents an implicit stack allocated memory that is used for the compiler generated code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompilerAlloca {
    /// The type of the memory that this `alloca` allocates.
    pub ty: Type,
}

implements_data_with_id! {
    /// Represents a stack memory allocation value in the SSA form.
    #[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
    pub enum Alloca {
        UserAlloca(UserAlloca),
        CompilerAlloca(CompilerAlloca),
    }
}

/// A trait that allows the [`Hir`] to inspect the value.
pub trait Inspectable<T: ?Sized> {
    /// Gets the [`ValueTypeID`] of the value.
    fn get_value_type_id(&self, value: &T) -> ValueTypeID;

    /// Gets the [`Reachability`] of the value.
    fn get_reachability(&self, value: &T) -> Reachability;

    /// Gets the [`Span`] of the value.
    fn get_span(&self, value: &T) -> Span;
}

/// Is the value of numeric literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NumericLiteral {
    /// The numeric literal token that was used to create this value.
    #[get = "pub"]
    numeric_literal_token: NumericLiteralToken,

    value_type_id: ValueTypeID,
}

impl Inspectable<NumericLiteral> for Hir {
    fn get_value_type_id(&self, value: &NumericLiteral) -> ValueTypeID { value.value_type_id }

    fn get_reachability(&self, _: &NumericLiteral) -> Reachability { Reachability::Reachable }

    fn get_span(&self, value: &NumericLiteral) -> Span { value.numeric_literal_token.span.clone() }
}

/// Is the value of boolean literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct BooleanLiteral {
    /// Specifies where the literal is located in the source code.
    #[get = "pub"]
    span: Span,

    /// The value of the literal.
    #[get_copy = "pub"]
    value: bool,
}

impl Inspectable<BooleanLiteral> for Hir {
    fn get_value_type_id(&self, _: &BooleanLiteral) -> ValueTypeID {
        ValueTypeID::Type(Type::PrimitiveType(PrimitiveType::Bool))
    }

    fn get_reachability(&self, _: &BooleanLiteral) -> Reachability { Reachability::Reachable }

    fn get_span(&self, value: &BooleanLiteral) -> Span { value.span.clone() }
}

/// Is the value of string literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct EnumLiteral {
    /// Specifies where the literal is located in the source code.
    #[get = "pub"]
    span: Span,

    /// The [`EnumVariantID`] of the enum variant that this literal represents.
    #[get_copy = "pub"]
    enum_variant_id: EnumVariantID,
}

impl Inspectable<EnumLiteral> for Hir {
    fn get_value_type_id(&self, value: &EnumLiteral) -> ValueTypeID {
        ValueTypeID::Type(Type::TypedID(
            self.table[value.enum_variant_id].parent_enum_id().into(),
        ))
    }

    fn get_reachability(&self, _: &EnumLiteral) -> Reachability { Reachability::Reachable }

    fn get_span(&self, value: &EnumLiteral) -> Span { value.span.clone() }
}

/// Is an enumeration of all literals.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Literal {
    NumericLiteral(NumericLiteral),
    BooleanLiteral(BooleanLiteral),
    EnumLiteral(EnumLiteral),
}

impl Inspectable<Literal> for Hir {
    fn get_value_type_id(&self, value: &Literal) -> ValueTypeID {
        match value {
            Literal::NumericLiteral(numeric_literal) => self.get_value_type_id(numeric_literal),
            Literal::BooleanLiteral(boolean_literal) => self.get_value_type_id(boolean_literal),
            Literal::EnumLiteral(enum_literal) => self.get_value_type_id(enum_literal),
        }
    }

    fn get_reachability(&self, value: &Literal) -> Reachability {
        match value {
            Literal::NumericLiteral(numeric_literal) => self.get_reachability(numeric_literal),
            Literal::BooleanLiteral(boolean_literal) => self.get_reachability(boolean_literal),
            Literal::EnumLiteral(enum_literal) => self.get_reachability(enum_literal),
        }
    }

    fn get_span(&self, value: &Literal) -> Span {
        match value {
            Literal::NumericLiteral(numeric_literal) => self.get_span(numeric_literal),
            Literal::BooleanLiteral(boolean_literal) => self.get_span(boolean_literal),
            Literal::EnumLiteral(enum_literal) => self.get_span(enum_literal),
        }
    }
}

/// Is the value that can be used in bindings.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SsaValue {
    /// Yielded by the literal expressions.
    Literal(Literal),

    /// The value is stored in the [`SsaVariable`].
    SsaVariableID(SsaVariableID),
}

impl Inspectable<SsaValue> for Hir {
    fn get_value_type_id(&self, value: &SsaValue) -> ValueTypeID {
        match value {
            SsaValue::Literal(literal) => self.get_value_type_id(literal),
            SsaValue::SsaVariableID(ssa_variable_id) => {
                self.get_value_type_id(&self.variables_by_id[ssa_variable_id].binding)
            }
        }
    }

    fn get_reachability(&self, value: &SsaValue) -> Reachability {
        match value {
            SsaValue::Literal(literal) => self.get_reachability(literal),
            SsaValue::SsaVariableID(ssa_variable_id) => {
                self.get_reachability(&self.variables_by_id[ssa_variable_id].binding)
            }
        }
    }

    fn get_span(&self, value: &SsaValue) -> Span {
        match value {
            SsaValue::Literal(literal) => self.get_span(literal),
            SsaValue::SsaVariableID(ssa_variable_id) => {
                self.get_span(&self.variables_by_id[ssa_variable_id].binding)
            }
        }
    }
}
