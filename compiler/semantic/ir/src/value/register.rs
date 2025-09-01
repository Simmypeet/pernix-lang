//! Contains the definition of [`Register`] and [`Assignment`].
//!
//! The register is a place where SSA values are stored. The assignment is the
//! value that is stored in the register.

use std::{collections::BTreeSet, ops::Deref};

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_semantic_element::{fields::Field, return_type::get_return_type};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{member::get_members, parent::get_parent};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, Symbol},
    instantiation::Instantiation,
    lifetime::Lifetime,
    r#type::{Primitive, Qualifier, Type},
    tuple,
};
use pernixc_type_system::{
    environment::Environment, normalizer::Normalizer, Error, Succeeded,
};

use super::Value;
use crate::{
    address::Address, control_flow_graph::Block, value::TypeOf, Values,
};

/// Represents an element of a [`Tuple`].
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct TupleElement {
    /// The value of the tuple element.
    pub value: Value,

    /// Whether the tuple element is unpacked.
    pub is_unpacked: bool,
}

/// Represents a tuple of values.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Tuple {
    /// The elements of kthe tuple.
    pub elements: Vec<TupleElement>,
}

impl Tuple {
    /// Returns the list of registers that are used in the tuple.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.elements
            .iter()
            .filter_map(|x| x.value.as_register().copied())
            .collect()
    }
}

async fn type_of_tuple_assignment(
    values: &Values,
    tuple: &Tuple,
    current_site: Global<pernixc_symbol::ID>,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Succeeded<Type>, Error> {
    let mut constraints = BTreeSet::new();
    let mut elements = Vec::new();

    for element in &tuple.elements {
        let Succeeded { result: ty, constraints: new_constraint } =
            Box::pin(values.type_of(&element.value, current_site, environment))
                .await?;

        constraints.extend(new_constraint);

        if element.is_unpacked {
            match ty {
                Type::Tuple(ty) => elements.extend(ty.elements),
                ty => elements
                    .push(tuple::Element { term: ty, is_unpacked: true }),
            }
        } else {
            elements.push(tuple::Element { term: ty, is_unpacked: false });
        }
    }

    Ok(Succeeded::with_constraints(
        Type::Tuple(tuple::Tuple { elements }),
        constraints,
    ))
}

/// Represents a load/read from an address in memory. (The type must be Copy)
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Load {
    /// The address where the value is stored and will be read from.
    pub address: Address,
}

async fn type_of_load_assignment(
    values: &Values,
    load: &Load,
    current_site: Global<pernixc_symbol::ID>,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Succeeded<Type>, Error> {
    values.type_of(&load.address, current_site, environment).await
}

/// Obtains a reference at the given address.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Borrow {
    /// The address to the value.
    pub address: Address,

    /// The qualfier of the reference.
    pub qualifier: Qualifier,

    /// The lifetime introduces by the reference of operation.
    pub lifetime: Lifetime,
}

async fn type_of_reference_of_assignment(
    values: &Values,
    reference_of: &Borrow,
    current_site: Global<pernixc_symbol::ID>,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Succeeded<Type>, Error> {
    values.type_of(&reference_of.address, current_site, environment).await.map(
        |x| {
            x.map(|x| {
                Type::Reference(pernixc_term::r#type::Reference {
                    qualifier: reference_of.qualifier,
                    lifetime: reference_of.lifetime,
                    pointee: Box::new(x),
                })
            })
        },
    )
}

/// An enumeration of the different kinds of prefix operators.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub enum PrefixOperator {
    /// The value must be the signed numbers type.
    Negate,

    /// The value must be the boolean type.
    LogicalNot,

    /// The value must be integers.
    BitwiseNot,
}

/// A value applied with a prefix operator.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Prefix {
    /// The operand of the prefix operator.
    pub operand: Value,

    /// The operator applied to the operand.
    pub operator: PrefixOperator,
}

impl Prefix {
    /// Returns the register that is used in the prefix.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.operand.as_register().copied().into_iter().collect()
    }
}

async fn type_of_prefix_assignment(
    values: &Values,
    prefix: &Prefix,
    current_site: Global<pernixc_symbol::ID>,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Succeeded<Type>, Error> {
    let operand_type =
        Box::pin(values.type_of(&prefix.operand, current_site, environment))
            .await?;

    match prefix.operator {
        PrefixOperator::Negate
        | PrefixOperator::LogicalNot
        | PrefixOperator::BitwiseNot => Ok(operand_type),
    }
}

/// Represents a struct value.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct Struct {
    /// The struct ID of the struct.
    pub struct_id: Global<pernixc_symbol::ID>,

    /// The field initializers of the struct.
    pub initializers_by_field_id: HashMap<ID<Field>, Value>,

    /// The generic arguments supplied to the struct.
    pub generic_arguments: GenericArguments,
}

impl Struct {
    /// Returns the list of registers that are used in the struct.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.initializers_by_field_id
            .values()
            .filter_map(|x| x.as_register().copied())
            .collect()
    }
}

fn type_of_struct_assignment(st: &Struct) -> Type {
    Type::Symbol(Symbol {
        id: st.struct_id,
        generic_arguments: st.generic_arguments.clone(),
    })
}

/// Represents a variant value.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Variant {
    /// The variant ID of the variant.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The field initializers of the variant.
    pub associated_value: Option<Value>,

    /// The generic arguments supplied to the enum.
    pub generic_arguments: GenericArguments,
}

impl Variant {
    /// Returns the list of registers that are used in the variant.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.associated_value
            .as_ref()
            .map(|x| x.as_register().copied())
            .into_iter()
            .flatten()
            .collect()
    }
}

async fn type_of_variant_assignment(
    variant: &Variant,
    engine: &TrackedEngine,
) -> Type {
    let enum_id = engine.get_parent(variant.variant_id).await.unwrap();

    Type::Symbol(Symbol {
        id: Global::new(variant.variant_id.target_id, enum_id),
        generic_arguments: variant.generic_arguments.clone(),
    })
}

/// Represents a function call.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct FunctionCall {
    /// The ID of the function that is called.
    pub callable_id: Global<pernixc_symbol::ID>,

    /// The arguments supplied to the function.
    pub arguments: Vec<Value>,

    /// The generic instantiations of the function.
    pub instantiation: Instantiation,
}

impl FunctionCall {
    /// Returns the list of registers that are used in the function call.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.arguments.iter().filter_map(|x| x.as_register().copied()).collect()
    }
}

async fn type_of_function_call_assignment(
    function_call: &FunctionCall,
    engine: &TrackedEngine,
) -> Result<Type, Error> {
    let mut return_type = engine
        .get_return_type(function_call.callable_id)
        .await?
        .deref()
        .clone();

    function_call.instantiation.instantiate(&mut return_type);

    Ok(return_type)
}

/// Represents an arithmetic operator that works on numbers.
///
/// The both lhs and rhs operands are required to have the same type. The return
/// type is the same as the operands.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub enum ArithmeticOperator {
    /// Supports for all numeric types.
    Add,

    /// Supports for all numeric types.
    Subtract,

    /// Supports for all numeric types.
    Multiply,

    /// Supports for all numeric types.
    Divide,

    /// Supports for all numeric ypes.
    Modulo,
}

/// Represents a relational operator that works on numbers and booleans.
///
/// The both lhs and rhs operands are required to have the same type. The return
/// type is always boolean.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub enum RelationalOperator {
    /// Supports for all numeric and boolean types.
    LessThan,

    /// Supports for all numeric and boolean types.
    LessThanOrEqual,

    /// Supports for all numeric and boolean types.
    GreaterThan,

    /// Supports for all numeric and boolean types.
    GreaterThanOrEqual,

    /// Supports for all numeric and boolean types.
    Equal,

    /// Supports for all numeric and boolean types.
    NotEqual,
}

/// Represents a bitwise operator that works on integers and booleans.
///
/// Except for `ShiftLeft` and `ShiftRight`, the both lhs and rhs operands are
/// required to have the same type. The return type is the same as the operands.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub enum BitwiseOperator {
    /// Supports for all integer and boolean types.
    And,

    /// Supports for all integer and boolean types.
    Or,

    /// Supports for all integer and boolean types.
    Xor,

    /// Supports for all integer types. The lhs and rhs operands aren't
    /// required to have the same type.
    LeftShift,

    /// Supports for all integer types. The lhs and rhs operands aren't
    /// required to have the same type.
    RightShift,
}

/// An enumeration of all binary operators.
///
/// The operator doesn't includes `and` and `or` operators as they are
/// defined in another kind of register.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
#[allow(missing_docs)]
pub enum BinaryOperator {
    Arithmetic(ArithmeticOperator),
    Relational(RelationalOperator),
    Bitwise(BitwiseOperator),
}

/// Represents a binary expression.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Binary {
    /// The left-hand side operand.
    pub lhs: Value,

    /// The right-hand side operand.
    pub rhs: Value,

    /// The operator applied to the operands.
    pub operator: BinaryOperator,
}

impl Binary {
    /// Returns the list of registers that are used in the binary.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.lhs
            .as_register()
            .copied()
            .into_iter()
            .chain(self.rhs.as_register().copied())
            .collect()
    }
}

async fn type_of_binary(
    values: &Values,
    binary: &Binary,
    current_site: Global<pernixc_symbol::ID>,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Succeeded<Type>, Error> {
    // the return type always based on the lhs field
    if let BinaryOperator::Relational(_) = binary.operator {
        Ok(Succeeded::new(Type::Primitive(
            pernixc_term::r#type::Primitive::Bool,
        )))
    } else {
        Box::pin(values.type_of(&binary.lhs, current_site, environment)).await
    }
}

/// Represents a phi node in the SSA form.
///
/// A phi node is used to determine the value based on the flow of the
/// execution. This is typcially used in the control flow related expressions
/// such as `if` and `match`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct Phi {
    /// Maps the incoming block to the value.
    pub incoming_values: HashMap<ID<Block>, Value>,

    /// The type of the phi node.
    ///
    /// The type must be declared separately as the incoming values can have
    /// different lifetime values; thus, the type of the phi node can't be
    /// solely determined by one of the incoming values.
    pub r#type: Type,
}

impl Phi {
    /// Returns the list of registers that are used in the phi node.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.incoming_values
            .values()
            .filter_map(|x| x.as_register().copied())
            .collect()
    }
}

/// Represents an array of values.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Array {
    /// The elements of the array.
    pub elements: Vec<Value>,

    /// The type of the element in the array.
    ///
    /// The type must be declared separately as the element values can have
    /// different lifetime values; thus, the type of the array can't be solely
    /// determined by one of the element values.
    pub element_type: Type,
}

impl Array {
    /// Returns the list of registers that are used in the array.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.elements.iter().filter_map(|x| x.as_register().copied()).collect()
    }
}

/// Represents a cast operation.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Cast {
    /// The value to be casted.
    pub value: Value,

    /// The type to cast the value to.
    pub r#type: Type,
}

impl Cast {
    /// Returns the register that is used in the cast.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.value.as_register().copied().into_iter().collect()
    }
}

/// Returns the variant number of the given address to the enum.
///
/// The variant number is supposed to be a unique identifier specifying which
/// variant is active in the enum. The number should correspond to the
/// declration order of it in the enum.
///
/// This is primarily used in the pattern matching.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct VariantNumber {
    /// Address to the num to get the variant number of.
    pub address: Address,

    /// The enum ID of the enum.
    pub enum_id: Global<pernixc_symbol::ID>,
}

async fn type_of_variant_number(
    variant_number: &VariantNumber,
    environment: &Environment<'_, impl Normalizer>,
) -> Type {
    let mut answer = Primitive::Bool;
    let members =
        environment.tracked_engine().get_members(variant_number.enum_id).await;

    let variant_count =
        members.member_ids_by_name.len() + members.unnameds.iter().len();

    let int_capacity = |a: Primitive| match a {
        Primitive::Bool => 2u64,
        Primitive::Uint8 => 2u64.pow(8),
        Primitive::Uint16 => 2u64.pow(16),
        Primitive::Uint32 => 2u64.pow(32),
        Primitive::Uint64 => 2u64.pow(64),
        _ => unreachable!(),
    };

    let next = |a: Primitive| match a {
        Primitive::Bool => Primitive::Uint8,
        Primitive::Uint8 => Primitive::Uint16,
        Primitive::Uint16 => Primitive::Uint32,
        Primitive::Uint32 => Primitive::Uint64,
        _ => unreachable!(),
    };

    while int_capacity(answer) < variant_count as u64 {
        answer = next(answer);
    }

    Type::Primitive(answer)
}

/// An enumeration of the different kinds of values that can be assigned in the
/// register.
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumAsInner, StableHash,
)]
#[allow(missing_docs)]
pub enum Assignment {
    Tuple(Tuple),
    Load(Load),
    Borrow(Borrow),
    Prefix(Prefix),
    Struct(Struct),
    Variant(Variant),
    FunctionCall(FunctionCall),
    Binary(Binary),
    Array(Array),
    Phi(Phi),
    Cast(Cast),
    VariantNumber(VariantNumber),
}

impl Assignment {
    /// Returns the register that is used in the assignment.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        match self {
            Self::Tuple(tuple) => tuple.get_used_registers(),
            Self::Prefix(prefix) => prefix.get_used_registers(),
            Self::Struct(st) => st.get_used_registers(),
            Self::Variant(variant) => variant.get_used_registers(),
            Self::FunctionCall(function_call) => {
                function_call.get_used_registers()
            }
            Self::Binary(binary) => binary.get_used_registers(),
            Self::Array(array) => array.get_used_registers(),
            Self::Phi(phi) => phi.get_used_registers(),
            Self::Cast(cast) => cast.get_used_registers(),

            Self::Load(_) | Self::Borrow(_) | Self::VariantNumber(_) => {
                Vec::new()
            }
        }
    }
}

/// Represents a register in the SSA from.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct Register {
    /// The value stored in the register.
    pub assignment: Assignment,

    /// The span where the value was defined.
    pub span: Option<RelativeSpan>,
}

impl TypeOf<ID<Register>> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        id: ID<Register>,
        current_site: Global<pernixc_symbol::ID>,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        let register = &self.registers[id];

        let ty = match &register.assignment {
            Assignment::Tuple(tuple) => {
                return type_of_tuple_assignment(
                    self,
                    tuple,
                    current_site,
                    environment,
                )
                .await
            }
            Assignment::Load(load) => {
                return type_of_load_assignment(
                    self,
                    load,
                    current_site,
                    environment,
                )
                .await
            }
            Assignment::Borrow(reference_of) => {
                return type_of_reference_of_assignment(
                    self,
                    reference_of,
                    current_site,
                    environment,
                )
                .await
            }
            Assignment::Prefix(prefix) => {
                return type_of_prefix_assignment(
                    self,
                    prefix,
                    current_site,
                    environment,
                )
                .await
            }
            Assignment::Struct(st) => Ok(type_of_struct_assignment(st)),
            Assignment::Variant(variant) => Ok(type_of_variant_assignment(
                variant,
                environment.tracked_engine(),
            )
            .await),
            Assignment::FunctionCall(function_call) => {
                type_of_function_call_assignment(
                    function_call,
                    environment.tracked_engine(),
                )
                .await
            }
            Assignment::Binary(binary) => {
                return type_of_binary(self, binary, current_site, environment)
                    .await;
            }
            Assignment::Phi(phi_node) => Ok(phi_node.r#type.clone()),
            Assignment::Array(array) => {
                Ok(Type::Array(pernixc_term::r#type::Array {
                    r#type: Box::new(array.element_type.clone()),
                    length: Constant::Primitive(
                        pernixc_term::constant::Primitive::Usize(
                            array.elements.len() as u64,
                        ),
                    ),
                }))
            }
            Assignment::Cast(cast) => Ok(cast.r#type.clone()),
            Assignment::VariantNumber(variant) => {
                return Ok(Succeeded::new(
                    type_of_variant_number(variant, environment).await,
                ))
            }
        }?;

        Ok(environment.simplify(ty).await?.deref().clone())
    }
}
