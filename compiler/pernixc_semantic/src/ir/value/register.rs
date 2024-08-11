//! Contains the definition of [`Register`] and [`Assignment`].
//!
//! The register is a place where SSA values are stored. The assignment is the
//! value that is stored in the register.

use std::collections::HashMap;

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use super::Value;
use crate::{
    arena::ID,
    ir::{
        address::Address, control_flow_graph::Block,
        representation::Representation, TypeOfError,
    },
    symbol::{
        self,
        table::{self, representation::Index, Table},
        CallableID, Field, GlobalID,
    },
    type_system::{
        environment::Environment,
        instantiation::{self, Instantiation},
        model::Model,
        normalizer::Normalizer,
        simplify,
        term::{
            self,
            constant::{self, Constant},
            lifetime::Lifetime,
            r#type::{self, Qualifier, SymbolID, Type},
            GenericArguments, Local, Symbol,
        },
    },
};

/// Represents an element of a [`Tuple`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleElement<M: Model> {
    /// The value of the tuple element.
    pub value: Value<M>,

    /// Whether the tuple element is unpacked.
    pub is_unpacked: bool,
}

/// Represents a tuple of values.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<M: Model> {
    /// The elements of kthe tuple.
    pub elements: Vec<TupleElement<M>>,
}

impl<M: Model> Representation<M> {
    fn type_of_tuple_assignment(
        &self,
        tuple: &Tuple<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl table::State, impl Normalizer<M>>,
    ) -> Result<Type<M>, TypeOfError<M>> {
        let mut elements = Vec::new();

        for element in &tuple.elements {
            let ty =
                self.type_of_value(&element.value, current_site, environment)?;

            if element.is_unpacked {
                match ty {
                    Type::Tuple(ty) => elements.extend(ty.elements),
                    ty => elements.push(term::TupleElement {
                        term: ty,
                        is_unpacked: true,
                    }),
                }
            } else {
                elements
                    .push(term::TupleElement { term: ty, is_unpacked: false });
            }
        }

        let mut ty = Type::Tuple(term::Tuple { elements });
        ty = simplify::simplify(&ty, environment).result;

        Ok(ty)
    }
}

/// An enumeration of either moving or copying loads.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LoadKind {
    /// The value is memcpy'd from the address and the value in the address is
    /// invalidated.
    Move,

    /// The value is copied from the address via `Copy` trait.
    Copy,
}

/// Represents a load/read from an address in memory.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Load<M: Model> {
    /// The address where the value is stored and will be read from.
    pub address: Address<M>,

    /// The kind of load.
    pub kind: LoadKind,
}

impl<M: Model> Representation<M> {
    fn type_of_load_assignment(
        &self,
        load: &Load<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl table::State, impl Normalizer<M>>,
    ) -> Result<Type<M>, TypeOfError<M>> {
        self.type_of_address(&load.address, current_site, environment)
    }
}

/// Obtains a reference at the given address.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceOf<M: Model> {
    /// The address to the value.
    pub address: Address<M>,

    /// The qualfier of the reference.
    pub qualifier: Qualifier,

    /// Checks if the reference of operation is local (with `@` operator).
    pub is_local: bool,

    /// The lifetime introduces by the reference of operation.
    pub lifetime: Lifetime<M>,
}

impl<M: Model> Representation<M> {
    fn type_of_reference_of_assignment(
        &self,
        reference_of: &ReferenceOf<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl table::State, impl Normalizer<M>>,
    ) -> Result<Type<M>, TypeOfError<M>> {
        let mut ty = self.type_of_address(
            &reference_of.address,
            current_site,
            environment,
        )?;

        Ok(if reference_of.is_local {
            ty = simplify::simplify(&ty, environment).result;

            match ty {
                Type::Local(local) => Type::Reference(r#type::Reference {
                    qualifier: reference_of.qualifier,
                    lifetime: reference_of.lifetime.clone(),
                    pointee: local.0,
                }),

                another_ty => {
                    return Err(TypeOfError::NonLocalAddressType {
                        address: reference_of.address.clone(),
                        r#type: another_ty,
                    })
                }
            }
        } else {
            Type::Reference(r#type::Reference {
                pointee: Box::new(ty),
                qualifier: reference_of.qualifier,
                lifetime: reference_of.lifetime.clone(),
            })
        })
    }
}

/// An enumeration of the different kinds of prefix operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOperator {
    /// The value must be the signed numbers type.
    Negate,

    /// The value must be the boolean type.
    LogicalNot,

    /// The value must be integers.
    BitwiseNot,

    /// The value can be any type.
    Local,

    /// The value must be a type of `local`.
    Unlocal,
}

/// A value applied with a prefix operator.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prefix<M: Model> {
    /// The operand of the prefix operator.
    pub operand: Value<M>,

    /// The operator applied to the operand.
    pub operator: PrefixOperator,
}

impl<M: Model> Representation<M> {
    fn type_of_prefix_assignment(
        &self,
        prefix: &Prefix<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl table::State, impl Normalizer<M>>,
    ) -> Result<Type<M>, TypeOfError<M>> {
        let mut operand_type =
            self.type_of_value(&prefix.operand, current_site, environment)?;

        match prefix.operator {
            PrefixOperator::Negate
            | PrefixOperator::LogicalNot
            | PrefixOperator::BitwiseNot => Ok(operand_type),

            PrefixOperator::Local => {
                Ok(Type::Local(Local(Box::new(operand_type))))
            }

            PrefixOperator::Unlocal => {
                operand_type =
                    simplify::simplify(&operand_type, environment).result;

                let inner = match operand_type {
                    Type::Local(inner) => *inner.0,
                    ty => {
                        return Err(TypeOfError::NonLocalAssignmentType {
                            value: prefix.operand.clone(),
                            r#type: ty,
                        })
                    }
                };

                Ok(inner)
            }
        }
    }
}

/// Represents a struct value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct<M: Model> {
    /// The struct ID of the struct.
    pub struct_id: ID<symbol::Struct>,

    /// The field initializers of the struct.
    pub initializers_by_field_id: HashMap<ID<Field>, Value<M>>,

    /// The generic arguments supplied to the struct.
    pub generic_arguments: GenericArguments<M>,
}

fn type_of_struct_assignment<M: Model>(st: &Struct<M>) -> Type<M> {
    Type::Symbol(Symbol {
        id: SymbolID::Struct(st.struct_id),
        generic_arguments: st.generic_arguments.clone(),
    })
}

/// Represents a variant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant<M: Model> {
    /// The variant ID of the variant.
    pub variant_id: ID<symbol::Variant>,

    /// The field initializers of the variant.
    pub associated_value: Option<Value<M>>,

    /// The generic arguments supplied to the enum.
    pub generic_arguments: GenericArguments<M>,
}

fn type_of_variant_assignment<M: Model>(
    variant: &Variant<M>,
    table: &Table<impl table::State>,
) -> Result<Type<M>, TypeOfError<M>> {
    let enum_id = table
        .get(variant.variant_id)
        .ok_or(TypeOfError::InvalidGlobalID(variant.variant_id.into()))?
        .parent_enum_id();

    Ok(Type::Symbol(Symbol {
        id: SymbolID::Enum(enum_id),
        generic_arguments: variant.generic_arguments.clone(),
    }))
}

/// Represents a function call.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionCall<M: Model> {
    /// The ID of the function that is called.
    pub callable_id: CallableID,

    /// The arguments supplied to the function.
    pub arguments: Vec<Value<M>>,

    /// The generic instantiations of the function.
    pub instantiation: Instantiation<M>,
}

fn type_of_function_call_assignment<M: Model>(
    function_call: &FunctionCall<M>,
    table: &Table<impl table::State>,
) -> Result<Type<M>, TypeOfError<M>> {
    let callable = table.get_callable(function_call.callable_id).ok_or(
        TypeOfError::InvalidGlobalID(function_call.callable_id.into()),
    )?;

    let mut return_type = M::from_default_type(callable.return_type().clone());

    instantiation::instantiate(&mut return_type, &function_call.instantiation);

    Ok(return_type)
}

/// Represents an arithmetic operator that works on numbers.
///
/// The both lhs and rhs operands are required to have the same type. The return
/// type is the same as the operands.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ArithmeticOperator {
    /// Supports for all numeric types.
    Add,

    /// Supports for all numeric types.
    Subtract,

    /// Supports for all numeric types.
    Multiply,

    /// Supports for all numeric types.
    Divide,

    /// Supports for all integer types.
    Modulo,
}

/// Represents a relational operator that works on numbers and booleans.
///
/// The both lhs and rhs operands are required to have the same type. The return
/// type is always boolean.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum BinaryOperator {
    Arithmetic(ArithmeticOperator),
    Relational(RelationalOperator),
    Bitwise(BitwiseOperator),
}

/// Represents a binary expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binary<M: Model> {
    /// The left-hand side operand.
    pub lhs: Value<M>,

    /// The right-hand side operand.
    pub rhs: Value<M>,

    /// The operator applied to the operands.
    pub operator: BinaryOperator,
}

impl<M: Model> Representation<M> {
    fn type_of_binary(
        &self,
        binary: &Binary<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl table::State, impl Normalizer<M>>,
    ) -> Result<Type<M>, TypeOfError<M>> {
        match binary.operator {
            BinaryOperator::Bitwise(_) | BinaryOperator::Arithmetic(_) => {
                // jsut return the lhs type
                self.type_of_value(&binary.lhs, current_site, environment)
            }
            BinaryOperator::Relational(_) => {
                // always return boolean
                Ok(Type::Primitive(r#type::Primitive::Bool))
            }
        }
    }
}

/// Represents a phi node in the SSA form.
///
/// A phi node is used to determine the value based on the flow of the
/// execution. This is typcially used in the control flow related expressions
/// such as `if` and `match`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Phi<M: Model> {
    /// Maps the incoming block to the value.
    pub incoming_values: HashMap<ID<Block<M>>, Value<M>>,

    /// The type of the phi node.
    ///
    /// The type must be declared separately as the incoming values can have
    /// different lifetime values; thus, the type of the phi node can't be
    /// solely determined by one of the incoming values.
    pub r#type: Type<M>,
}

/// Represents an array of values.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array<M: Model> {
    /// The elements of the array.
    pub elements: Vec<Value<M>>,

    /// The type of the element in the array.
    ///
    /// The type must be declared separately as the element values can have
    /// different lifetime values; thus, the type of the array can't be solely
    /// determined by one of the element values.
    pub element_type: Type<M>,
}

/// Represents a cast operation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cast<M: Model> {
    /// The value to be casted.
    pub value: Value<M>,

    /// The type to cast the value to.
    pub r#type: Type<M>,
}

/// An enumeration of the different kinds of values that can be assigned in the
/// register.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
#[allow(missing_docs)]
pub enum Assignment<M: Model> {
    Tuple(Tuple<M>),
    Load(Load<M>),
    ReferenceOf(ReferenceOf<M>),
    Prefix(Prefix<M>),
    Struct(Struct<M>),
    Variant(Variant<M>),
    FunctionCall(FunctionCall<M>),
    Binary(Binary<M>),
    Array(Array<M>),
    Phi(Phi<M>),
    Cast(Cast<M>),
}

/// Represents a register in the SSA from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Register<M: Model> {
    /// The value stored in the register.
    pub assignment: Assignment<M>,

    /// The span where the value was defined.
    pub span: Option<Span>,
}

impl<M: Model> Representation<M> {
    /// Gets the type of the [`Register`] with the given ID.
    ///
    /// # Parameters
    ///
    /// - `id`: The ID of the register to get the type of.
    /// - `current_site`: The site where the IR binding is being taken place in.
    /// - `table`: The table to get the required information from.
    ///
    /// # Errors
    ///
    /// See [`TypeOfError`] for the possible errors that can occur.
    pub fn type_of_register(
        &self,
        id: ID<Register<M>>,
        current_site: GlobalID,
        environment: &Environment<M, impl table::State, impl Normalizer<M>>,
    ) -> Result<Type<M>, TypeOfError<M>> {
        let register = self
            .registers()
            .get(id)
            .ok_or(TypeOfError::InvalidRegisterID(id))?;

        match &register.assignment {
            Assignment::Tuple(tuple) => {
                self.type_of_tuple_assignment(tuple, current_site, environment)
            }
            Assignment::Load(load) => {
                self.type_of_load_assignment(load, current_site, environment)
            }
            Assignment::ReferenceOf(reference_of) => self
                .type_of_reference_of_assignment(
                    reference_of,
                    current_site,
                    environment,
                ),
            Assignment::Prefix(prefix) => self.type_of_prefix_assignment(
                prefix,
                current_site,
                environment,
            ),
            Assignment::Struct(st) => Ok(type_of_struct_assignment(st)),
            Assignment::Variant(variant) => {
                type_of_variant_assignment(variant, environment.table())
            }
            Assignment::FunctionCall(function_call) => {
                type_of_function_call_assignment(
                    function_call,
                    environment.table(),
                )
            }
            Assignment::Binary(binary) => {
                self.type_of_binary(binary, current_site, environment)
            }
            Assignment::Phi(phi_node) => Ok(phi_node.r#type.clone()),
            Assignment::Array(array) => Ok(Type::Array(r#type::Array {
                r#type: Box::new(array.element_type.clone()),
                length: Constant::Primitive(constant::Primitive::Integer(
                    array.elements.len() as i128,
                )),
            })),
            Assignment::Cast(cast) => Ok(cast.r#type.clone()),
        }
    }
}
