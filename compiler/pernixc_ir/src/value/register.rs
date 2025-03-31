//! Contains the definition of [`Register`] and [`Assignment`].
//!
//! The register is a place where SSA values are stored. The assignment is the
//! value that is stored in the register.

use std::{
    collections::{BTreeSet, HashMap},
    ops::Deref,
};

use enum_as_inner::EnumAsInner;
use pernixc_abort::Abort;
use pernixc_arena::{Key, ID};
use pernixc_component::{
    fields::Field, function_signature::FunctionSignature,
    implementation::Implementation,
};
use pernixc_source_file::Span;
use pernixc_semantic::{
    component::{Member, Parent, SymbolKind},
    GlobalID, Table,
};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::GenericParameters,
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    r#type::{Primitive, Qualifier, Type},
    MemberSymbol, ModelOf, Symbol,
};
use pernixc_type_system::{
    environment::Environment, normalizer::Normalizer, Error, Succeeded,
};
use serde::{Deserialize, Serialize};

use super::Value;
use crate::{
    address::Address, control_flow_graph::Block, model::Transform, Values,
};

/// Represents an element of a [`Tuple`].
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct TupleElement<M: pernixc_term::Model> {
    /// The value of the tuple element.
    pub value: Value<M>,

    /// Whether the tuple element is unpacked.
    pub is_unpacked: bool,
}

/// Represents a tuple of values.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Tuple<M: pernixc_term::Model> {
    /// The elements of kthe tuple.
    pub elements: Vec<TupleElement<M>>,
}

impl<M: pernixc_term::Model> Tuple<M> {
    /// Returns the list of registers that are used in the tuple.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register<M>>> {
        self.elements
            .iter()
            .filter_map(|x| x.value.as_register().copied())
            .collect()
    }
}

impl<M: pernixc_term::Model> Values<M> {
    fn type_of_tuple_assignment(
        &self,
        tuple: &Tuple<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Succeeded<Type<M>, M>, Error> {
        let mut constraints = BTreeSet::new();
        let mut elements = Vec::new();

        for element in &tuple.elements {
            let Succeeded { result: ty, constraints: new_constraint } =
                self.type_of_value(&element.value, current_site, environment)?;

            constraints.extend(new_constraint);

            if element.is_unpacked {
                match ty {
                    Type::Tuple(ty) => elements.extend(ty.elements),
                    ty => elements.push(pernixc_term::TupleElement {
                        term: ty,
                        is_unpacked: true,
                    }),
                }
            } else {
                elements.push(pernixc_term::TupleElement {
                    term: ty,
                    is_unpacked: false,
                });
            }
        }

        Ok(Succeeded::with_constraints(
            Type::Tuple(pernixc_term::Tuple { elements }),
            constraints,
        ))
    }
}

/// Represents a load/read from an address in memory. (The type must be Copy)
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Load<M: pernixc_term::Model> {
    /// The address where the value is stored and will be read from.
    pub address: Address<M>,
}

impl<M: pernixc_term::Model> Values<M> {
    fn type_of_load_assignment(
        &self,
        load: &Load<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Succeeded<Type<M>, M>, Error> {
        self.type_of_address(&load.address, current_site, environment)
    }
}

/// Obtains a reference at the given address.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Borrow<M: pernixc_term::Model> {
    /// The address to the value.
    pub address: Address<M>,

    /// The qualfier of the reference.
    pub qualifier: Qualifier,

    /// The lifetime introduces by the reference of operation.
    pub lifetime: Lifetime<M>,
}

impl<M: pernixc_term::Model> Values<M> {
    fn type_of_reference_of_assignment(
        &self,
        reference_of: &Borrow<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Succeeded<Type<M>, M>, Error> {
        self.type_of_address(&reference_of.address, current_site, environment)
            .map(|x| {
                x.map(|x| {
                    Type::Reference(pernixc_term::r#type::Reference {
                        qualifier: reference_of.qualifier,
                        lifetime: reference_of.lifetime.clone(),
                        pointee: Box::new(x),
                    })
                })
            })
    }
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
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Prefix<M: pernixc_term::Model> {
    /// The operand of the prefix operator.
    pub operand: Value<M>,

    /// The operator applied to the operand.
    pub operator: PrefixOperator,
}

impl<M: pernixc_term::Model> Prefix<M> {
    /// Returns the register that is used in the prefix.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register<M>>> {
        self.operand.as_register().copied().into_iter().collect()
    }
}

impl<M: pernixc_term::Model> Values<M> {
    fn type_of_prefix_assignment(
        &self,
        prefix: &Prefix<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Succeeded<Type<M>, M>, Error> {
        let operand_type =
            self.type_of_value(&prefix.operand, current_site, environment)?;

        match prefix.operator {
            PrefixOperator::Negate
            | PrefixOperator::LogicalNot
            | PrefixOperator::BitwiseNot => Ok(operand_type),
        }
    }
}

/// Represents a struct value.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Struct<M: pernixc_term::Model> {
    /// The struct ID of the struct.
    pub struct_id: GlobalID,

    /// The field initializers of the struct.
    pub initializers_by_field_id: HashMap<ID<Field>, Value<M>>,

    /// The generic arguments supplied to the struct.
    pub generic_arguments: GenericArguments<M>,
}

impl<M: pernixc_term::Model> Struct<M> {
    /// Returns the list of registers that are used in the struct.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register<M>>> {
        self.initializers_by_field_id
            .values()
            .filter_map(|x| x.as_register().copied())
            .collect()
    }
}

fn type_of_struct_assignment<M: pernixc_term::Model>(
    st: &Struct<M>,
) -> Type<M> {
    Type::Symbol(Symbol {
        id: st.struct_id,
        generic_arguments: st.generic_arguments.clone(),
    })
}

/// Represents a variant value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Variant<M: pernixc_term::Model> {
    /// The variant ID of the variant.
    pub variant_id: GlobalID,

    /// The field initializers of the variant.
    pub associated_value: Option<Value<M>>,

    /// The generic arguments supplied to the enum.
    pub generic_arguments: GenericArguments<M>,
}

impl<M: pernixc_term::Model> Variant<M> {
    /// Returns the list of registers that are used in the variant.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register<M>>> {
        self.associated_value
            .as_ref()
            .map(|x| x.as_register().copied())
            .into_iter()
            .flatten()
            .collect()
    }
}

fn type_of_variant_assignment<M: pernixc_term::Model>(
    variant: &Variant<M>,
    table: &Table,
) -> Type<M> {
    let enum_id = table.get::<Parent>(variant.variant_id).parent.unwrap();

    Type::Symbol(Symbol {
        id: GlobalID::new(variant.variant_id.target_id, enum_id),
        generic_arguments: variant.generic_arguments.clone(),
    })
}

/// Represents a function call.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct FunctionCall<M: pernixc_term::Model> {
    /// The ID of the function that is called.
    pub callable_id: GlobalID,

    /// The arguments supplied to the function.
    pub arguments: Vec<Value<M>>,

    /// The generic instantiations of the function.
    pub instantiation: Instantiation<M>,
}

impl<M: pernixc_term::Model> FunctionCall<M> {
    /// Returns the list of registers that are used in the function call.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register<M>>> {
        self.arguments.iter().filter_map(|x| x.as_register().copied()).collect()
    }
}

fn type_of_function_call_assignment<M: pernixc_term::Model>(
    function_call: &FunctionCall<M>,
    table: &Table,
) -> Result<Type<M>, Error> {
    let callable =
        table.query::<FunctionSignature>(function_call.callable_id)?;

    let mut return_type = M::from_default_type(callable.return_type.clone());

    instantiation::instantiate(&mut return_type, &function_call.instantiation);

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
)]
#[allow(missing_docs)]
pub enum BinaryOperator {
    Arithmetic(ArithmeticOperator),
    Relational(RelationalOperator),
    Bitwise(BitwiseOperator),
}

/// Represents a binary expression.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Binary<M: pernixc_term::Model> {
    /// The left-hand side operand.
    pub lhs: Value<M>,

    /// The right-hand side operand.
    pub rhs: Value<M>,

    /// The operator applied to the operands.
    pub operator: BinaryOperator,
}

impl<M: pernixc_term::Model> Binary<M> {
    /// Returns the list of registers that are used in the binary.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register<M>>> {
        self.lhs
            .as_register()
            .copied()
            .into_iter()
            .chain(self.rhs.as_register().copied())
            .collect()
    }
}

impl<M: pernixc_term::Model> Values<M> {
    fn type_of_binary(
        &self,
        binary: &Binary<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Succeeded<Type<M>, M>, Error> {
        // the return type always based on the lhs field
        if let BinaryOperator::Relational(_) = binary.operator {
            Ok(Succeeded::new(Type::Primitive(
                pernixc_term::r#type::Primitive::Bool,
            )))
        } else {
            self.type_of_value(&binary.lhs, current_site, environment)
        }
    }
}

/// Represents a phi node in the SSA form.
///
/// A phi node is used to determine the value based on the flow of the
/// execution. This is typcially used in the control flow related expressions
/// such as `if` and `match`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Phi<M: pernixc_term::Model> {
    /// Maps the incoming block to the value.
    pub incoming_values: HashMap<ID<Block<M>>, Value<M>>,

    /// The type of the phi node.
    ///
    /// The type must be declared separately as the incoming values can have
    /// different lifetime values; thus, the type of the phi node can't be
    /// solely determined by one of the incoming values.
    pub r#type: Type<M>,
}

impl<M: pernixc_term::Model> Phi<M> {
    /// Returns the list of registers that are used in the phi node.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register<M>>> {
        self.incoming_values
            .values()
            .filter_map(|x| x.as_register().copied())
            .collect()
    }
}

/// Represents an array of values.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Array<M: pernixc_term::Model> {
    /// The elements of the array.
    pub elements: Vec<Value<M>>,

    /// The type of the element in the array.
    ///
    /// The type must be declared separately as the element values can have
    /// different lifetime values; thus, the type of the array can't be solely
    /// determined by one of the element values.
    pub element_type: Type<M>,
}

impl<M: pernixc_term::Model> Array<M> {
    /// Returns the list of registers that are used in the array.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register<M>>> {
        self.elements.iter().filter_map(|x| x.as_register().copied()).collect()
    }
}

/// Represents a cast operation.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Cast<M: pernixc_term::Model> {
    /// The value to be casted.
    pub value: Value<M>,

    /// The type to cast the value to.
    pub r#type: Type<M>,
}

impl<M: pernixc_term::Model> Cast<M> {
    /// Returns the register that is used in the cast.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register<M>>> {
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
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct VariantNumber<M: pernixc_term::Model> {
    /// Address to the num to get the variant number of.
    pub address: Address<M>,

    /// The enum ID of the enum.
    pub enum_id: GlobalID,
}

impl<M: pernixc_term::Model> Values<M> {
    fn type_of_variant_number(
        variant_number: &VariantNumber<M>,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Type<M> {
        let mut answer = Primitive::Bool;
        let variant_count =
            environment.table().get::<Member>(variant_number.enum_id).len();

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
}

/// An enumeration of the different kinds of values that can be assigned in the
/// register.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumAsInner)]
#[allow(missing_docs)]
pub enum Assignment<M: pernixc_term::Model> {
    Tuple(Tuple<M>),
    Load(Load<M>),
    Borrow(Borrow<M>),
    Prefix(Prefix<M>),
    Struct(Struct<M>),
    Variant(Variant<M>),
    FunctionCall(FunctionCall<M>),
    Binary(Binary<M>),
    Array(Array<M>),
    Phi(Phi<M>),
    Cast(Cast<M>),
    VariantNumber(VariantNumber<M>),
}

impl<M: pernixc_term::Model> Assignment<M> {
    /// Returns the register that is used in the assignment.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register<M>>> {
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Register<M: pernixc_term::Model> {
    /// The value stored in the register.
    pub assignment: Assignment<M>,

    /// The span where the value was defined.
    #[serde(skip)]
    pub span: Option<Span>,
}

impl<M: pernixc_term::Model> Values<M> {
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
    /// See [`AbruptError`] for the possible errors that can occur.
    pub fn type_of_register(
        &self,
        id: ID<Register<M>>,
        current_site: GlobalID,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Succeeded<Type<M>, M>, Error> {
        let register = &self.registers[id];

        let ty = match &register.assignment {
            Assignment::Tuple(tuple) => {
                return self.type_of_tuple_assignment(
                    tuple,
                    current_site,
                    environment,
                )
            }
            Assignment::Load(load) => {
                return self.type_of_load_assignment(
                    load,
                    current_site,
                    environment,
                )
            }
            Assignment::Borrow(reference_of) => {
                return self.type_of_reference_of_assignment(
                    reference_of,
                    current_site,
                    environment,
                )
            }
            Assignment::Prefix(prefix) => {
                return self.type_of_prefix_assignment(
                    prefix,
                    current_site,
                    environment,
                )
            }
            Assignment::Struct(st) => Ok(type_of_struct_assignment(st)),
            Assignment::Variant(variant) => {
                Ok(type_of_variant_assignment(variant, environment.table()))
            }
            Assignment::FunctionCall(function_call) => {
                type_of_function_call_assignment(
                    function_call,
                    environment.table(),
                )
            }
            Assignment::Binary(binary) => {
                return self.type_of_binary(binary, current_site, environment);
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
                return Ok(Succeeded::new(Self::type_of_variant_number(
                    variant,
                    environment,
                )))
            }
        }?;

        Ok(environment.simplify(ty)?.deref().clone())
    }
}

fn transform_instantiation<
    E,
    FM: pernixc_term::Model,
    TM: pernixc_term::Model,
    T: Transform<Lifetime<FM>, Target = TM, Error = E>
        + Transform<Type<FM>, Target = TM, Error = E>
        + Transform<Constant<FM>, Target = TM, Error = E>,
>(
    transformer: &mut T,
    instantiation: Instantiation<FM>,
    span: Option<&Span>,
) -> Result<Instantiation<TM>, E> {
    Ok(Instantiation {
        lifetimes: instantiation
            .lifetimes
            .into_iter()
            .map(|(k, v)| {
                Ok((
                    transformer.transform(k, span)?,
                    transformer.transform(v, span)?,
                ))
            })
            .collect::<Result<_, E>>()?,
        types: instantiation
            .types
            .into_iter()
            .map(|(k, v)| {
                Ok((
                    transformer.transform(k, span)?,
                    transformer.transform(v, span)?,
                ))
            })
            .collect::<Result<_, E>>()?,
        constants: {
            instantiation
                .constants
                .into_iter()
                .map(|(k, v)| {
                    Ok((
                        transformer.transform(k, span)?,
                        transformer.transform(v, span)?,
                    ))
                })
                .collect::<Result<_, E>>()?
        },
    })
}

impl<M: pernixc_term::Model> Register<M> {
    /// Transforms the [`Register`] to another model using the given
    /// transformer.
    #[allow(clippy::too_many_lines, clippy::missing_errors_doc)]
    pub fn transform_model<
        E: From<Abort>,
        U: pernixc_term::Model,
        T: Transform<Lifetime<M>, Target = U, Error = E>
            + Transform<Type<M>, Target = U, Error = E>
            + Transform<Constant<M>, Target = U, Error = E>,
    >(
        self,
        transformer: &mut T,
        table: &Table,
    ) -> Result<Register<U>, E> {
        Ok(Register {
            span: self.span.clone(),
            assignment: match self.assignment {
                Assignment::Tuple(tuple) => Assignment::Tuple(Tuple {
                    elements: tuple
                        .elements
                        .into_iter()
                        .map(|x| {
                            Ok(TupleElement {
                                value: x.value.transform_model(transformer)?,
                                is_unpacked: x.is_unpacked,
                            })
                        })
                        .collect::<Result<_, E>>()?,
                }),
                Assignment::Load(load) => Assignment::Load(Load {
                    address: load.address.transform_model(transformer)?,
                }),
                Assignment::Borrow(reference_of) => {
                    Assignment::Borrow(Borrow {
                        address: reference_of
                            .address
                            .transform_model(transformer)?,
                        qualifier: reference_of.qualifier,
                        lifetime: transformer.inspect_and_transform(
                            reference_of.lifetime,
                            self.span.as_ref(),
                        )?,
                    })
                }
                Assignment::Prefix(prefix) => Assignment::Prefix(Prefix {
                    operand: prefix.operand.transform_model(transformer)?,
                    operator: prefix.operator,
                }),
                Assignment::Struct(st) => Assignment::Struct(Struct {
                    struct_id: st.struct_id,
                    initializers_by_field_id: st
                        .initializers_by_field_id
                        .into_iter()
                        .map(|(k, v)| Ok((k, v.transform_model(transformer)?)))
                        .collect::<Result<_, E>>()?,
                    generic_arguments: {
                        transformer
                            .inspect_and_transform(
                                Type::Symbol(Symbol {
                                    id: st.struct_id,
                                    generic_arguments: st.generic_arguments,
                                }),
                                self.span.as_ref(),
                            )?
                            .into_symbol()
                            .unwrap()
                            .generic_arguments
                    },
                }),
                Assignment::Variant(variant) => Assignment::Variant(Variant {
                    variant_id: variant.variant_id,
                    associated_value: variant
                        .associated_value
                        .map(|x| x.transform_model(transformer))
                        .transpose()?,
                    generic_arguments: {
                        let enum_id = GlobalID::new(
                            variant.variant_id.target_id,
                            table
                                .get::<Parent>(variant.variant_id)
                                .parent
                                .unwrap(),
                        );

                        transformer
                            .inspect_and_transform(
                                Type::Symbol(Symbol {
                                    id: enum_id,
                                    generic_arguments: variant
                                        .generic_arguments,
                                }),
                                self.span.as_ref(),
                            )?
                            .into_symbol()
                            .unwrap()
                            .generic_arguments
                    },
                }),

                // this is such a bad idea :(
                Assignment::FunctionCall(function_call) => {
                    let symbol_kind =
                        *table.get::<SymbolKind>(function_call.callable_id);

                    let instantiation = match symbol_kind {
                        SymbolKind::ExternFunction | SymbolKind::Function => {
                            let generic_parameters = table
                                .query::<GenericParameters>(
                                    function_call.callable_id,
                                )?;

                            let generic_arguments = function_call
                                .instantiation
                                .create_generic_arguments(
                                    function_call.callable_id,
                                    &generic_parameters,
                                )
                                .unwrap();

                            transformer
                                .inspect_and_transform(
                                    Type::Symbol(Symbol {
                                        id: function_call.callable_id,
                                        generic_arguments,
                                    }),
                                    self.span.as_ref(),
                                )?
                                .into_symbol()
                                .unwrap();

                            transform_instantiation(
                                transformer,
                                function_call.instantiation,
                                self.span.as_ref(),
                            )?
                        }
                        SymbolKind::TraitFunction => {
                            let parent_trait = GlobalID::new(
                                function_call.callable_id.target_id,
                                table
                                    .get::<Parent>(function_call.callable_id)
                                    .parent
                                    .unwrap(),
                            );

                            let trait_generic_params =
                                table
                                    .query::<GenericParameters>(parent_trait)?;

                            let trait_function_generic_params =
                                table.query::<GenericParameters>(
                                    function_call.callable_id,
                                )?;

                            let parent_generic_arguments = function_call
                                .instantiation
                                .create_generic_arguments(
                                    parent_trait,
                                    &trait_generic_params,
                                )
                                .unwrap();
                            let member_generic_arguments = function_call
                                .instantiation
                                .create_generic_arguments(
                                    function_call.callable_id,
                                    &trait_function_generic_params,
                                )
                                .unwrap();

                            transformer.inspect(
                                &Type::MemberSymbol(MemberSymbol {
                                    id: function_call.callable_id,
                                    member_generic_arguments,
                                    parent_generic_arguments,
                                }),
                                self.span.as_ref(),
                            )?;

                            transform_instantiation(
                                transformer,
                                function_call.instantiation,
                                self.span.as_ref(),
                            )?
                        }
                        SymbolKind::TraitImplementationFunction
                        | SymbolKind::AdtImplementationFunction => {
                            let parent_implementation_id = GlobalID::new(
                                function_call.callable_id.target_id,
                                table
                                    .get::<Parent>(function_call.callable_id)
                                    .parent
                                    .unwrap(),
                            );
                            let implementation = table
                                .query::<Implementation>(
                                    parent_implementation_id,
                                )?;

                            let mut parent_generic_arguments =
                                GenericArguments::from_other_model(
                                    implementation.generic_arguments.clone(),
                                );

                            parent_generic_arguments
                                .instantiate(&function_call.instantiation);

                            let function_generic_params = table
                                .query::<GenericParameters>(
                                function_call.callable_id,
                            )?;

                            let member_generic_arguments = function_call
                                .instantiation
                                .create_generic_arguments(
                                    function_call.callable_id,
                                    &function_generic_params,
                                )
                                .unwrap();

                            transformer.inspect(
                                &Type::MemberSymbol(MemberSymbol {
                                    id: function_call.callable_id,
                                    member_generic_arguments,
                                    parent_generic_arguments,
                                }),
                                self.span.as_ref(),
                            )?;

                            transform_instantiation(
                                transformer,
                                function_call.instantiation,
                                self.span.as_ref(),
                            )?
                        }

                        _ => unreachable!(
                            "invalid callable id {:?}",
                            function_call.callable_id
                        ),
                    };

                    Assignment::FunctionCall(FunctionCall {
                        callable_id: function_call.callable_id,
                        arguments: function_call
                            .arguments
                            .into_iter()
                            .map(|x| x.transform_model(transformer))
                            .collect::<Result<_, E>>()?,
                        instantiation,
                    })
                }
                Assignment::Binary(binary) => Assignment::Binary(Binary {
                    lhs: binary.lhs.transform_model(transformer)?,
                    rhs: binary.rhs.transform_model(transformer)?,
                    operator: binary.operator,
                }),
                Assignment::Array(array) => Assignment::Array(Array {
                    elements: array
                        .elements
                        .into_iter()
                        .map(|x| x.transform_model(transformer))
                        .collect::<Result<_, E>>()?,
                    element_type: transformer.inspect_and_transform(
                        array.element_type,
                        self.span.as_ref(),
                    )?,
                }),
                Assignment::Phi(phi) => Assignment::Phi(Phi {
                    incoming_values: phi
                        .incoming_values
                        .into_iter()
                        .map(|(k, v)| {
                            Ok((
                                ID::from_index(k.into_index()),
                                v.transform_model(transformer)?,
                            ))
                        })
                        .collect::<Result<_, E>>()?,
                    r#type: transformer.inspect_and_transform(
                        phi.r#type,
                        self.span.as_ref(),
                    )?,
                }),
                Assignment::Cast(cast) => Assignment::Cast(Cast {
                    value: cast.value.transform_model(transformer)?,
                    r#type: transformer.inspect_and_transform(
                        cast.r#type,
                        self.span.as_ref(),
                    )?,
                }),
                Assignment::VariantNumber(variant_number) => {
                    Assignment::VariantNumber(VariantNumber {
                        address: variant_number
                            .address
                            .transform_model(transformer)?,
                        enum_id: variant_number.enum_id,
                    })
                }
            },
        })
    }
}
