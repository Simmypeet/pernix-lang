//! Contains the definition of [`Register`] and [`Assignment`].
//!
//! The register is a place where SSA values are stored. The assignment is the
//! value that is stored in the register.

use std::collections::{BTreeSet, HashMap};

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use super::Value;
use crate::{
    arena::{Key, ID},
    ir::{
        address::Address, control_flow_graph::Block, representation::Values,
        Transform, TypeOfError,
    },
    symbol::{
        self,
        table::{self, representation::Index, Table},
        AdtID, CallableID, Field, GlobalID, ImplementationID,
    },
    type_system::{
        environment::Environment,
        instantiation::{self, Instantiation},
        model::Model,
        normalizer::Normalizer,
        observer::Observer,
        simplify,
        term::{
            self,
            constant::{self, Constant},
            lifetime::Lifetime,
            r#type::{self, Qualifier, Type},
            GenericArguments, Local, MemberSymbol, Symbol,
        },
        Succeeded,
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

impl<M: Model> Values<M> {
    fn type_of_tuple_assignment<S: table::State>(
        &self,
        tuple: &Tuple<M>,
        current_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Succeeded<Type<M>, M>, TypeOfError<M>> {
        let mut constraints = BTreeSet::new();
        let mut elements = Vec::new();

        for element in &tuple.elements {
            let Succeeded { result: ty, constraints: new_constraint } =
                self.type_of_value(&element.value, current_site, environment)?;

            constraints.extend(new_constraint);

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

        Ok(Succeeded::with_constraints(
            Type::Tuple(term::Tuple { elements }),
            constraints,
        ))
    }
}

/// Represents a load/read from an address in memory. (The type must be Copy)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Load<M: Model> {
    /// The address where the value is stored and will be read from.
    pub address: Address<M>,
}

impl<M: Model> Values<M> {
    fn type_of_load_assignment<S: table::State>(
        &self,
        load: &Load<M>,
        current_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Succeeded<Type<M>, M>, TypeOfError<M>> {
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

    /// The lifetime introduces by the reference of operation.
    pub lifetime: Lifetime<M>,
}

impl<M: Model> Values<M> {
    fn type_of_reference_of_assignment<S: table::State>(
        &self,
        reference_of: &ReferenceOf<M>,
        current_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Succeeded<Type<M>, M>, TypeOfError<M>> {
        self.type_of_address(&reference_of.address, current_site, environment)
            .map(|x| {
                x.map(|x| {
                    Type::Reference(r#type::Reference {
                        qualifier: reference_of.qualifier,
                        lifetime: reference_of.lifetime.clone(),
                        pointee: Box::new(x),
                    })
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

impl<M: Model> Values<M> {
    fn type_of_prefix_assignment<S: table::State>(
        &self,
        prefix: &Prefix<M>,
        current_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Succeeded<Type<M>, M>, TypeOfError<M>> {
        let mut operand_type =
            self.type_of_value(&prefix.operand, current_site, environment)?;

        match prefix.operator {
            PrefixOperator::Negate
            | PrefixOperator::LogicalNot
            | PrefixOperator::BitwiseNot => Ok(operand_type),

            PrefixOperator::Local => {
                operand_type.result =
                    Type::Local(Local(Box::new(operand_type.result)));

                Ok(operand_type)
            }

            PrefixOperator::Unlocal => operand_type.try_map(|x| match x {
                Type::Local(x) => Ok(*x.0),
                _ => Err(TypeOfError::NonLocalAssignmentType {
                    value: prefix.operand.clone(),
                    r#type: x,
                }),
            }),
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
        id: r#type::SymbolID::Adt(AdtID::Struct(st.struct_id)),
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
        id: r#type::SymbolID::Adt(AdtID::Enum(enum_id)),
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

impl<M: Model> Values<M> {
    fn type_of_binary<S: table::State>(
        &self,
        binary: &Binary<M>,
        current_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Succeeded<Type<M>, M>, TypeOfError<M>> {
        // the return type always based on the lhs field
        if let BinaryOperator::Relational(_) = binary.operator {
            Ok(Succeeded::new(Type::Primitive(r#type::Primitive::Bool)))
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

/// Returns the variant number of the given address to the enum.
///
/// The variant number is supposed to be a unique identifier specifying which
/// variant is active in the enum. The number should correspond to the
/// declration order of it in the enum.
///
/// This is primarily used in the pattern matching.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariantNumber<M: Model> {
    /// Address to the num to get the variant number of.
    pub address: Address<M>,
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
    VariantNumber(VariantNumber<M>),
}

/// Represents a register in the SSA from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Register<M: Model> {
    /// The value stored in the register.
    pub assignment: Assignment<M>,

    /// The span where the value was defined.
    pub span: Option<Span>,
}

impl<M: Model> Values<M> {
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
    pub fn type_of_register<S: table::State>(
        &self,
        id: ID<Register<M>>,
        current_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Succeeded<Type<M>, M>, TypeOfError<M>> {
        let register = self
            .registers()
            .get(id)
            .ok_or(TypeOfError::InvalidRegisterID(id))?;

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
            Assignment::ReferenceOf(reference_of) => {
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
                type_of_variant_assignment(variant, environment.table())
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
            Assignment::Array(array) => Ok(Type::Array(r#type::Array {
                r#type: Box::new(array.element_type.clone()),
                length: Constant::Primitive(constant::Primitive::Integer(
                    array.elements.len() as i128,
                )),
            })),
            Assignment::Cast(cast) => Ok(cast.r#type.clone()),
            Assignment::VariantNumber(_) => {
                Ok(Type::Primitive(r#type::Primitive::Usize))
            }
        }?;

        Ok(simplify::simplify(&ty, environment))
    }
}

fn transform_instantiation<
    FM: Model,
    TM: Model,
    T: Transform<Lifetime<FM>, Target = TM>
        + Transform<Type<FM>, Target = TM>
        + Transform<Constant<FM>, Target = TM>,
>(
    transformer: &mut T,
    instantiation: Instantiation<FM>,
) -> Instantiation<TM> {
    Instantiation {
        lifetimes: instantiation
            .lifetimes
            .into_iter()
            .map(|(k, v)| (transformer.transform(k), transformer.transform(v)))
            .collect(),
        types: instantiation
            .types
            .into_iter()
            .map(|(k, v)| (transformer.transform(k), transformer.transform(v)))
            .collect(),
        constants: {
            instantiation
                .constants
                .into_iter()
                .map(|(k, v)| {
                    (transformer.transform(k), transformer.transform(v))
                })
                .collect()
        },
    }
}

impl<M: Model> Register<M> {
    /// Transforms the [`Register`] to another model using the given
    /// transformer.
    #[allow(clippy::too_many_lines)]
    pub fn transform_model<
        U: Model,
        T: Transform<Lifetime<M>, Target = U>
            + Transform<Type<M>, Target = U>
            + Transform<Constant<M>, Target = U>,
    >(
        self,
        transformer: &mut T,
        table: &Table<impl table::State>,
    ) -> Register<U> {
        Register {
            span: self.span.clone(),
            assignment: match self.assignment {
                Assignment::Tuple(tuple) => Assignment::Tuple(Tuple {
                    elements: tuple
                        .elements
                        .into_iter()
                        .map(|x| TupleElement {
                            value: x.value.transform_model(transformer),
                            is_unpacked: x.is_unpacked,
                        })
                        .collect(),
                }),
                Assignment::Load(load) => Assignment::Load(Load {
                    address: load.address.transform_model(transformer),
                }),
                Assignment::ReferenceOf(reference_of) => {
                    Assignment::ReferenceOf(ReferenceOf {
                        address: reference_of
                            .address
                            .transform_model(transformer),
                        qualifier: reference_of.qualifier,
                        lifetime: transformer.inspect_and_transform(
                            reference_of.lifetime,
                            self.span,
                        ),
                    })
                }
                Assignment::Prefix(prefix) => Assignment::Prefix(Prefix {
                    operand: prefix.operand.transform_model(transformer),
                    operator: prefix.operator,
                }),
                Assignment::Struct(st) => Assignment::Struct(Struct {
                    struct_id: st.struct_id,
                    initializers_by_field_id: st
                        .initializers_by_field_id
                        .into_iter()
                        .map(|(k, v)| (k, v.transform_model(transformer)))
                        .collect(),
                    generic_arguments: {
                        transformer
                            .inspect_and_transform(
                                Type::Symbol(Symbol {
                                    id: r#type::SymbolID::Adt(AdtID::Struct(
                                        st.struct_id,
                                    )),
                                    generic_arguments: st.generic_arguments,
                                }),
                                self.span,
                            )
                            .into_symbol()
                            .unwrap()
                            .generic_arguments
                    },
                }),
                Assignment::Variant(variant) => Assignment::Variant(Variant {
                    variant_id: variant.variant_id,
                    associated_value: variant
                        .associated_value
                        .map(|x| x.transform_model(transformer)),
                    generic_arguments: {
                        let enum_id = table
                            .get(variant.variant_id)
                            .unwrap()
                            .parent_enum_id();

                        transformer
                            .inspect_and_transform(
                                Type::Symbol(Symbol {
                                    id: r#type::SymbolID::Adt(AdtID::Enum(
                                        enum_id,
                                    )),
                                    generic_arguments: variant
                                        .generic_arguments,
                                }),
                                self.span,
                            )
                            .into_symbol()
                            .unwrap()
                            .generic_arguments
                    },
                }),

                // this is such a bad idea :(
                Assignment::FunctionCall(function_call) => {
                    Assignment::FunctionCall(FunctionCall {
                        callable_id: function_call.callable_id,
                        arguments: function_call
                            .arguments
                            .into_iter()
                            .map(|x| x.transform_model(transformer))
                            .collect(),
                        instantiation: match function_call.callable_id {
                            CallableID::Function(id) => {
                                let function_symbol = table.get(id).unwrap();

                                let generic_arguments = function_call
                                    .instantiation
                                    .create_generic_arguments(
                                        id.into(),
                                        &function_symbol
                                            .generic_declaration
                                            .parameters,
                                    )
                                    .unwrap();

                                transformer
                                    .inspect_and_transform(
                                        Type::Symbol(Symbol {
                                            id: r#type::SymbolID::Function(id),
                                            generic_arguments,
                                        }),
                                        self.span,
                                    )
                                    .into_symbol()
                                    .unwrap();

                                transform_instantiation(
                                    transformer,
                                    function_call.instantiation,
                                )
                            }

                            CallableID::TraitFunction(id) => {
                                let trait_funciton = table.get(id).unwrap();
                                let parent_trait = trait_funciton.parent_id();

                                let parent_generic_arguments = function_call
                                    .instantiation
                                    .create_generic_arguments(
                                        parent_trait.into(),
                                        &table
                                            .get(parent_trait)
                                            .unwrap()
                                            .generic_declaration
                                            .parameters,
                                    )
                                    .unwrap();
                                let member_generic_arguments = function_call
                                    .instantiation
                                    .create_generic_arguments(
                                        id.into(),
                                        &trait_funciton
                                            .generic_declaration
                                            .parameters,
                                    )
                                    .unwrap();

                                transformer.inspect_and_transform(
                                    Type::MemberSymbol(MemberSymbol {
                                        id: r#type::MemberSymbolID::Function(
                                            id.into(),
                                        ),
                                        member_generic_arguments,
                                        parent_generic_arguments,
                                    }),
                                    self.span,
                                );

                                transform_instantiation(
                                    transformer,
                                    function_call.instantiation,
                                )
                            }

                            CallableID::TraitImplementationFunction(_)
                            | CallableID::AdtImplementationFunction(_) => {
                                let parent_implementation_id =
                                    match function_call.callable_id {
                                        CallableID::TraitImplementationFunction(id) => {
                                            ImplementationID::PositiveTrait(
                                                table.get(id).unwrap().parent_id(),
                                            )
                                        }
                                        CallableID::AdtImplementationFunction(id) => {
                                            ImplementationID::Adt(
                                                table.get(id).unwrap().parent_id(),
                                            )
                                        }
                                        _ => unreachable!()
                                    };

                                let mut parent_generic_arguments =
                                    GenericArguments::from_other_model(
                                        table
                                            .get_implementation(
                                                parent_implementation_id,
                                            )
                                            .unwrap()
                                            .arguments()
                                            .clone(),
                                    );

                                parent_generic_arguments
                                    .instantiate(&function_call.instantiation);

                                let member_generic_arguments = function_call
                                    .instantiation
                                    .create_generic_arguments(
                                        function_call.callable_id.into(),
                                        &table
                                            .get_generic(
                                                function_call
                                                    .callable_id
                                                    .into(),
                                            )
                                            .unwrap()
                                            .generic_declaration()
                                            .parameters,
                                    )
                                    .unwrap();

                                transformer.inspect_and_transform(
                                    Type::MemberSymbol(MemberSymbol {
                                        id: r#type::MemberSymbolID::Function(
                                            match function_call.callable_id {
                                                CallableID::TraitImplementationFunction(id) => id.into(),
                                                CallableID::AdtImplementationFunction(id) => id.into(),
                                                _ => unreachable!()
                                            }
                                        ),
                                        member_generic_arguments,
                                        parent_generic_arguments,
                                    }),
                                    self.span,
                                ).into_member_symbol().unwrap();

                                transform_instantiation(
                                    transformer,
                                    function_call.instantiation,
                                )
                            }
                        },
                    })
                }
                Assignment::Binary(binary) => Assignment::Binary(Binary {
                    lhs: binary.lhs.transform_model(transformer),
                    rhs: binary.rhs.transform_model(transformer),
                    operator: binary.operator,
                }),
                Assignment::Array(array) => Assignment::Array(Array {
                    elements: array
                        .elements
                        .into_iter()
                        .map(|x| x.transform_model(transformer))
                        .collect(),
                    element_type: transformer
                        .inspect_and_transform(array.element_type, self.span),
                }),
                Assignment::Phi(phi) => Assignment::Phi(Phi {
                    incoming_values: phi
                        .incoming_values
                        .into_iter()
                        .map(|(k, v)| {
                            (
                                ID::from_index(k.into_index()),
                                v.transform_model(transformer),
                            )
                        })
                        .collect(),
                    r#type: transformer
                        .inspect_and_transform(phi.r#type, self.span),
                }),
                Assignment::Cast(cast) => Assignment::Cast(Cast {
                    value: cast.value.transform_model(transformer),
                    r#type: transformer
                        .inspect_and_transform(cast.r#type, self.span),
                }),
                Assignment::VariantNumber(variant_number) => {
                    Assignment::VariantNumber(VariantNumber {
                        address: variant_number
                            .address
                            .transform_model(transformer),
                    })
                }
            },
        }
    }
}
