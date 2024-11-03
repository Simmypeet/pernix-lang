//! Contains the definition of [`Address`] and its variants.

use enum_as_inner::EnumAsInner;

use super::{
    alloca::Alloca, representation::Representation, value::Value, TypeOfError,
};
use crate::{
    arena::ID,
    ir::address,
    symbol::{
        self,
        table::{self, representation::Index as _},
        AdtID, CallableID, GlobalID, Parameter,
    },
    type_system::{
        environment::Environment,
        instantiation::{self, Instantiation},
        model::Model,
        normalizer::Normalizer,
        observer::Observer,
        simplify,
        term::{self, r#type::Type, Symbol},
        Succeeded,
    },
};

/// The address points to a field in a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field<M: Model> {
    /// The address to the struct.
    pub struct_address: Box<Address<M>>,

    /// The field that the address points to.
    pub id: ID<symbol::Field>,
}

/// The address points to an element in an array.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Index<M: Model> {
    /// The address to the array.
    pub array_address: Box<Address<M>>,

    /// The index to access.
    pub indexing_value: Value<M>,
}

/// The offset from the start or end of a tuple.
///
/// Primarily used for indexing into a tuple element with an offset.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Offset {
    /// The offset is from the start of the tuple (0-indexed).
    FromStart(usize),

    /// The offset is from the end of the tuple (0-indexed).
    FromEnd(usize),
}

/// Interprets the enum addresss as an associated value of a particular variant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant<M: Model> {
    /// The address to the variant.
    pub enum_address: Box<Address<M>>,

    /// The variant of to interpret the enum address as.
    pub variant_id: ID<symbol::Variant>,
}

/// The address points to an element in a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<M: Model> {
    /// The address to the tuple.
    pub tuple_address: Box<Address<M>>,

    /// The offset of the element to access.
    pub offset: Offset,
}

/// The memory pointer is stored in an address.
///
/// # Example
///
/// ```pnx
/// let x = 32;
/// let address = &unique x;
/// *address = 5;
/// ```
///
/// The `*address` is represented by `ReferenceAddress { address:
/// Memory::Alloca(&address) }`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::module_name_repetitions)]
pub struct ReferenceAddress<M: Model> {
    /// The address where the memory pointer is stored.
    pub reference_address: Box<Address<M>>,
}

/// Represents a real memory location.
///
/// This is used to represent the base address of a memory location for the
/// [`Address`] type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Memory<M: Model> {
    Parameter(ID<Parameter>),
    Alloca(ID<Alloca<M>>),

    /// The memory pointer is stored in a register.
    ///
    /// # Example
    ///
    /// ```pnx
    /// *getReference() = 5;
    /// ```
    ///
    /// The `*getReference()` is represented by `Memory::ReferenceValue(
    /// "getReference()" )` where `getReference()` is a function that returns a
    /// reference.
    ReferenceValue(Value<M>),
}

/// Represents an address to a particular location in memory.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Address<M: Model> {
    Memory(Memory<M>),

    Field(Field<M>),
    Tuple(Tuple<M>),
    Index(Index<M>),
    Variant(Variant<M>),
    ReferenceAddress(ReferenceAddress<M>),
}

impl<M: Model> Address<M> {
    /// Checks if the address has a root that is a reference.
    ///
    /// The root is either from [`Memory::ReferenceValue`] or
    /// [`Self::ReferenceAddress`].
    pub fn is_behind_reference(&self) -> bool {
        match self {
            Self::Memory(Memory::Alloca(_) | Memory::Parameter(_)) => false,

            Self::Field(field) => field.struct_address.is_behind_reference(),
            Self::Tuple(tuple) => tuple.tuple_address.is_behind_reference(),
            Self::Index(index) => index.array_address.is_behind_reference(),
            Self::Variant(variant) => {
                variant.enum_address.is_behind_reference()
            }

            Self::Memory(Memory::ReferenceValue(_))
            | Self::ReferenceAddress(_) => true,
        }
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "the `Field` address requires `struct_address` field to have an adress of \
     type some struct but found the other type"
)]
#[allow(missing_docs)]
pub struct InvalidStructAddressError<M: Model>(Type<M>);

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
    #[allow(clippy::too_many_lines)]
    pub fn type_of_address<S: table::State>(
        &self,
        address: &Address<M>,
        current_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Succeeded<Type<M>, M>, TypeOfError<M>> {
        match address {
            Address::Memory(Memory::Parameter(parameter)) => {
                let callable_id =
                    CallableID::try_from(current_site).ok().ok_or(
                        TypeOfError::CurrentSiteIsNotFunction(current_site),
                    )?;

                let callable = environment
                    .table()
                    .get_callable(callable_id)
                    .ok_or(TypeOfError::InvalidGlobalID(callable_id.into()))?;

                let ty = callable
                    .parameters()
                    .get(*parameter)
                    .ok_or(TypeOfError::InvalidParameterID {
                        parameter_id: *parameter,
                        in_function: callable_id,
                    })?
                    .r#type
                    .clone();

                Ok(simplify::simplify(&M::from_default_type(ty), environment))
            }

            Address::Memory(Memory::Alloca(parameter)) => {
                let alloca = self
                    .allocas()
                    .get(*parameter)
                    .ok_or(TypeOfError::InvalidAllocaID(*parameter))?;

                Ok(simplify::simplify(&alloca.r#type.clone(), environment))
            }

            Address::Memory(Memory::ReferenceValue(value)) => self
                .type_of_value(value, current_site, environment)?
                .try_map(|x| match x {
                    Type::Reference(x) => Ok(*x.pointee),
                    found => Err(TypeOfError::NonReferenceValueType {
                        value: value.clone(),
                        r#type: found,
                    }),
                }),

            Address::Field(field_address) => {
                // the type of address should've been simplified
                let Succeeded { result, mut constraints } = self
                    .type_of_address(
                        &field_address.struct_address,
                        current_site,
                        environment,
                    )?;

                let (struct_id, generic_arguments) = match result {
                    Type::Symbol(Symbol {
                        id: AdtID::Struct(struct_id),
                        generic_arguments,
                    }) => (struct_id, generic_arguments),

                    another_ty => {
                        return Err(TypeOfError::NonStructAddressType {
                            address: (*field_address.struct_address).clone(),
                            r#type: another_ty,
                        });
                    }
                };

                let struct_sym = environment
                    .table()
                    .get(struct_id)
                    .ok_or(TypeOfError::InvalidGlobalID(struct_id.into()))?;

                let instantiation =
                    match Instantiation::from_generic_arguments(
                        generic_arguments,
                        struct_id.into(),
                        &struct_sym.generic_declaration.parameters,
                    ) {
                        Ok(instantiation) => instantiation,

                        Err(error) => return Err(
                            TypeOfError::InvalidStructAddressInstantiation {
                                struct_id,
                                mismatched_generic_argument_error: error,
                            },
                        ),
                    };

                let mut field_ty = M::from_default_type(
                    struct_sym
                        .fields()
                        .get(field_address.id)
                        .ok_or(TypeOfError::InvalidFieldID {
                            field_id: field_address.id,
                            in_struct: struct_id,
                        })?
                        .r#type
                        .clone(),
                );

                instantiation::instantiate(&mut field_ty, &instantiation);
                let simplification = simplify::simplify(&field_ty, environment);
                constraints.extend(simplification.constraints);

                Ok(Succeeded { result: simplification.result, constraints })
            }

            Address::Tuple(tuple) => {
                self.type_of_address(
                    &tuple.tuple_address,
                    current_site,
                    environment,
                )?
                .try_map(|x| {
                    // extract tuple type
                    let mut tuple_ty = match x {
                        Type::Tuple(tuple_ty) => tuple_ty,
                        another_ty => {
                            return Err(TypeOfError::NonTupleAddressType {
                                address: (*tuple.tuple_address).clone(),
                                r#type: another_ty,
                            })
                        }
                    };

                    match match tuple.offset {
                        address::Offset::FromStart(id) => Some(id),
                        address::Offset::FromEnd(id) => tuple_ty
                            .elements
                            .len()
                            .checked_sub(1)
                            .and_then(|x| x.checked_sub(id)),
                    } {
                        Some(id) if id < tuple_ty.elements.len() => {
                            let element_ty = tuple_ty.elements.remove(id);

                            Ok(if element_ty.is_unpacked {
                                Type::Tuple(term::Tuple {
                                    elements: vec![term::TupleElement {
                                        term: element_ty.term,
                                        is_unpacked: true,
                                    }],
                                })
                            } else {
                                element_ty.term
                            })
                        }

                        _ => Err(TypeOfError::InvalidTupleOffset {
                            offset: tuple.offset,
                            tuple_type: tuple_ty,
                        }),
                    }
                })
            }

            Address::Index(index) => self
                .type_of_address(
                    &index.array_address,
                    current_site,
                    environment,
                )?
                .try_map(|x| {
                    let element_ty = match x {
                        Type::Array(array_ty) => *array_ty.r#type,
                        another_ty => {
                            return Err(TypeOfError::NonArrayAddressType {
                                address: (*index.array_address).clone(),
                                r#type: another_ty,
                            });
                        }
                    };

                    Ok(element_ty)
                }),

            Address::Variant(variant) => {
                let Succeeded { result, mut constraints } = self
                    .type_of_address(
                        &variant.enum_address,
                        current_site,
                        environment,
                    )?;

                let (enum_id, generic_arguments) = match result {
                    Type::Symbol(Symbol {
                        id: AdtID::Enum(enum_id),
                        generic_arguments,
                    }) => (enum_id, generic_arguments),

                    another_ty => {
                        return Err(TypeOfError::NonEnumAddressType {
                            address: (*variant.enum_address).clone(),
                            r#type: another_ty,
                        });
                    }
                };

                let variant_sym =
                    environment.table().get(variant.variant_id).ok_or(
                        TypeOfError::InvalidGlobalID(variant.variant_id.into()),
                    )?;

                // mismatched enum id
                if variant_sym.parent_enum_id() != enum_id {
                    return Err(TypeOfError::InvalidVariantID {
                        variant_id: variant.variant_id,
                        enum_id,
                    });
                }

                let enum_sym = environment
                    .table()
                    .get(enum_id)
                    .ok_or(TypeOfError::InvalidGlobalID(enum_id.into()))?;

                let instantiation = match Instantiation::from_generic_arguments(
                    generic_arguments,
                    enum_id.into(),
                    &enum_sym.generic_declaration.parameters,
                ) {
                    Ok(instantiation) => instantiation,

                    Err(error) => {
                        return Err(
                            TypeOfError::InvalidEnumAddressInstantiation {
                                enum_id,
                                mismatched_generic_argument_error: error,
                            },
                        )
                    }
                };

                let mut variant_ty = M::from_default_type(
                    variant_sym.associated_type.clone().ok_or(
                        TypeOfError::VariantHasNoAssociatedValue {
                            variant_id: variant.variant_id,
                        },
                    )?,
                );

                instantiation::instantiate(&mut variant_ty, &instantiation);

                let simplification =
                    simplify::simplify(&variant_ty, environment);
                constraints.extend(simplification.constraints);

                Ok(Succeeded::with_constraints(
                    simplification.result,
                    constraints,
                ))
            }

            Address::ReferenceAddress(value) => self
                .type_of_address(
                    &value.reference_address,
                    current_site,
                    environment,
                )?
                .try_map(|x| match x {
                    Type::Reference(value) => Ok(*value.pointee),
                    _ => Err(TypeOfError::NonReferenceAddressType {
                        address: (*value.reference_address).clone(),
                        r#type: x,
                    }),
                }),
        }
    }
}
