//! Contains the definition of [`Address`] and its variants.

use enum_as_inner::EnumAsInner;

use super::{
    alloca::Alloca, representation::Values, value::Value, Transform,
    TypeOfError,
};
use crate::{
    arena::{Key, ID},
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
        term::{
            self,
            r#type::{self, Qualifier, Type},
            Symbol,
        },
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

    /// Points to the non-definite unpacked tuple element.
    Unpacked,
}

/// Interprets the enum addresss as an associated value of a particular variant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant<M: Model> {
    /// The address to the variant.
    pub enum_address: Box<Address<M>>,

    /// The variant of to interpret the enum address as.
    pub id: ID<symbol::Variant>,
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
pub struct Reference<M: Model> {
    /// The reference qualifier of the memory pointer.
    pub qualifier: Qualifier,

    /// The address where the memory pointer is stored.
    pub reference_address: Box<Address<M>>,
}

/// Represents a real memory location.
///
/// This is used to represent the base address of a memory location for the
/// [`Address`] type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Memory<M: Model> {
    Parameter(ID<Parameter>),
    Alloca(ID<Alloca<M>>),
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
    Reference(Reference<M>),
}

impl<M: Model> Address<M> {
    /// Checks if the `self` address is the child of the `parent` address.
    pub fn is_child_of(mut self: &Self, parent: &Self) -> bool {
        loop {
            if self == parent {
                return true;
            }

            match self {
                Self::Memory(_) => return false,

                Self::Field(field) => self = &*field.struct_address,
                Self::Tuple(tuple) => self = &*tuple.tuple_address,
                Self::Index(index) => self = &*index.array_address,
                Self::Variant(variant) => self = &*variant.enum_address,
                Self::Reference(reference) => {
                    self = &*reference.reference_address;
                }
            }
        }
    }

    /// Checks if the address has a root that is a reference.
    ///
    /// This checks if the address contains [`Address::Reference`].
    pub const fn is_behind_reference(mut self: &Self) -> bool {
        loop {
            match self {
                Self::Memory(_) => return false,

                Self::Reference(_) => return true,

                Self::Field(field) => self = &*field.struct_address,
                Self::Tuple(tuple) => self = &*tuple.tuple_address,
                Self::Index(index) => self = &*index.array_address,
                Self::Variant(variant) => self = &*variant.enum_address,
            }
        }
    }

    /// Gets the root memory of the address.
    pub const fn get_root_memory(mut self: &Self) -> &Memory<M> {
        loop {
            match self {
                Self::Memory(memory) => return memory,
                Self::Field(field) => self = &*field.struct_address,
                Self::Tuple(tuple) => self = &*tuple.tuple_address,
                Self::Index(index) => self = &*index.array_address,
                Self::Variant(variant) => self = &*variant.enum_address,
                Self::Reference(reference) => {
                    self = &*reference.reference_address;
                }
            }
        }
    }

    /// Checks if the address is behind an index.
    ///
    /// This checks if the address contains [`Address::Index`].
    pub const fn is_behind_index(mut self: &Self) -> bool {
        loop {
            match self {
                Self::Memory(_) => return false,

                Self::Index(_) => return true,

                Self::Field(field) => self = &*field.struct_address,
                Self::Tuple(tuple) => self = &*tuple.tuple_address,
                Self::Variant(variant) => self = &*variant.enum_address,
                Self::Reference(reference) => {
                    self = &*reference.reference_address;
                }
            }
        }
    }

    /// Gets the reference qualifier of the address.
    pub fn get_reference_qualifier(&self) -> Option<Qualifier> {
        match self {
            Self::Memory(_) => None,
            Self::Field(field) => {
                field.struct_address.get_reference_qualifier()
            }
            Self::Tuple(tuple) => tuple.tuple_address.get_reference_qualifier(),
            Self::Index(index) => index.array_address.get_reference_qualifier(),
            Self::Variant(variant) => {
                variant.enum_address.get_reference_qualifier()
            }
            Self::Reference(reference) => {
                let parent_qualifier =
                    reference.reference_address.get_reference_qualifier();

                Some(
                    reference
                        .qualifier
                        .min(parent_qualifier.unwrap_or(Qualifier::Mutable)),
                )
            }
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

impl<M: Model> Address<M> {
    /// Transforms the [`Address`] to another model using the given transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Result<Address<T::Target>, T::Error> {
        Ok(match self {
            Self::Memory(memory) => match memory {
                Memory::Parameter(id) => Address::Memory(Memory::Parameter(id)),
                Memory::Alloca(id) => Address::Memory(Memory::Alloca(
                    ID::from_index(id.into_index()),
                )),
            },
            Self::Field(field) => Address::Field(Field {
                struct_address: Box::new(
                    field.struct_address.transform_model(transformer)?,
                ),
                id: field.id,
            }),
            Self::Tuple(tuple) => Address::Tuple(Tuple {
                tuple_address: Box::new(
                    tuple.tuple_address.transform_model(transformer)?,
                ),
                offset: tuple.offset,
            }),
            Self::Index(index) => Address::Index(Index {
                array_address: Box::new(
                    index.array_address.transform_model(transformer)?,
                ),
                indexing_value: index
                    .indexing_value
                    .transform_model(transformer)?,
            }),
            Self::Variant(variant) => Address::Variant(Variant {
                enum_address: Box::new(
                    variant.enum_address.transform_model(transformer)?,
                ),
                id: variant.id,
            }),
            Self::Reference(reference_address) => {
                Address::Reference(Reference {
                    qualifier: reference_address.qualifier,
                    reference_address: Box::new(
                        reference_address
                            .reference_address
                            .transform_model(transformer)?,
                    ),
                })
            }
        })
    }
}

impl<M: Model> Values<M> {
    /// Gets the type of the [`Address`] with the given ID.
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
    ) -> Result<Succeeded<Type<M>, M>, TypeOfError> {
        match address {
            Address::Memory(Memory::Parameter(parameter)) => {
                let callable_id = CallableID::try_from(current_site)
                    .ok()
                    .ok_or(TypeOfError::Discrepancy)?;

                let callable = environment
                    .table()
                    .get_callable(callable_id)
                    .ok_or(TypeOfError::Discrepancy)?;

                let ty = callable
                    .parameters()
                    .get(*parameter)
                    .ok_or(TypeOfError::Discrepancy)?
                    .r#type
                    .clone();

                Ok(simplify::simplify(&M::from_default_type(ty), environment)?)
            }

            Address::Memory(Memory::Alloca(parameter)) => {
                let alloca = self
                    .allocas()
                    .get(*parameter)
                    .ok_or(TypeOfError::Discrepancy)?;

                Ok(simplify::simplify(&alloca.r#type.clone(), environment)?)
            }

            Address::Field(field_address) => {
                // the type of address should've been simplified
                let Succeeded { result, mut constraints } = self
                    .type_of_address(
                        &field_address.struct_address,
                        current_site,
                        environment,
                    )?;
                let Type::Symbol(Symbol {
                    id: r#type::SymbolID::Adt(AdtID::Struct(struct_id)),
                    generic_arguments,
                }) = result
                else {
                    return Err(TypeOfError::Discrepancy);
                };

                let struct_sym = environment
                    .table()
                    .get(struct_id)
                    .ok_or(TypeOfError::Discrepancy)?;

                let Ok(instantiation) = Instantiation::from_generic_arguments(
                    generic_arguments,
                    struct_id.into(),
                    &struct_sym.generic_declaration.parameters,
                ) else {
                    return Err(TypeOfError::Discrepancy);
                };

                let mut field_ty = M::from_default_type(
                    struct_sym
                        .fields()
                        .get(field_address.id)
                        .ok_or(TypeOfError::Discrepancy)?
                        .r#type
                        .clone(),
                );

                instantiation::instantiate(&mut field_ty, &instantiation);
                let simplification =
                    simplify::simplify(&field_ty, environment)?;
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
                    let Type::Tuple(mut tuple_ty) = x else {
                        return Err(TypeOfError::Discrepancy);
                    };

                    match match tuple.offset {
                        address::Offset::FromStart(id) => Some(id),
                        address::Offset::FromEnd(id) => tuple_ty
                            .elements
                            .len()
                            .checked_sub(1)
                            .and_then(|x| x.checked_sub(id)),
                        address::Offset::Unpacked => {
                            let Some(unpacked_ty) = tuple_ty
                                .elements
                                .iter()
                                .find_map(|x| x.is_unpacked.then_some(&x.term))
                            else {
                                return Err(TypeOfError::Discrepancy);
                            };

                            return Ok(unpacked_ty.clone());
                        }
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

                        _ => Err(TypeOfError::Discrepancy),
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
                        _ => {
                            return Err(TypeOfError::Discrepancy);
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

                let Type::Symbol(Symbol {
                    id: r#type::SymbolID::Adt(AdtID::Enum(enum_id)),
                    generic_arguments,
                }) = result
                else {
                    return Err(TypeOfError::Discrepancy);
                };

                let variant_sym = environment
                    .table()
                    .get(variant.id)
                    .ok_or(TypeOfError::Discrepancy)?;

                // mismatched enum id
                if variant_sym.parent_enum_id() != enum_id {
                    return Err(TypeOfError::Discrepancy);
                }

                let enum_sym = environment
                    .table()
                    .get(enum_id)
                    .ok_or(TypeOfError::Discrepancy)?;

                let Ok(instantiation) = Instantiation::from_generic_arguments(
                    generic_arguments,
                    enum_id.into(),
                    &enum_sym.generic_declaration.parameters,
                ) else {
                    return Err(TypeOfError::Discrepancy);
                };

                let mut variant_ty = M::from_default_type(
                    variant_sym
                        .associated_type
                        .clone()
                        .ok_or(TypeOfError::Discrepancy)?,
                );

                instantiation::instantiate(&mut variant_ty, &instantiation);

                let simplification =
                    simplify::simplify(&variant_ty, environment)?;
                constraints.extend(simplification.constraints);

                Ok(Succeeded::with_constraints(
                    simplification.result,
                    constraints,
                ))
            }

            Address::Reference(value) => self
                .type_of_address(
                    &value.reference_address,
                    current_site,
                    environment,
                )?
                .try_map(|x| match x {
                    Type::Reference(value) => Ok(*value.pointee),
                    _ => Err(TypeOfError::Discrepancy),
                }),
        }
    }
}
