//! Contains the definition of [`Address`] and its variants.

use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_semantic_element::{
    fields::{self, get_fields},
    parameter::{Parameter, get_parameters},
    variant::get_variant_associated_type,
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::Symbol,
    generic_parameters::get_generic_parameters,
    instantiation::Instantiation,
    tuple,
    r#type::{self, Qualifier, Type},
};
use pernixc_type_system::{Error, Succeeded, normalizer::Normalizer};

use crate::{
    Values,
    alloca::Alloca,
    capture::Capture,
    closure_parameters::ClosureParameter,
    transform::Transformer,
    value::{Environment, TypeOf, Value},
};

/// The address points to a field in a struct.
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
pub struct Field {
    /// The address to the struct.
    pub struct_address: Box<Address>,

    /// The field that the address points to.
    pub id: ID<fields::Field>,
}

/// The address points to an element in an array.
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
pub struct Index {
    /// The address to the array.
    pub array_address: Box<Address>,

    /// The index to access.
    pub indexing_value: Value,
}

/// The offset from the start or end of a tuple.
///
/// Primarily used for indexing into a tuple element with an offset.
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
pub enum Offset {
    /// The offset is from the start of the tuple (0-indexed).
    FromStart(usize),

    /// The offset is from the end of the tuple (0-indexed).
    FromEnd(usize),

    /// Points to the non-definite unpacked tuple element.
    Unpacked,
}

impl Offset {
    /// Flips the offset between `FromStart` and `FromEnd` given the total
    /// length of the tuple.
    #[must_use]
    pub const fn flip(&self, total_len: usize) -> Self {
        match self {
            Self::FromStart(id) => Self::FromEnd(total_len - *id - 1),
            Self::FromEnd(id) => Self::FromStart(total_len - *id - 1),
            Self::Unpacked => Self::Unpacked,
        }
    }

    /// Gets the tuple element at the given offset.
    #[must_use]
    pub fn index<'s, T>(
        &self,
        tuple: &'s pernixc_term::tuple::Tuple<T>,
    ) -> Option<&'s pernixc_term::tuple::Element<T>> {
        match self {
            Self::FromStart(id) => tuple.elements.get(*id),
            Self::FromEnd(id) => tuple
                .elements
                .len()
                .checked_sub(1)
                .and_then(|x| x.checked_sub(*id))
                .and_then(|id| tuple.elements.get(id)),
            Self::Unpacked => tuple.elements.iter().find(|x| x.is_unpacked),
        }
    }
}

/// Interprets the enum addresss as an associated value of a particular variant.
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
    /// The address to the variant.
    pub enum_address: Box<Address>,

    /// The variant of to interpret the enum address as.
    pub id: Global<pernixc_symbol::ID>,
}

/// The address points to an element in a tuple.
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
    /// The address to the tuple.
    pub tuple_address: Box<Address>,

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
pub struct Reference {
    /// The reference qualifier of the memory pointer.
    pub qualifier: Qualifier,

    /// The address where the memory pointer is stored.
    pub reference_address: Box<Address>,
}

/// Represents a real memory location.
///
/// This is used to represent the base address of a memory location for the
/// [`Address`] type.
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
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Memory {
    Parameter(ID<Parameter>),
    Alloca(ID<Alloca>),
    ClosureParameter(ID<ClosureParameter>),

    /// A captured variable from the parent closure/function.
    Capture(ID<Capture>),
}

impl Memory {
    /// Returns the drop priority of the memory location.
    ///
    /// 0 being the highest priority, dropping the memory location first.
    #[must_use]
    pub const fn drop_priority(&self) -> usize {
        match self {
            Self::Capture(_) => 0,
            Self::ClosureParameter(_) => 1,
            Self::Parameter(_) => 2,
            Self::Alloca(_) => 3,
        }
    }
}

/// Represents an address to a particular location in memory.
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
    EnumAsInner,
)]
#[allow(missing_docs, clippy::large_enum_variant)]
pub enum Address {
    Memory(Memory),

    Field(Field),
    Tuple(Tuple),
    Index(Index),
    Variant(Variant),
    Reference(Reference),
}

impl Address {
    /// Gets the number of dereference operations found in the address.
    #[must_use]
    pub const fn get_dereference_count(mut self: &Self) -> usize {
        let mut count = 0;

        loop {
            match self {
                Self::Memory(_) => return count,
                Self::Field(field) => self = &*field.struct_address,
                Self::Tuple(tuple) => self = &*tuple.tuple_address,
                Self::Index(index) => self = &*index.array_address,
                Self::Variant(variant) => self = &*variant.enum_address,
                Self::Reference(reference) => {
                    self = &*reference.reference_address;
                    count += 1;
                }
            }
        }
    }

    /// Checks if the `self` address is the child of the `parent` address.
    #[must_use]
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
    #[must_use]
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
    #[must_use]
    pub const fn get_root_memory(mut self: &Self) -> &Memory {
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
    #[must_use]
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
    #[must_use]
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

    /// Replaces the sub-address of `self` that matches `cond` with `new`.
    ///
    /// Suppose `self` is `a.b.c.d`, `cond` is `a.b.c`, and `new` is `x.y`, then
    /// after calling this function, `self` will be `x.y.d`.
    ///
    /// Suppose `self` is `a.b.c.d`, `cond` is `b.c`, and `new` is `x.y`,
    /// then after calling this function, `self` will remain `a.b.c.d` since
    /// `cond` has to match from the root of `self`.
    ///
    /// Returns `true` if a replacement was made, `false` otherwise.
    pub fn replace_with(mut self: &mut Self, cond: &Self, new: Self) -> bool {
        loop {
            if self == cond {
                *self = new;
                return true;
            }

            match self {
                Self::Memory(_) => return false,

                Self::Field(field) => self = field.struct_address.as_mut(),
                Self::Tuple(tuple) => self = tuple.tuple_address.as_mut(),
                Self::Index(index) => self = index.array_address.as_mut(),
                Self::Variant(variant) => self = variant.enum_address.as_mut(),
                Self::Reference(reference) => {
                    self = reference.reference_address.as_mut();
                }
            }
        }
    }
}

impl Values {
    /// Retrieves the type held in the given memory address without type
    /// simplification/transformation.
    pub async fn simple_type_of_memory(
        &self,
        address: &Memory,
        envionment: &Environment<'_, impl Normalizer>,
    ) -> Result<Type, CyclicError> {
        match address {
            Memory::Parameter(id) => {
                let function_signature = envionment
                    .tracked_engine()
                    .get_parameters(envionment.current_site)
                    .await?;

                let ty = function_signature.parameters[*id].r#type.clone();

                Ok(ty)
            }

            Memory::Alloca(id) => {
                let alloca = &self.allocas[*id];

                Ok(alloca.r#type.clone())
            }

            Memory::ClosureParameter(id) => {
                let closure_parameter = &envionment.closure_parameters()[*id];

                Ok(closure_parameter.r#type.clone())
            }
            Memory::Capture(id) => {
                let capture = &envionment.captures()[*id];

                Ok(capture.address_type.clone())
            }
        }
    }
}

impl TypeOf<&Address> for Values {
    #[allow(clippy::too_many_lines)]
    async fn type_of<N: Normalizer>(
        &self,
        address: &Address,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        match address {
            Address::Memory(Memory::Parameter(parameter)) => {
                let function_signature = environment
                    .tracked_engine()
                    .get_parameters(environment.current_site)
                    .await?;

                let ty =
                    function_signature.parameters[*parameter].r#type.clone();

                Ok(environment
                    .type_environment
                    .simplify(ty)
                    .await?
                    .deref()
                    .clone())
            }

            Address::Memory(Memory::Capture(parameter)) => {
                let capture = &environment.captures()[*parameter];

                let mut ty = environment
                    .type_environment
                    .simplify(capture.address_type.clone())
                    .await?
                    .deref()
                    .clone();

                match &capture.capture_mode {
                    crate::capture::CaptureMode::ByValue => Ok(ty),
                    crate::capture::CaptureMode::ByReference(
                        reference_capture_mode,
                    ) => {
                        ty.result = Type::Reference(r#type::Reference {
                            qualifier: reference_capture_mode.qualifier,
                            lifetime: reference_capture_mode.lifetime.clone(),
                            pointee: Box::new(ty.result),
                        });

                        Ok(ty)
                    }
                }
            }

            Address::Memory(Memory::ClosureParameter(parameter)) => {
                let closure_parameter =
                    &environment.closure_parameters()[*parameter];

                Ok(environment
                    .type_environment
                    .simplify(closure_parameter.r#type.clone())
                    .await?
                    .deref()
                    .clone())
            }

            Address::Memory(Memory::Alloca(parameter)) => {
                let alloca = &self.allocas[*parameter];

                Ok(environment
                    .type_environment
                    .simplify(alloca.r#type.clone())
                    .await?
                    .deref()
                    .clone())
            }

            Address::Field(field_address) => {
                // the type of address should've been simplified
                let Succeeded { result, mut constraints } = Box::pin(
                    self.type_of(&*field_address.struct_address, environment),
                )
                .await?;
                let Type::Symbol(Symbol { id: struct_id, generic_arguments }) =
                    result
                else {
                    panic!("expected struct type");
                };

                let generic_parameters = environment
                    .tracked_engine()
                    .get_generic_parameters(struct_id)
                    .await?;

                let fields =
                    environment.tracked_engine().get_fields(struct_id).await?;

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    struct_id,
                    &generic_parameters,
                )
                .unwrap();

                let mut field_ty =
                    fields.fields[field_address.id].r#type.clone();

                instantiation.instantiate(&mut field_ty);
                let simplification =
                    environment.type_environment.simplify(field_ty).await?;
                constraints.extend(simplification.constraints.iter().cloned());

                Ok(Succeeded {
                    result: simplification.result.clone(),
                    constraints,
                })
            }

            Address::Tuple(tuple) => {
                Ok(Box::pin(self.type_of(&*tuple.tuple_address, environment))
                    .await?
                    .map(|x| {
                        // extract tuple type
                        let Type::Tuple(mut tuple_ty) = x else {
                            panic!("expected tuple type");
                        };

                        match match tuple.offset {
                            Offset::FromStart(id) => Some(id),
                            Offset::FromEnd(id) => tuple_ty
                                .elements
                                .len()
                                .checked_sub(1)
                                .and_then(|x| x.checked_sub(id)),
                            Offset::Unpacked => {
                                let Some(unpacked_ty) =
                                    tuple_ty.elements.iter().find_map(|x| {
                                        x.is_unpacked.then_some(&x.term)
                                    })
                                else {
                                    panic!("expected unpacked tuple element");
                                };

                                return unpacked_ty.clone();
                            }
                        } {
                            Some(id) if id < tuple_ty.elements.len() => {
                                let element_ty = tuple_ty.elements.remove(id);

                                if element_ty.is_unpacked {
                                    Type::Tuple(tuple::Tuple {
                                        elements: vec![tuple::Element {
                                            term: element_ty.term,
                                            is_unpacked: true,
                                        }],
                                    })
                                } else {
                                    element_ty.term
                                }
                            }

                            _ => panic!("invalid tuple offset"),
                        }
                    }))
            }

            Address::Index(index) => {
                Ok(Box::pin(self.type_of(&*index.array_address, environment))
                    .await?
                    .map(|x| *x.into_array().unwrap().r#type))
            }

            Address::Variant(variant) => {
                let Succeeded { result, mut constraints } =
                    Box::pin(self.type_of(&*variant.enum_address, environment))
                        .await?;

                let Type::Symbol(Symbol { id: enum_id, generic_arguments }) =
                    result
                else {
                    panic!("expected enum type");
                };

                let enum_generic_params = environment
                    .tracked_engine()
                    .get_generic_parameters(enum_id)
                    .await?;

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    enum_id,
                    &enum_generic_params,
                )
                .unwrap();

                let variant = environment
                    .tracked_engine()
                    .get_variant_associated_type(variant.id)
                    .await?;

                let mut variant_ty = variant.as_deref().cloned().unwrap();

                instantiation.instantiate(&mut variant_ty);

                let simplification =
                    environment.type_environment.simplify(variant_ty).await?;
                constraints.extend(simplification.constraints.iter().cloned());

                Ok(Succeeded::with_constraints(
                    simplification.result.clone(),
                    constraints,
                ))
            }

            Address::Reference(value) => Ok(Box::pin(
                self.type_of(&*value.reference_address, environment),
            )
            .await?
            .map(|x| *x.into_reference().unwrap().pointee)),
        }
    }
}

impl Address {
    /// Transforms the types in the address using the provided transformer.
    pub async fn transform<T: Transformer<Type>>(
        mut self: &mut Self,
        transformer: &mut T,
    ) -> Result<(), CyclicError> {
        loop {
            match self {
                Self::Memory(_) => return Ok(()),

                Self::Field(field) => {
                    self = field.struct_address.as_mut();
                }
                Self::Tuple(tuple) => {
                    self = tuple.tuple_address.as_mut();
                }
                Self::Index(index) => {
                    if let Value::Literal(literal) = &mut index.indexing_value {
                        literal.transform(transformer).await?;
                    }

                    self = index.array_address.as_mut();
                }
                Self::Variant(variant) => {
                    self = variant.enum_address.as_mut();
                }
                Self::Reference(reference) => {
                    self = reference.reference_address.as_mut();
                }
            }
        }
    }
}

impl Values {
    /// Returns the span of the given memory address.
    pub async fn span_of_memory<N: Normalizer>(
        &self,
        address: &Memory,
        environment: &Environment<'_, N>,
    ) -> Result<RelativeSpan, CyclicError> {
        match address {
            Memory::Parameter(id) => {
                let parameters = environment
                    .tracked_engine()
                    .get_parameters(environment.current_site)
                    .await?;

                Ok(parameters.parameters[*id]
                    .span
                    .expect("local target should've included span"))
            }

            Memory::Alloca(id) => Ok(self.allocas[*id].span),

            Memory::Capture(id) => Ok(environment.captures()[*id].span),

            Memory::ClosureParameter(id) => {
                Ok(environment.closure_parameters()[*id].span)
            }
        }
    }
}
