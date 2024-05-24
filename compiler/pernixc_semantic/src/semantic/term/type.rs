//! Contains the definition of [`Type`].

use core::fmt;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use enum_as_inner::EnumAsInner;

use super::{
    constant::Constant, lifetime::Lifetime, GenericArguments, Local,
    MemberSymbol, ModelOf, Never, Symbol, Term, TupleElement,
};
use crate::{
    arena::ID,
    semantic::{
        instantiation::{self, Instantiation},
        mapping::Mapping,
        matching::{self, Match, Matching},
        model::{Default, Model},
        normalizer::Normalizer,
        predicate::{self, Outlives, Predicate, Satisfiability},
        session::{ExceedLimitError, Limit, Session},
        sub_term::{
            AssignSubTermError, Location, SubMemberSymbolLocation,
            SubSymbolLocation, SubTerm, SubTraitMemberLocation,
            SubTupleLocation,
        },
        unification::{self, Unification},
        Environment,
    },
    symbol::{
        self,
        table::{self, representation::Index, DisplayObject, State, Table},
        Enum, GenericID, GlobalID, Struct, TypeParameter, TypeParameterID,
    },
};

/// Enumeration of all symbol kinds (as a type term).
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum SymbolID {
    Struct(ID<Struct>),
    Enum(ID<Enum>),
    Type(ID<symbol::Type>),
}

impl From<SymbolID> for GlobalID {
    fn from(value: SymbolID) -> Self {
        match value {
            SymbolID::Struct(id) => Self::Struct(id),
            SymbolID::Enum(id) => Self::Enum(id),
            SymbolID::Type(id) => Self::Type(id),
        }
    }
}

impl From<SymbolID> for GenericID {
    fn from(value: SymbolID) -> Self {
        match value {
            SymbolID::Struct(id) => Self::Struct(id),
            SymbolID::Enum(id) => Self::Enum(id),
            SymbolID::Type(id) => Self::Type(id),
        }
    }
}

/// Enumeration of either a trait implementation constant or an ADT
/// implementation constant.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum MemberSymbolID {
    TraitImplementation(ID<symbol::TraitImplementationType>),
    AdtImplementation(ID<symbol::AdtImplementationType>),
}

impl From<MemberSymbolID> for GlobalID {
    fn from(value: MemberSymbolID) -> Self {
        match value {
            MemberSymbolID::TraitImplementation(id) => id.into(),
            MemberSymbolID::AdtImplementation(id) => id.into(),
        }
    }
}

impl From<MemberSymbolID> for GenericID {
    fn from(value: MemberSymbolID) -> Self {
        match value {
            MemberSymbolID::TraitImplementation(id) => id.into(),
            MemberSymbolID::AdtImplementation(id) => id.into(),
        }
    }
}

/// A qualifier that can be applied to references/pointers.  
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Qualifier {
    Immutable,
    Mutable,
    Unique,
}

/// Represents a pointer type, denoted by `*QUALIFIER TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer<M: Model> {
    /// The qualifier applied to the pointer.
    pub qualifier: Qualifier,

    /// The type that the pointer points to.
    pub pointee: Box<Type<M>>,
}

/// Represents a reference type, denoted by `&'LIFETIME QUALIFIER TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reference<M: Model> {
    /// The qualifier applied to the reference.
    pub qualifier: Qualifier,

    /// The lifetime that the reference lives in.
    pub lifetime: Lifetime<M>,

    /// The type that the reference points to.
    pub pointee: Box<Type<M>>,
}

/// Represents an array type, denoted by `[ELEMENT: LENGTH]` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array<M: Model> {
    /// Constant representing the length of the array.
    pub length: Constant<M>,

    /// The type of the elements in the array.
    pub r#type: Box<Type<M>>,
}

/// Contains all primitive types in the language.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display(fmt = "int8")]
    Int8,
    #[display(fmt = "int16")]
    Int16,
    #[display(fmt = "int32")]
    Int32,
    #[display(fmt = "int64")]
    Int64,
    #[display(fmt = "uint8")]
    Uint8,
    #[display(fmt = "uint16")]
    Uint16,
    #[display(fmt = "uint32")]
    Uint32,
    #[display(fmt = "uint64")]
    Uint64,
    #[display(fmt = "float32")]
    Float32,
    #[display(fmt = "float64")]
    Float64,
    #[display(fmt = "bool")]
    Bool,
    #[display(fmt = "usize")]
    Usize,
    #[display(fmt = "isize")]
    Isize,
}

/// Represents a tuple type, denoted by `(type, type, ...type)` syntax.
pub type Tuple<M> = super::Tuple<Type<M>>;

/// Represents a trait-member type, denoted by `TRAIT[ARGS]::TYPE[ARGS]`
/// syntax.
pub type TraitMember<M> = MemberSymbol<M, ID<symbol::TraitType>>;

/// Represents a phantom type, denoted by `phantom TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Phantom<M: Model>(pub Box<Type<M>>);

/// The location pointing to a sub-lifetime term in a type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubLifetimeLocation {
    /// The index of lifetime argument in a [`Symbol`] type.
    #[from]
    Symbol(SubSymbolLocation),

    /// The lifetime of a reference.
    Reference,

    /// A lifetime argument in a [`MemberSymbol`] type.
    #[from]
    MemberSymbol(SubMemberSymbolLocation),

    /// A lifetime argument in a [`Type::TraitMember`] variant.
    #[from]
    TraitMember(SubTraitMemberLocation),
}

impl<M: Model> Location<Type<M>, Lifetime<M>> for SubLifetimeLocation {
    fn assign_sub_term(
        self,
        term: &mut Type<M>,
        sub_term: Lifetime<M>,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                &mut reference.lifetime
            }
            (Type::Symbol(symbol), Self::Symbol(location)) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,
            (
                Type::MemberSymbol(member_symbol),
                Self::MemberSymbol(location),
            ) => member_symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Type::TraitMember(trait_member), Self::TraitMember(location)) => {
                trait_member
                    .get_term_mut(location.0)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;
        Ok(())
    }

    fn get_sub_term(self, term: &Type<M>) -> Option<Lifetime<M>> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(reference.lifetime.clone())
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Type::MemberSymbol(member_symbol),
                Self::MemberSymbol(location),
            ) => member_symbol.get_term(location).cloned(),

            (Type::TraitMember(trait_member), Self::TraitMember(location)) => {
                trait_member.get_term(location.0).cloned()
            }

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type<M>) -> Option<&Lifetime<M>> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(&reference.lifetime)
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location)
            }

            (
                Type::MemberSymbol(member_symbol),
                Self::MemberSymbol(location),
            ) => member_symbol.get_term(location),

            (Type::TraitMember(trait_member), Self::TraitMember(location)) => {
                trait_member.get_term(location.0)
            }

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type<M>) -> Option<&mut Lifetime<M>> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(&mut reference.lifetime)
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location)
            }

            (
                Type::MemberSymbol(member_symbol),
                Self::MemberSymbol(location),
            ) => member_symbol.get_term_mut(location),

            (Type::TraitMember(trait_member), Self::TraitMember(location)) => {
                trait_member.get_term_mut(location.0)
            }

            _ => None,
        }
    }
}

/// The location pointing to a sub-type term in a type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubTypeLocation {
    /// The index of the type argument in a [`Symbol`] type.
    #[from]
    Symbol(SubSymbolLocation),

    /// The [`Pointer::pointee`] of a pointer.
    Pointer,

    /// The [`Reference::pointee`] of a reference.
    Reference,

    /// The [`Array::r#type`] of an array.
    Array,

    /// The index of the type element in a [`Tuple`] type.
    #[from]
    Tuple(SubTupleLocation),

    /// The inner type of a [`Local`] type.
    Local,

    /// The inner type of a [`Phantom`] type.
    Phantom,

    /// The type argument in a [`MemberSymbol`] type.
    #[from]
    MemberSymbol(SubMemberSymbolLocation),

    /// A type argument in a [`Type::TraitMember`] variant.
    #[from]
    TraitMember(SubTraitMemberLocation),
}

impl<M: Model> Location<Type<M>, Type<M>> for SubTypeLocation {
    fn assign_sub_term(
        self,
        term: &mut Type<M>,
        sub_term: Type<M>,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Self::Pointer, Type::Pointer(pointer)) => &mut *pointer.pointee,

            (Self::Phantom, Type::Phantom(phantom)) => &mut *phantom.0,

            (Self::Reference, Type::Reference(reference)) => {
                &mut *reference.pointee
            }

            (Self::Array, Type::Array(array)) => &mut *array.r#type,

            (Self::Tuple(location), Type::Tuple(tuple)) => {
                return tuple.assign_sub_term(location, sub_term)
            }

            (Self::Local, Type::Local(local)) => &mut *local.0,

            (Self::MemberSymbol(location), Type::MemberSymbol(symbol)) => {
                symbol
                    .get_term_mut(location)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member
                    .get_term_mut(location.0)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;
        Ok(())
    }

    fn get_sub_term(self, term: &Type<M>) -> Option<Type<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::Pointer, Type::Pointer(pointer)) => {
                Some((*pointer.pointee).clone())
            }

            (Self::Reference, Type::Reference(reference)) => {
                Some((*reference.pointee).clone())
            }

            (Self::Array, Type::Array(array)) => Some((*array.r#type).clone()),

            (Self::Tuple(location), Type::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(|x| x.as_term().clone())
                }
                SubTupleLocation::Range { begin, end } => tuple
                    .elements
                    .get(begin..end)
                    .map(|x| Type::Tuple(Tuple { elements: x.to_vec() })),
            },

            (Self::Local, Type::Local(local)) => Some((*local.0).clone()),

            (Self::MemberSymbol(location), Type::MemberSymbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.get_term(location.0).cloned()
            }

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type<M>) -> Option<&Type<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location)
            }

            (Self::Pointer, Type::Pointer(pointer)) => Some(&*pointer.pointee),

            (Self::Reference, Type::Reference(reference)) => {
                Some(&*reference.pointee)
            }

            (Self::Array, Type::Array(array)) => Some(&*array.r#type),

            (Self::Tuple(location), Type::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(TupleElement::as_term)
                }
                SubTupleLocation::Range { .. } => None,
            },

            (Self::Local, Type::Local(local)) => Some(&*local.0),

            (Self::MemberSymbol(location), Type::MemberSymbol(symbol)) => {
                symbol.get_term(location)
            }

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.get_term(location.0)
            }

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type<M>) -> Option<&mut Type<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location)
            }

            (Self::Pointer, Type::Pointer(pointer)) => {
                Some(&mut *pointer.pointee)
            }

            (Self::Reference, Type::Reference(reference)) => {
                Some(&mut *reference.pointee)
            }

            (Self::Array, Type::Array(array)) => Some(&mut *array.r#type),

            (Self::Tuple(location), Type::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => tuple
                    .elements
                    .get_mut(single)
                    .map(TupleElement::as_term_mut),
                SubTupleLocation::Range { .. } => None,
            },

            (Self::Local, Type::Local(local)) => Some(&mut *local.0),

            (Self::MemberSymbol(location), Type::MemberSymbol(symbol)) => {
                symbol.get_term_mut(location)
            }

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.get_term_mut(location.0)
            }

            _ => None,
        }
    }
}

/// The location pointing to a sub-constant term in a type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubConstantLocation {
    /// The index of the constant argument in a [`Symbol`] type.
    #[from]
    Symbol(SubSymbolLocation),

    /// The constant argument in a [`MemberSymbol`] type.
    #[from]
    MemberSymbol(SubMemberSymbolLocation),

    /// The [`Array::length`] of an array.
    Array,

    /// A constant argument in a [`Type::TraitMember`] variant.
    #[from]
    TraitMember(SubTraitMemberLocation),
}

impl<M: Model> Location<Type<M>, Constant<M>> for SubConstantLocation {
    fn assign_sub_term(
        self,
        term: &mut Type<M>,
        sub_term: Constant<M>,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Self::MemberSymbol(location), Type::MemberSymbol(symbol)) => {
                symbol
                    .get_term_mut(location)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            (Self::Array, Type::Array(array)) => &mut array.length,

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member
                    .get_term_mut(location.0)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;
        Ok(())
    }

    fn get_sub_term(self, term: &Type<M>) -> Option<Constant<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::MemberSymbol(location), Type::MemberSymbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::Array, Type::Array(array)) => Some(array.length.clone()),

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.get_term(location.0).cloned()
            }

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type<M>) -> Option<&Constant<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location)
            }

            (Self::MemberSymbol(location), Type::MemberSymbol(symbol)) => {
                symbol.get_term(location)
            }

            (Self::Array, Type::Array(array)) => Some(&array.length),

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.get_term(location.0)
            }

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type<M>) -> Option<&mut Constant<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location)
            }

            (Self::MemberSymbol(location), Type::MemberSymbol(symbol)) => {
                symbol.get_term_mut(location)
            }

            (Self::Array, Type::Array(array)) => Some(&mut array.length),

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.get_term_mut(location.0)
            }

            _ => None,
        }
    }
}

impl<M: Model> SubTerm for Type<M> {
    type SubLifetimeLocation = SubLifetimeLocation;
    type SubTypeLocation = SubTypeLocation;
    type SubConstantLocation = SubConstantLocation;
    type ThisSubTermLocation = SubTypeLocation;
}

impl<M: Model> Match for Type<M> {
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    > {
        match (self, other) {
            (Self::Symbol(lhs), Self::Symbol(rhs)) if lhs.id == rhs.id => {
                lhs.generic_arguments.substructural_match(
                    &rhs.generic_arguments,
                    matching::Substructural::default(),
                    SubSymbolLocation,
                )
            }

            (Self::Pointer(lhs), Self::Pointer(rhs))
                if lhs.qualifier == rhs.qualifier =>
            {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: vec![Matching {
                        lhs: (*lhs.pointee).clone(),
                        rhs: (*rhs.pointee).clone(),
                        lhs_location: SubTypeLocation::Pointer,
                        rhs_location: SubTypeLocation::Pointer,
                    }],
                    constants: Vec::new(),
                })
            }

            (Self::Reference(lhs), Self::Reference(rhs))
                if lhs.qualifier == rhs.qualifier =>
            {
                Some(matching::Substructural {
                    lifetimes: vec![Matching {
                        lhs: lhs.lifetime.clone(),
                        rhs: rhs.lifetime.clone(),
                        lhs_location: SubLifetimeLocation::Reference,
                        rhs_location: SubLifetimeLocation::Reference,
                    }],
                    types: vec![Matching {
                        lhs: (*lhs.pointee).clone(),
                        rhs: (*rhs.pointee).clone(),
                        lhs_location: SubTypeLocation::Reference,
                        rhs_location: SubTypeLocation::Reference,
                    }],
                    constants: Vec::new(),
                })
            }

            (Self::Array(lhs), Self::Array(rhs)) => {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: vec![Matching {
                        lhs: (*lhs.r#type).clone(),
                        rhs: (*rhs.r#type).clone(),
                        lhs_location: SubTypeLocation::Array,
                        rhs_location: SubTypeLocation::Array,
                    }],
                    constants: vec![Matching {
                        lhs: lhs.length.clone(),
                        rhs: rhs.length.clone(),
                        lhs_location: SubConstantLocation::Array,
                        rhs_location: SubConstantLocation::Array,
                    }],
                })
            }

            (Self::Local(lhs), Self::Local(rhs)) => {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: vec![Matching {
                        lhs: (*lhs.0).clone(),
                        rhs: (*rhs.0).clone(),
                        lhs_location: SubTypeLocation::Local,
                        rhs_location: SubTypeLocation::Local,
                    }],
                    constants: Vec::new(),
                })
            }

            (Self::Tuple(lhs), Self::Tuple(rhs)) => {
                lhs.substructural_match(rhs)
            }

            (Self::MemberSymbol(lhs), Self::MemberSymbol(rhs))
                if lhs.id == rhs.id =>
            {
                lhs.parent_generic_arguments
                    .substructural_match(
                        &rhs.parent_generic_arguments,
                        matching::Substructural::default(),
                        |x| SubMemberSymbolLocation {
                            index: x,
                            from_parent: true,
                        },
                    )
                    .and_then(|x| {
                        lhs.member_generic_arguments.substructural_match(
                            &rhs.member_generic_arguments,
                            x,
                            |x| SubMemberSymbolLocation {
                                index: x,
                                from_parent: false,
                            },
                        )
                    })
            }

            _ => None,
        }
    }

    fn get_substructural(
        substructural: &matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.types
    }

    fn get_substructural_mut(
        substructural: &mut matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.types
    }
}

/// Represents a type term.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Type<M: Model> {
    #[from]
    Primitive(Primitive),
    #[from]
    Parameter(TypeParameterID),
    Inference(M::TypeInference),
    #[from]
    Symbol(Symbol<M, SymbolID>),
    #[from]
    Pointer(Pointer<M>),
    #[from]
    Reference(Reference<M>),
    #[from]
    Array(Array<M>),
    #[from]
    Tuple(Tuple<M>),
    #[from]
    Local(Local<Self>),
    #[from]
    Phantom(Phantom<M>),
    #[from]
    MemberSymbol(MemberSymbol<M, MemberSymbolID>),
    #[from]
    TraitMember(TraitMember<M>),
}

impl<M: Model> From<Never> for Type<M> {
    fn from(value: Never) -> Self { match value {} }
}

impl<M: Model> ModelOf for Type<M> {
    type Model = M;
}

impl<M: Model> TryFrom<Type<M>> for Tuple<M> {
    type Error = Type<M>;

    fn try_from(value: Type<M>) -> Result<Self, Self::Error> {
        value.into_tuple()
    }
}

impl<M: Model> TryFrom<Type<M>> for TypeParameterID {
    type Error = Type<M>;

    fn try_from(value: Type<M>) -> Result<Self, Self::Error> {
        value.into_parameter()
    }
}

impl<M: Model> std::default::Default for Type<M> {
    fn default() -> Self { Self::Tuple(Tuple { elements: Vec::new() }) }
}

impl<M: Model> Term for Type<M>
where
    Self: ModelOf<Model = M>,
{
    type GenericParameter = TypeParameter;
    type TraitMember = TraitMember<M>;
    type Rebind<Ms: Model> = Type<Ms>;

    #[allow(clippy::too_many_lines)]
    fn normalize(
        &self,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        limit: &mut Limit<
            impl Session<Lifetime<M>> + Session<Self> + Session<Constant<M>>,
        >,
    ) -> Result<Option<Self>, ExceedLimitError> {
        match self {
            // transform type alias into the aliased type equivalent
            Self::Symbol(Symbol {
                id: SymbolID::Type(id),
                generic_arguments,
            }) => {
                let Some(type_sym) = environment.table.get(*id) else {
                    return Ok(None);
                };

                let mut type_aliased =
                    M::from_default_type(type_sym.r#type.clone());

                let Ok(inst) = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    (*id).into(),
                    &type_sym.generic_declaration.parameters,
                ) else {
                    return Ok(None);
                };
                instantiation::instantiate(&mut type_aliased, &inst);

                Ok(Some(type_aliased))
            }

            // transform the trait-member into trait-implementation-type
            // equivalent
            Self::TraitMember(trait_member) => {
                let Some((trait_id, name)) = environment
                    .table
                    .get(trait_member.id)
                    .map(|x| (x.parent_id(), x.name().to_owned()))
                else {
                    // invalid id
                    return Ok(None);
                };

                // resolve for the appropriate trait-implementation
                let Ok(mut result) = predicate::resolve_implementation(
                    trait_id,
                    &trait_member.parent_generic_arguments,
                    environment,
                    limit,
                ) else {
                    return Ok(None);
                };

                let Some(implementation_type_id) =
                    environment.table.get(result.id).and_then(|x| {
                        x.member_ids_by_name()
                            .get(&name)
                            .cloned()
                            .and_then(|x| x.into_type().ok())
                    })
                else {
                    return Ok(None);
                };

                let result_ty = Self::MemberSymbol(MemberSymbol {
                    id: implementation_type_id.into(),
                    member_generic_arguments: trait_member
                        .member_generic_arguments
                        .clone(),
                    parent_generic_arguments: trait_member
                        .parent_generic_arguments
                        .clone(),
                });

                // append the deduced generic arguments
                Ok(Some(result_ty))
            }

            // transform trait-implementation-type into the aliased type
            Self::MemberSymbol(MemberSymbol {
                id: MemberSymbolID::TraitImplementation(id),
                member_generic_arguments,
                parent_generic_arguments,
            }) => {
                let Some(implementation_type_symbol) =
                    environment.table.get(*id)
                else {
                    return Ok(None);
                };

                let Some(implementation_symbol) = environment
                    .table
                    .get(implementation_type_symbol.parent_id())
                else {
                    return Ok(None);
                };

                let adt_implementation_symbol_arguments =
                    GenericArguments::from_default_model(
                        implementation_symbol.arguments.clone(),
                    );

                // gets the decution for the parent generic arguments
                let Some(mut deduction) = adt_implementation_symbol_arguments
                    .deduce(
                    parent_generic_arguments,
                    environment,
                    limit,
                )?
                else {
                    return Ok(None);
                };

                if deduction
                    .append_from_generic_arguments(
                        member_generic_arguments.clone(),
                        (*id).into(),
                        &implementation_type_symbol
                            .generic_declaration
                            .parameters,
                    )
                    .is_err()
                {
                    return Ok(None);
                }

                let mut aliased = M::from_default_type(
                    implementation_type_symbol.r#type.clone(),
                );

                instantiation::instantiate(&mut aliased, &deduction);

                Ok(Some(aliased))
            }

            // transform into its aliased equivalent
            Self::MemberSymbol(MemberSymbol {
                id: MemberSymbolID::AdtImplementation(id),
                member_generic_arguments,
                parent_generic_arguments,
            }) => {
                let Some(implementation_type_symbol) =
                    environment.table.get(*id)
                else {
                    return Ok(None);
                };

                let Some(adt_implementation_symbol) = environment
                    .table
                    .get(implementation_type_symbol.parent_id())
                else {
                    return Ok(None);
                };

                let adt_implementation_symbol_arguments =
                    GenericArguments::from_default_model(
                        adt_implementation_symbol.arguments.clone(),
                    );

                // gets the decution for the parent generic arguments
                let Some(mut deduction) = adt_implementation_symbol_arguments
                    .deduce(
                    parent_generic_arguments,
                    environment,
                    limit,
                )?
                else {
                    return Ok(None);
                };

                if deduction
                    .append_from_generic_arguments(
                        member_generic_arguments.clone(),
                        (*id).into(),
                        &implementation_type_symbol
                            .generic_declaration
                            .parameters,
                    )
                    .is_err()
                {
                    return Ok(None);
                }

                let mut aliased = M::from_default_type(
                    implementation_type_symbol.r#type.clone(),
                );

                instantiation::instantiate(&mut aliased, &deduction);

                Ok(Some(aliased))
            }

            // unpack the tuple
            Self::Tuple(tuple) => {
                let contain_upacked =
                    tuple.elements.iter().any(super::TupleElement::is_unpacked);

                if !contain_upacked {
                    return Ok(None);
                }

                let mut result = Vec::new();

                for element in tuple.elements.iter().cloned() {
                    match element {
                        regular @ super::TupleElement::Regular(_) => {
                            result.push(regular);
                        }
                        super::TupleElement::Unpacked(term) => match term {
                            Self::Tuple(inner) => {
                                result.extend(inner.elements.iter().cloned());
                            }
                            term => {
                                result
                                    .push(super::TupleElement::Unpacked(term));
                            }
                        },
                    }
                }

                Ok(Some(Self::Tuple(Tuple { elements: result })))
            }

            Self::Inference(inference) => {
                Normalizer::normalize_type(inference, environment, limit)
            }

            _ => Ok(None),
        }
    }

    fn outlives_satisfiability(
        &self,
        _: &Lifetime<M>,
        _: &Environment<M, impl State, impl Normalizer<M>>,
        _: &mut Limit<
            impl Session<Self>
                + Session<Lifetime<M>>
                + Session<Self>
                + Session<Constant<M>>,
        >,
    ) -> Result<Satisfiability, ExceedLimitError> {
        match self {
            Self::Primitive(_) => Ok(Satisfiability::Satisfied),

            Self::Inference(_) | Self::Parameter(_) => {
                Ok(Satisfiability::Unsatisfied)
            }

            Self::TraitMember(_)
            | Self::Local(_)
            | Self::Symbol(_)
            | Self::Pointer(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Tuple(_)
            | Self::Phantom(_)
            | Self::MemberSymbol(_) => Ok(Satisfiability::Congruent),
        }
    }

    fn as_generic_parameter(&self) -> Option<&TypeParameterID> {
        self.as_parameter()
    }

    fn as_generic_parameter_mut(&mut self) -> Option<&mut TypeParameterID> {
        self.as_parameter_mut()
    }

    fn into_generic_parameter(self) -> Result<TypeParameterID, Self> {
        self.into_parameter()
    }

    fn as_trait_member(&self) -> Option<&TraitMember<M>> {
        match self {
            Self::TraitMember(trait_member) => Some(trait_member),
            _ => None,
        }
    }

    fn as_trait_member_mut(&mut self) -> Option<&mut TraitMember<M>> {
        match self {
            Self::TraitMember(trait_member) => Some(trait_member),
            _ => None,
        }
    }

    fn into_trait_member(self) -> Result<TraitMember<M>, Self> {
        match self {
            Self::TraitMember(trait_member) => Ok(trait_member),
            _ => Err(self),
        }
    }

    fn as_tuple(&self) -> Option<&Tuple<M>> {
        match self {
            Self::Tuple(tuple) => Some(tuple),
            _ => None,
        }
    }

    fn as_tuple_mut(&mut self) -> Option<&mut Tuple<M>> {
        match self {
            Self::Tuple(tuple) => Some(tuple),
            _ => None,
        }
    }

    fn into_tuple(self) -> Result<Tuple<M>, Self> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            _ => Err(self),
        }
    }

    fn get_adt_fields(&self, table: &Table<impl State>) -> Option<Vec<Self>> {
        match self {
            Self::Symbol(Symbol { id, generic_arguments }) => match *id {
                SymbolID::Struct(struct_id) => {
                    let struct_sym = table.get(struct_id)?;

                    let Ok(substitution) =
                        Instantiation::from_generic_arguments(
                            generic_arguments.clone(),
                            struct_id.into(),
                            &struct_sym.generic_declaration.parameters,
                        )
                    else {
                        return None;
                    };

                    Some(
                        struct_sym
                            .fields
                            .values()
                            .map(|field| {
                                let mut ty =
                                    M::from_default_type(field.r#type.clone());
                                instantiation::instantiate(
                                    &mut ty,
                                    &substitution,
                                );
                                ty
                            })
                            .collect(),
                    )
                }
                SymbolID::Enum(enum_id) => {
                    let enum_sym = table.get(enum_id)?;

                    let Ok(substitution) =
                        Instantiation::from_generic_arguments(
                            generic_arguments.clone(),
                            enum_id.into(),
                            &enum_sym.generic_declaration.parameters,
                        )
                    else {
                        return None;
                    };

                    Some(
                        enum_sym
                            .variant_ids_by_name()
                            .values()
                            .copied()
                            .filter_map(|variant| table.get(variant))
                            .filter_map(|variant| {
                                let mut ty = M::from_default_type(
                                    variant.associated_type.as_ref()?.clone(),
                                );

                                instantiation::instantiate(
                                    &mut ty,
                                    &substitution,
                                );
                                Some(ty)
                            })
                            .collect(),
                    )
                }
                SymbolID::Type(_) => None,
            },

            _ => None,
        }
    }

    fn as_outlive_predicate(
        predicate: &Predicate<M>,
    ) -> Option<&Outlives<Self>> {
        predicate.as_type_outlives()
    }

    fn as_outlive_predicate_mut(
        predicate: &mut Predicate<M>,
    ) -> Option<&mut Outlives<Self>> {
        predicate.as_type_outlives_mut()
    }

    fn into_outlive_predicate(
        predicate: Predicate<M>,
    ) -> Result<Outlives<Self>, Predicate<M>> {
        predicate.into_type_outlives()
    }

    fn as_constant_type_predicate(predicate: &Predicate<M>) -> Option<&Self> {
        predicate.as_constant_type().map(|x| &x.0)
    }

    fn as_constant_type_predicate_mut(
        predicate: &mut Predicate<M>,
    ) -> Option<&mut Self> {
        predicate.as_constant_type_mut().map(|x| &mut x.0)
    }

    fn into_constant_type_predicate(
        predicate: Predicate<M>,
    ) -> Result<Self, Predicate<M>> {
        predicate.into_constant_type().map(|x| x.0)
    }

    fn as_trait_member_equality_predicate(
        predicate: &Predicate<M>,
    ) -> Option<&predicate::Equality<TraitMember<M>, Self>> {
        predicate.as_trait_type_equality()
    }

    fn as_trait_member_equality_predicate_mut(
        predicate: &mut Predicate<M>,
    ) -> Option<&mut predicate::Equality<TraitMember<M>, Self>> {
        predicate.as_trait_type_equality_mut()
    }

    fn into_trait_member_equality_predicate(
        predicate: Predicate<M>,
    ) -> Result<predicate::Equality<TraitMember<M>, Self>, Predicate<M>> {
        predicate.into_trait_type_equality()
    }

    fn as_tuple_predicate(
        predicate: &Predicate<M>,
    ) -> Option<&predicate::Tuple<Self>> {
        predicate.as_tuple_type()
    }

    fn as_tuple_predicate_mut(
        predicate: &mut Predicate<M>,
    ) -> Option<&mut predicate::Tuple<Self>> {
        predicate.as_tuple_type_mut()
    }

    fn into_tuple_predicate(
        predicate: Predicate<M>,
    ) -> Result<predicate::Tuple<Self>, Predicate<M>> {
        predicate.into_tuple_type()
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::Local(_)
            | Self::Pointer(_)
            | Self::Symbol(_)
            | Self::MemberSymbol(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::TraitMember(_)
            | Self::Phantom(_)
            | Self::Tuple(_) => Satisfiability::Congruent,
        }
    }

    fn constant_type_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Primitive(primitive_type) => match primitive_type {
                Primitive::Int8
                | Primitive::Int16
                | Primitive::Int32
                | Primitive::Int64
                | Primitive::Uint8
                | Primitive::Uint16
                | Primitive::Uint32
                | Primitive::Uint64
                | Primitive::Bool
                | Primitive::Usize
                | Primitive::Isize => Satisfiability::Satisfied,

                Primitive::Float32 | Primitive::Float64 => {
                    Satisfiability::Unsatisfied
                }
            },

            Self::TraitMember(_)
            | Self::Parameter(_)
            | Self::Inference(_)
            | Self::MemberSymbol(_) => Satisfiability::Unsatisfied,

            Self::Symbol(Symbol { id, .. }) => match id {
                SymbolID::Struct(_) | SymbolID::Enum(_) => {
                    Satisfiability::Congruent
                }

                SymbolID::Type(_) => Satisfiability::Unsatisfied,
            },

            Self::Pointer(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Tuple(_)
            | Self::Phantom(_)
            | Self::Local(_) => Satisfiability::Congruent,
        }
    }

    fn get_instantiation(
        instantiation: &Instantiation<M>,
    ) -> &HashMap<Self, Self> {
        &instantiation.types
    }

    fn get_instantiation_mut(
        instantiation: &mut Instantiation<M>,
    ) -> &mut HashMap<Self, Self> {
        &mut instantiation.types
    }

    fn get_substructural_unification<'a, T: Term>(
        substructural: &'a unification::Substructural<T>,
    ) -> impl Iterator<Item = &'a Unification<Type<T::Model>>>
    where
        Self: 'a,
    {
        substructural.types.values()
    }

    fn get_mapping(mapping: &Mapping<M>) -> &HashMap<Self, HashSet<Self>> {
        &mapping.types
    }

    fn get_mapping_mut(
        mapping: &mut Mapping<M>,
    ) -> &mut HashMap<Self, HashSet<Self>> {
        &mut mapping.types
    }

    fn get_generic_arguments(
        generic_arguments: &GenericArguments<M>,
    ) -> &[Self] {
        &generic_arguments.types
    }

    fn get_generic_arguments_mut(
        generic_arguments: &mut GenericArguments<M>,
    ) -> &mut Vec<Self> {
        &mut generic_arguments.types
    }

    fn from_default_model(term: Type<Default>) -> Self {
        M::from_default_type(term)
    }
}

impl<T: State, M: Model> table::Display<T> for Type<M> {
    fn fmt(
        &self,
        table: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Primitive(primitve) => {
                write!(f, "{primitve}")
            }
            Self::Parameter(type_parameter) => {
                write!(
                    f,
                    "{}",
                    table
                        .get_generic(type_parameter.parent)
                        .ok_or(fmt::Error)?
                        .generic_declaration()
                        .parameters
                        .types()
                        .get(type_parameter.id)
                        .ok_or(fmt::Error)?
                        .name
                        .as_deref()
                        .unwrap_or("{unknown}")
                )
            }
            Self::Inference(_) => write!(f, "?"),
            Self::Symbol(symbol) => {
                write!(f, "{}", DisplayObject { table, display: symbol })
            }
            Self::Pointer(pointer) => {
                write!(f, "*")?;

                match pointer.qualifier {
                    Qualifier::Immutable => {}
                    Qualifier::Mutable => {
                        write!(f, "mutable ")?;
                    }
                    Qualifier::Unique => {
                        write!(f, "unique ")?;
                    }
                }

                write!(f, "{}", DisplayObject {
                    table,
                    display: &*pointer.pointee
                })
            }
            Self::Reference(reference) => {
                write!(f, "&")?;

                match reference.qualifier {
                    Qualifier::Immutable => {}
                    Qualifier::Mutable => {
                        write!(f, "mutable ")?;
                    }
                    Qualifier::Unique => {
                        write!(f, "unique ")?;
                    }
                }

                write!(f, "{} ", DisplayObject {
                    table,
                    display: &reference.lifetime
                })?;

                write!(f, "{}", DisplayObject {
                    table,
                    display: &*reference.pointee
                })
            }
            Self::Array(array) => {
                write!(
                    f,
                    "[{}: {}]",
                    DisplayObject { table, display: &*array.r#type },
                    DisplayObject { table, display: &array.length },
                )
            }
            Self::Tuple(tuple) => {
                write!(f, "{}", DisplayObject { table, display: tuple })
            }
            Self::Local(ty) => {
                write!(f, "local {}", DisplayObject { table, display: &*ty.0 })
            }
            Self::MemberSymbol(ty) => {
                write!(f, "{}", DisplayObject { table, display: ty })
            }
            Self::TraitMember(ty) => {
                write!(f, "{}", DisplayObject { table, display: ty })
            }
            Self::Phantom(phantom) => {
                write!(f, "phantom {}", DisplayObject {
                    table,
                    display: &*phantom.0
                })
            }
        }
    }
}

impl<M: Model> Type<M> {
    /// Gets a list of [`GlobalID`]s that occur in the type.
    #[must_use]
    pub fn get_global_id_dependencies(
        &self,
        table: &Table<impl State>,
    ) -> Option<Vec<GlobalID>> {
        let mut occurrences = match self {
            Self::Primitive(_) | Self::Parameter(_) | Self::Inference(_) => {
                return Some(Vec::new());
            }

            Self::Symbol(symbol) => {
                let mut occurrences = symbol
                    .generic_arguments
                    .get_global_id_dependencies(table)?;
                occurrences.push(symbol.id.into());
                occurrences
            }

            Self::Pointer(symbol) => {
                return symbol.pointee.get_global_id_dependencies(table);
            }
            Self::Reference(symbol) => {
                return symbol.pointee.get_global_id_dependencies(table);
            }

            Self::Array(array) => {
                let mut occurrences =
                    array.r#type.get_global_id_dependencies(table)?;
                occurrences
                    .extend(array.length.get_global_id_dependencies(table)?);
                occurrences
            }
            Self::Tuple(tuple) => {
                let mut occurrences = Vec::new();
                for element in &tuple.elements {
                    occurrences.extend(
                        element.as_term().get_global_id_dependencies(table)?,
                    );
                }
                occurrences
            }

            Self::Local(local) => {
                return local.0.get_global_id_dependencies(table);
            }

            Self::MemberSymbol(member_symbol) => {
                let mut occurrences = member_symbol
                    .parent_generic_arguments
                    .get_global_id_dependencies(table)?;
                occurrences.extend(
                    member_symbol
                        .member_generic_arguments
                        .get_global_id_dependencies(table)?,
                );

                occurrences.push(member_symbol.id.into());

                match member_symbol.id {
                    MemberSymbolID::TraitImplementation(id) => {
                        let implementation_id = table.get(id)?.parent_id();
                        let trait_id =
                            table.get(implementation_id)?.implemented_id();

                        occurrences.push(implementation_id.into());
                        occurrences.push(trait_id.into());
                    }
                    MemberSymbolID::AdtImplementation(id) => {
                        let implementation_id = table.get(id)?.parent_id();
                        let adt_kind_id =
                            table.get(implementation_id)?.implemented_id();

                        occurrences.push(implementation_id.into());
                        occurrences.push(adt_kind_id.into());
                    }
                }

                occurrences
            }
            Self::TraitMember(member_symbol) => {
                let mut occurrences = member_symbol
                    .parent_generic_arguments
                    .get_global_id_dependencies(table)?;
                occurrences.extend(
                    member_symbol
                        .member_generic_arguments
                        .get_global_id_dependencies(table)?,
                );

                occurrences.push(member_symbol.id.into());
                occurrences
                    .push(table.get(member_symbol.id)?.parent_id().into());

                occurrences
            }
            Self::Phantom(phantom) => {
                return phantom.0.get_global_id_dependencies(table);
            }
        };

        occurrences.sort_unstable();
        occurrences.dedup();

        Some(occurrences)
    }
}

#[cfg(test)]
mod tests;
