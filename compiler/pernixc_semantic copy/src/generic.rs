//! Contains code related to generic, traits, bounds, and substitution.
//!
//! Since type substitution, trait resolution, and constraint checking are closely related, they
//! are all implemented in this module.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    marker::PhantomData,
};

use enum_as_inner::EnumAsInner;
use getset::Getters;

use crate::{
    constant,
    symbol::{
        GenericItemRef, ImplementsRef, ImplementsTypeRef, LifetimeParameterRef, LocalImplementsRef,
        LocalSubstitution, Substitution, TraitAssociatedRef, TraitRef, TypeParameterRef,
    },
    table::Table,
    ty::{self, Lifetime},
};

mod deduction;

/// Represents a where clause that can be defined in a symbol.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct Constraints {
    /// Contains both trait-associated type and constant bounds, denoted by `trait<args>::assoc =
    /// bound`
    #[get = "pub"]
    associated_bounds: AssociatedBounds,

    /// Represents the lifetime bounds
    ///
    /// Where the key is the operand that will be tested against lifetimes whether or not the
    /// operand outlives the other lifetimes.
    #[get = "pub"]
    lifetime_bounds: HashMap<LifetimeBoundOperand, HashSet<Lifetime>>,

    /// Represents the trait bounds
    ///
    /// Where the key is the trait bound and the value is whether or not the trait bound is a
    /// constant trait bound or not.
    #[get = "pub"]
    trait_bounds: HashMap<TraitBound, bool>,

    /// Represents the constant type bounds
    ///
    /// These types must be usable as a type of a constant value.
    #[get = "pub"]
    constant_type_bounds: HashSet<ConstantTypeOperand>,
}

/// Contains kinds of type that can be bounded as a constant type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ConstantTypeOperand {
    Parameter(TypeParameterRef),
    TraitAssociated(TraitAssociatedRef),
}

/// Represents a higher-ranked lifetime, denoted by `for<'a>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetime {
    /// The unique [`usize`] value that distinguishes this lifetime from other lifetimes.
    pub unique_id: usize,
}

/// A lifetime that may be higher-ranked. Used in [`TraitBound`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum HigherRankedableLifetime {
    Regular(Lifetime),
    HigherRanked(HigherRankedLifetime),
}

/// Represents a trait bound in the where clause.
///
/// This bound states that there must be an `implements` for the trait with the given index and
/// generic arguments.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitBound {
    pub trait_ref: TraitRef,
    pub type_substituions: Vec<ty::Type>,
    pub lifetime_substituions: Vec<HigherRankedableLifetime>,
    pub constant_substituions: Vec<constant::Constant>,
}

/// An operand that will be tested against other lifetimes whether or not the operand outlives the
/// other lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeBoundOperand {
    TypeParameter(TypeParameterRef),
    TraitAssociated(ty::TraitAssociated),
    Parameter(LifetimeParameterRef),
}

/// Contains all the associated bounds.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct AssociatedBounds {
    pub associated_type_bounds: HashMap<ty::TraitAssociated, ty::Type>,
    pub associated_constant_bounds: HashMap<constant::TraitAssociated, constant::Constant>,
}

/// No implements found for the given trait and generic arguments.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("no implements found for the given trait and generic arguments.")]
pub struct TraitImplementsNotFoundError {
    /// The reference to the trait symbol.
    pub trait_ref: TraitRef,

    /// The generic arguments supplied to the trait.
    pub local_substitution: LocalSubstitution,
}

/// The trait associated type in the trait associated bound got resolved to a type that does not
/// satisfy the bound.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssociatedTypeBoundError {
    /// The trait associated type in the trait associated bound.
    pub formal_trait_associated_ty: ty::TraitAssociated,

    /// The type of the [`Self::formal_trait_associated_ty`] that got resolved.
    pub resolved_trait_associated_ty: ty::Type,

    /// The type that [`Self::resolved_trait_associated_ty`] must equal to.
    pub bound: ty::Type,
}

/// The trait associated constant in the trait associated bound got resolved to a constant that
/// does not satisfy the bound.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssociatedConstantBoundError {
    /// The trait associated constant in the trait associated bound.
    pub formal_trait_associated_constant: constant::TraitAssociated,

    /// The constant of the [`Self::formal_trait_associated_constant`] that got resolved.
    pub resolved_trait_associated_constant: constant::Constant,

    /// The constant that [`Self::resolved_trait_associated_constant`] must equal to.
    pub bound: constant::Constant,
}

/// Afters substitutes the [`ConstantTypeOperand`] the type is clearly not usable as a type of a
/// constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantTypeBoundError {
    /// The constant type operand that got substituted.
    pub formal_constant_type_operand: ConstantTypeOperand,

    /// The type of the [`Self::formal_constant_type_operand`] that got substituted.
    pub substituted_type: ty::Type,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[allow(missing_docs)]
pub enum ConstraintSubstitutionError {
    #[error("The given arguments are invalid.")]
    InvalidArguments,

    #[error(
        "trait resolved into an implements that was incomplete and thus cannot access to an \
         appropriate associated type/constant."
    )]
    IncompleteImplements,

    #[error("trait resolution occurred during substitution, but no implements was found.")]
    TraitImplementsNotFound(#[from] TraitImplementsNotFoundError),
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[allow(missing_docs)]
pub enum SubstitutionError {
    #[error("The given arguments are invalid.")]
    InvalidArguments,

    #[error(
        "trait resolved into an implements that was incomplete and thus cannot access to an \
         appropriate associated type/constant."
    )]
    IncompleteImplements,

    #[error("trait resolution occurred during substitution, but no implements was found.")]
    TraitImplementsNotFound(#[from] TraitImplementsNotFoundError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, thiserror::Error)]
#[allow(missing_docs)]
pub enum TraitResolutionError {
    #[error("The given arguments are invalid.")]
    InvalidArguments,

    #[error("no implements found for the given trait and generic arguments.")]
    NotFound,

    #[error("trait resolved into a negative implements.")]
    Negative,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitResolution {
    implements_ref: ImplementsRef,
    deduced_substitution: LocalSubstitution,
}

impl Table {
    /// Transforms the given [`Constraints`] to a new [`Constraints`] with the given substitution.
    ///
    /// # Errors
    ///  
    /// - [`ConstraintSubstitutionError::InvalidArguments`] if the given arguments are invalid.
    /// - [`ConstraintSubstitutionError::IncompleteImplements`] if the trait resolved into an
    ///   [`ImplementsRef`] that was incomplete and thus cannot access to an appropriate associated
    ///   type/constant.
    /// - [`TraitImplementsNotFoundError`] if trait resolution occurred during substitution, but no
    ///  [`ImplementsRef`] was found.
    pub fn substitute_constraint(
        &self,
        constraint: &Constraints,
        substitution: &Substitution,
    ) -> Result<Constraints, ConstraintSubstitutionError> {
        todo!()
    }

    /// Performs a substitution/mapping on the given [`LocalSubstitution`].
    #[allow(clippy::missing_errors_doc)]
    pub fn substitute_local_substitution(
        &self,
        source: LocalSubstitution,
        substitution: &Substitution,
        associated_bounds: &AssociatedBounds,
    ) -> Result<LocalSubstitution, SubstitutionError> {
        let mut type_substitutions = Vec::with_capacity(source.types.len());
        let mut constant_substitutions = Vec::with_capacity(source.constants.len());
        let mut lifetime_substitutions = Vec::with_capacity(source.lifetimes.len());

        for ty in source.types {
            type_substitutions.push(self.substitute_type(ty, substitution, associated_bounds)?);
        }

        for constant in source.constants {
            constant_substitutions.push(self.substitute_constant(
                constant,
                substitution,
                associated_bounds,
            )?);
        }

        for lifetime in source.lifetimes {
            lifetime_substitutions.push(
                lifetime
                    .into_parameter()
                    .ok()
                    .and_then(|x| substitution.lifetime_substitutions.get(&x).copied())
                    .unwrap_or(lifetime),
            );
        }

        Ok(LocalSubstitution {
            types: type_substitutions,
            constants: constant_substitutions,
            lifetimes: lifetime_substitutions,
        })
    }

    /// Performs a substitution/mapping on the given [`constant::Constant`].
    #[allow(clippy::too_many_lines, clippy::missing_errors_doc)]
    pub fn substitute_constant(
        &self,
        constant: constant::Constant,
        substitution: &Substitution,
        associated_bounds: &AssociatedBounds,
    ) -> Result<constant::Constant, SubstitutionError> {
        match constant {
            normal @ constant::Constant::Primitive(..) => Ok(normal),
            constant::Constant::Struct(struct_val) => {
                let transformed_struct_ty = self.substitute_type(
                    ty::Type::Struct(struct_val.struct_ty),
                    substitution,
                    associated_bounds,
                )?;

                let mut fields = Vec::with_capacity(struct_val.fields.len());

                for field in struct_val.fields {
                    fields.push(self.substitute_constant(
                        field,
                        substitution,
                        associated_bounds,
                    )?);
                }

                Ok(constant::Constant::Struct(constant::Struct {
                    struct_ty: transformed_struct_ty
                        .into_struct()
                        .expect("should be some of struct"),
                    fields,
                }))
            }
            constant::Constant::Enum(enum_val) => {
                let transformed_enum_ty = self.substitute_type(
                    ty::Type::Enum(enum_val.enum_ty),
                    substitution,
                    associated_bounds,
                )?;

                let value = if let Some(value) = enum_val.value {
                    Some(Box::new(self.substitute_constant(
                        *value,
                        substitution,
                        associated_bounds,
                    )?))
                } else {
                    None
                };

                Ok(constant::Constant::Enum(constant::Enum {
                    enum_ty: transformed_enum_ty
                        .into_enum()
                        .expect("should be some of enum"),
                    local_variant_ref: enum_val.local_variant_ref,
                    value,
                }))
            }
            constant::Constant::Array(array_val) => {
                let mut elements = Vec::with_capacity(array_val.elements.len());

                let element_ty =
                    self.substitute_type(array_val.element_ty, substitution, associated_bounds)?;

                for element in array_val.elements {
                    elements.push(self.substitute_constant(
                        element,
                        substitution,
                        associated_bounds,
                    )?);
                }

                Ok(constant::Constant::Array(constant::Array {
                    element_ty,
                    elements,
                }))
            }
            constant::Constant::Tuple(tuple_val) => {
                let mut elements = Vec::with_capacity(tuple_val.elements.len());

                for element in tuple_val.elements {
                    match element {
                        constant::TupleElement::Regular(regular) => {
                            elements.push(constant::TupleElement::Regular(
                                self.substitute_constant(regular, substitution, associated_bounds)?,
                            ));
                        }
                        constant::TupleElement::Unpacked(constant) => {
                            let transformed = match constant {
                                constant::Unpacked::Parameter(ty) => self.substitute_constant(
                                    constant::Constant::Parameter(ty),
                                    substitution,
                                    associated_bounds,
                                ),
                                constant::Unpacked::TraitAssociated(ty) => self
                                    .substitute_constant(
                                        constant::Constant::TraitAssociated(ty),
                                        substitution,
                                        associated_bounds,
                                    ),
                            }?;

                            match transformed {
                                constant::Constant::Parameter(parameter) => {
                                    elements.push(constant::TupleElement::Unpacked(
                                        constant::Unpacked::Parameter(parameter),
                                    ));
                                }
                                constant::Constant::TraitAssociated(trait_associated) => {
                                    elements.push(constant::TupleElement::Unpacked(
                                        constant::Unpacked::TraitAssociated(trait_associated),
                                    ));
                                }
                                constant::Constant::Tuple(tuple) => {
                                    elements.extend(tuple.elements.into_iter());
                                }
                                regular => {
                                    elements.push(constant::TupleElement::Regular(regular));
                                }
                            }
                        }
                    }
                }

                Ok(constant::Constant::Tuple(constant::Tuple { elements }))
            }
            constant::Constant::Parameter(parameter) => substitution
                .constant_substitutions
                .get(&parameter)
                .cloned()
                .map_or_else(
                    || Ok(constant::Constant::Parameter(parameter)),
                    |x| self.substitute_constant(x, substitution, associated_bounds),
                ),
            constant::Constant::TraitAssociated(trait_associated_val) => {
                // try to find the associated type bounds early
                if let Some(transformed) = associated_bounds
                    .associated_constant_bounds
                    .get(&trait_associated_val)
                    .cloned()
                {
                    return self.substitute_constant(transformed, substitution, associated_bounds);
                }

                let new_trait_associated_val = constant::TraitAssociated {
                    trait_constant_ref: trait_associated_val.trait_constant_ref,
                    trait_substitution: self.substitute_local_substitution(
                        trait_associated_val.trait_substitution,
                        substitution,
                        associated_bounds,
                    )?,
                };

                // early return
                if !new_trait_associated_val.trait_substitution.is_concrete() {
                    if let Some(transformed) = associated_bounds
                        .associated_constant_bounds
                        .get(&new_trait_associated_val)
                        .cloned()
                    {
                        return self.substitute_constant(
                            transformed,
                            substitution,
                            associated_bounds,
                        );
                    }

                    return Ok(constant::Constant::TraitAssociated(
                        new_trait_associated_val,
                    ));
                }

                let implements = match self.resolve_trait_implements(
                    new_trait_associated_val.trait_constant_ref.trait_ref,
                    &new_trait_associated_val.trait_substitution,
                    associated_bounds,
                ) {
                    Ok(ok) => ok,
                    Err(err) => match err {
                        TraitResolutionError::InvalidArguments => {
                            return Err(SubstitutionError::InvalidArguments);
                        }
                        TraitResolutionError::NotFound | TraitResolutionError::Negative => {
                            return Err(SubstitutionError::TraitImplementsNotFound(
                                TraitImplementsNotFoundError {
                                    trait_ref: new_trait_associated_val
                                        .trait_constant_ref
                                        .trait_ref,
                                    local_substitution: new_trait_associated_val.trait_substitution,
                                },
                            ));
                        }
                    },
                };

                let constant = {
                    let implements = &self.traits()
                        [new_trait_associated_val.trait_constant_ref.trait_ref.0]
                        .implements[implements.implements_ref.local_ref.0];

                    let local_implements_constant_ref = implements
                        .implements_constant_ref_by_trait_constant_ref
                        .get(&new_trait_associated_val.trait_constant_ref.local_ref)
                        .copied()
                        .ok_or(SubstitutionError::IncompleteImplements)?;

                    implements.constants[local_implements_constant_ref.0]
                        .constant
                        .clone()
                };

                Ok(self.substitute_constant(
                    constant,
                    &Substitution::from_local(
                        &dynements.deduced_substitution,
                        GenericItemRef::Implements(implements.implements_ref),
                    ),
                    associated_bounds,
                )?)
            }
        }
    }

    /// Performs a substitution/mapping on the given [`ty::Type`].
    #[allow(
        clippy::too_many_lines,
        clippy::too_many_arguments,
        clippy::missing_errors_doc
    )]
    pub fn substitute_type(
        &self,
        source: ty::Type,
        substitution: &Substitution,
        associated_bounds: &AssociatedBounds,
    ) -> Result<ty::Type, SubstitutionError> {
        match source {
            ty::Type::Enum(enum_ty) => {
                let transformed_substitution = self.substitute_local_substitution(
                    enum_ty.substitution,
                    substitution,
                    associated_bounds,
                )?;

                Ok(ty::Type::Enum(ty::Enum {
                    enum_ref: enum_ty.enum_ref,
                    substitution: transformed_substitution,
                }))
            }
            ty::Type::Reference(reference_ty) => {
                let transformed_ty =
                    self.substitute_type(*reference_ty.operand, substitution, associated_bounds)?;

                let lifetime = reference_ty
                    .lifetime
                    .into_parameter()
                    .ok()
                    .and_then(|x| substitution.lifetime_substitutions.get(&x).copied())
                    .unwrap_or(reference_ty.lifetime);

                Ok(ty::Type::Reference(ty::Reference {
                    qualifier: reference_ty.qualifier,
                    lifetime,
                    operand: Box::new(transformed_ty),
                }))
            }
            ty::Type::Pointer(pointer_ty) => {
                let transformed_ty =
                    self.substitute_type(*pointer_ty.operand, substitution, associated_bounds)?;

                Ok(ty::Type::Pointer(ty::Pointer {
                    qualifier: pointer_ty.qualifier,
                    operand: Box::new(transformed_ty),
                }))
            }
            ty::Type::Struct(struct_ty) => {
                let transformed_substitution = self.substitute_local_substitution(
                    struct_ty.substitution,
                    substitution,
                    associated_bounds,
                )?;

                Ok(ty::Type::Struct(ty::Struct {
                    struct_ref: struct_ty.struct_ref,
                    substitution: transformed_substitution,
                }))
            }
            ty::Type::Tuple(tuple) => {
                let mut elements = Vec::with_capacity(tuple.elements.len());

                for ty in tuple.elements {
                    match ty {
                        ty::TupleElement::Regular(regular) => {
                            elements.push(ty::TupleElement::Regular(self.substitute_type(
                                regular,
                                substitution,
                                associated_bounds,
                            )?));
                        }
                        ty::TupleElement::Unpacked(ty) => {
                            let transformed = match ty {
                                ty::Unpacked::Parameter(ty) => self.substitute_type(
                                    ty::Type::Parameter(ty),
                                    substitution,
                                    associated_bounds,
                                ),
                                ty::Unpacked::TraitAssociated(ty) => self.substitute_type(
                                    ty::Type::TraitAssociated(ty),
                                    substitution,
                                    associated_bounds,
                                ),
                            }?;

                            match transformed {
                                ty::Type::Parameter(parameter) => {
                                    elements.push(ty::TupleElement::Unpacked(
                                        ty::Unpacked::Parameter(parameter),
                                    ));
                                }
                                ty::Type::TraitAssociated(trait_associated) => {
                                    elements.push(ty::TupleElement::Unpacked(
                                        ty::Unpacked::TraitAssociated(trait_associated),
                                    ));
                                }
                                ty::Type::Tuple(tuple) => {
                                    elements.extend(tuple.elements.into_iter());
                                }
                                regular => {
                                    elements.push(ty::TupleElement::Regular(regular));
                                }
                            }
                        }
                    }
                }

                Ok(ty::Type::Tuple(ty::Tuple { elements }))
            }
            ty::Type::Parameter(ty_parameter) => substitution
                .type_substitutions
                .get(&ty_parameter)
                .cloned()
                .map_or_else(
                    || Ok(ty::Type::Parameter(ty_parameter)),
                    |x| self.substitute_type(x, substitution, associated_bounds),
                ),
            ty::Type::Primitive(primitive_type) => Ok(ty::Type::Primitive(primitive_type)),
            ty::Type::TraitAssociated(trait_associated_ty) => {
                // try to find the associated type bounds early
                if let Some(transformed) = associated_bounds
                    .associated_type_bounds
                    .get(&trait_associated_ty)
                    .cloned()
                {
                    return self.substitute_type(transformed, substitution, associated_bounds);
                }

                let new_trait_associated_ty = ty::TraitAssociated {
                    trait_type_ref: trait_associated_ty.trait_type_ref,
                    trait_substitution: self.substitute_local_substitution(
                        trait_associated_ty.trait_substitution,
                        substitution,
                        associated_bounds,
                    )?,
                    associated_substitution: self.substitute_local_substitution(
                        trait_associated_ty.associated_substitution,
                        substitution,
                        associated_bounds,
                    )?,
                };

                // early return
                if !new_trait_associated_ty.trait_substitution.is_concrete() {
                    if let Some(transformed) = associated_bounds
                        .associated_type_bounds
                        .get(&new_trait_associated_ty)
                        .cloned()
                    {
                        return self.substitute_type(transformed, substitution, associated_bounds);
                    }

                    return Ok(ty::Type::TraitAssociated(new_trait_associated_ty));
                }

                let implements = match self.resolve_trait_implements(
                    new_trait_associated_ty.trait_type_ref.trait_ref,
                    &new_trait_associated_ty.trait_substitution,
                    associated_bounds,
                ) {
                    Ok(ok) => ok,
                    Err(err) => match err {
                        TraitResolutionError::InvalidArguments => {
                            return Err(SubstitutionError::InvalidArguments);
                        }
                        TraitResolutionError::NotFound | TraitResolutionError::Negative => {
                            return Err(SubstitutionError::TraitImplementsNotFound(
                                TraitImplementsNotFoundError {
                                    trait_ref: new_trait_associated_ty.trait_type_ref.trait_ref,
                                    local_substitution: new_trait_associated_ty.trait_substitution,
                                },
                            ));
                        }
                    },
                };

                let (type_alias, local_implements_type_ref) = {
                    let implements = &self.traits()
                        [new_trait_associated_ty.trait_type_ref.trait_ref.0]
                        .implements[implements.implements_ref.local_ref.0];

                    let local_implements_type_ref = implements
                        .implements_type_ref_by_trait_type_ref
                        .get(&new_trait_associated_ty.trait_type_ref.local_ref)
                        .copied()
                        .ok_or(SubstitutionError::IncompleteImplements)?;

                    (
                        implements.types[local_implements_type_ref.0].alias.clone(),
                        local_implements_type_ref,
                    )
                };

                Ok(self.substitute_type(
                    type_alias,
                    &Substitution::combine(
                        &Substitution::from_local(
                            &dynements.deduced_substitution,
                            GenericItemRef::Implements(implements.implements_ref),
                        ),
                        &Substitution::from_local(
                            &new_trait_associated_ty.associated_substitution,
                            GenericItemRef::ImplementsType(ImplementsTypeRef {
                                implements_ref: implements.implements_ref,
                                local_ref: local_implements_type_ref,
                            }),
                        ),
                    )
                    .unwrap(),
                    associated_bounds,
                )?)
            }
            ty::Type::Array(arrayy_ty) => {
                let transformed_element_ty =
                    self.substitute_type(*arrayy_ty.element_ty, substitution, associated_bounds)?;

                let transformed_size =
                    self.substitute_constant(*arrayy_ty.size, substitution, associated_bounds)?;

                Ok(ty::Type::Array(ty::Array {
                    element_ty: Box::new(transformed_element_ty),
                    size: Box::new(transformed_size),
                }))
            }
        }
    }
}

impl TraitBound {
    /// Checks if `self` trait bound is a subset or equivalent trait bound of the `source` trait
    /// bound.
    #[must_use]
    pub fn is_subset_or_equal(&self, source: &Self) -> bool {
        if !(self.trait_ref == source.trait_ref
            && self.type_substituions == source.type_substituions
            && self.constant_substituions == source.constant_substituions)
        {
            return false;
        }

        assert_eq!(
            self.lifetime_substituions.len(),
            source.lifetime_substituions.len()
        );

        let mut source_higher_ranked_lifetime_map: HashMap<
            HigherRankedLifetime,
            HigherRankedableLifetime,
        > = HashMap::new();

        for (target_lt, source_lt) in self
            .lifetime_substituions
            .iter()
            .copied()
            .zip(source.lifetime_substituions.iter().copied())
        {
            match (target_lt, source_lt) {
                // arbitrary lifetime check
                (
                    HigherRankedableLifetime::Regular(target),
                    HigherRankedableLifetime::Regular(source),
                ) => {
                    if target != source {
                        return false;
                    }
                }
                // maps the higher ranked trait bound of source to target lt
                (target_lt, HigherRankedableLifetime::HigherRanked(source)) => {
                    match source_higher_ranked_lifetime_map.entry(source) {
                        Entry::Occupied(existing) => {
                            if *existing.get() != target_lt {
                                return false;
                            }
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(target_lt);
                        }
                    }
                }
                _ => return false,
            }
        }

        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ImplementsKeys {
    Negative(usize),
    Positive(LocalImplementsRef),
}

impl Table {
    /// Resolves the given trait reference and local substitution to a [`TraitResolution`].
    ///
    /// # Errors
    ///
    /// - [`TraitResolutionError::InvalidArguments`] if the given arguments are invalid.
    pub fn resolve_trait_implements(
        &self,
        trait_ref: TraitRef,
        local_substitution: &LocalSubstitution,
        active_associated_bounds: &AssociatedBounds,
    ) -> Result<TraitResolution, TraitResolutionError> {
        if !local_substitution.is_concrete() {
            return Err(TraitResolutionError::InvalidArguments);
        }

        let trait_sym = self
            .traits()
            .get(trait_ref.0)
            .ok_or(TraitResolutionError::InvalidArguments)?;

        let positives = trait_sym
            .implements
            .iter()
            .enumerate()
            .map(|(index, implements)| {
                (
                    ImplementsKeys::Positive(LocalImplementsRef(index)),
                    &dynements.signature,
                )
            });

        let negatives = trait_sym.negative_implements.iter().enumerate().map(
            |(index, implements_signature)| (ImplementsKeys::Negative(index), implements_signature),
        );

        let mut implements_keys_by_deduced_substitution = HashMap::new();

        for (key, signature) in positives.chain(negatives) {
            match self.get_deduced_substitution(
                &signature.trait_substitution,
                local_substitution,
                active_associated_bounds,
            ) {
                Ok(ok) => {
                    assert!(implements_keys_by_deduced_substitution
                        .insert(key, ok)
                        .is_none());
                }
                Err(deduction::Error::NonUnifiable) => {
                    continue;
                }
                Err(deduction::Error::InvalidArguments) => {
                    return Err(TraitResolutionError::InvalidArguments);
                }
            }
        }

        todo!()
    }
}
