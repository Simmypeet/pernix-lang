//! Responsible for transforming semantic information via substitution or combination.

use std::collections::{HashMap, HashSet};

use super::{
    trait_resolution::{self, TraitSubstitution},
    Table,
};
use crate::{
    constant,
    symbol::{
        self, AssociatedBounds, GenericItemRef, HigherRankedableLifetime, ImplementsRef,
        ImplementsTypeRef, LifetimeBoundOperand, LocalSubstitution, Substitution, TraitBound,
    },
    ty::{self, ElidedLifetime, Lifetime},
};

/// Represents a resolved trait implementation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedImplements {
    /// The resolved implements reference.
    pub implements_ref: ImplementsRef,

    /// The deduced substitution of the resolved implements.
    pub deduced_substituion: LocalSubstitution,
}

/// Contains the result of semantic information transformation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Transformed<T> {
    /// The transformed semantic information.
    pub transformed: T,

    /// List of trait implementations that are resolved during the transformation.
    ///
    /// To validate the correctness of the transformation, the caller can check for each bound of
    /// these resolved trait implementations.
    pub resolved_implements: Vec<ResolvedImplements>,
}

impl<T> Transformed<T> {
    /// Extract the [`Self::transformed`] field out and cascade the [`Self::resolved_implements`] to
    /// the given vector.
    fn extract(self, resolved_implements: &mut Vec<ResolvedImplements>) -> T {
        resolved_implements.extend(self.resolved_implements);

        self.transformed
    }
}

fn try_transform_higherankedable_lifetime(
    lifetime_substitution: Vec<HigherRankedableLifetime>,
) -> Result<Vec<Lifetime>, Vec<HigherRankedableLifetime>> {
    if lifetime_substitution
        .iter()
        .all(HigherRankedableLifetime::is_regular)
    {
        Ok(lifetime_substitution
            .into_iter()
            .map(|x| x.into_regular().unwrap())
            .collect())
    } else {
        Err(lifetime_substitution)
    }
}

/// Represents the result of [`symbol::Bounds`] transformation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bounds {
    pub transformed: symbol::Bounds,
    pub resolved_implements: Vec<ResolvedImplements>,

    /// The lifetime bounds that has elided lifetimes as the operand.
    ///
    /// Having elided lifetimes as the operand will always fail to pass the bounds check,
    /// therefore, they are separated from the [`symbol::Bounds::lifetime_bounds`] field.
    pub elided_lifetime_bounds: HashMap<ElidedLifetime, HashSet<Lifetime>>,
}

/// Represents an error that occurs during the transformation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum SubstitutionError {
    #[error("trait resolution occurs during transformation and failed to resolve the trait")]
    FailedTraitResolution(trait_resolution::TraitSubstitution),

    #[error("trait resolved into an implements that was incomplete")]
    IncompleteImplements,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
pub enum CombinationError {
    #[error("trait associated type bound appears more than once")]
    AssociatedTypeBoundConflict(ty::TraitAssociated),

    #[error("trait associated constant bound appears more than once")]
    AssociatedConstantBoundConflict(constant::TraitAssociated),

    #[error("trait associated type bound always maps to itself")]
    AssociatedTypeBoundSelfReference(ty::TraitAssociated),

    #[error("trait associated constant bound always maps to itself")]
    AssociatedConstantBoundSelfReference(constant::TraitAssociated),

    #[error("trait associated type operand resolved into a type that doesn't match the bound")]
    AssociatedTypeBoundAndOperandMismatch { operand: ty::Type, bound: ty::Type },

    #[error(
        "trait associated constant operand resolved into a constant that doesn't match the bound"
    )]
    AssociatedConstantBoundAndOperandMismatch {
        operand: constant::Constant,
        bound: constant::Constant,
    },

    #[error("{0}")]
    Substitution(#[from] SubstitutionError),
}

impl Table {
    /// Combines two [`AssociatedBounds`] into one.
    #[allow(clippy::missing_errors_doc)]
    pub fn combine_associated_bounds(
        &self,
        first: &AssociatedBounds,
        second: &AssociatedBounds,
    ) -> Result<Transformed<AssociatedBounds>, CombinationError> {
        let mut associated_bounds = first.clone();

        for (trait_associated, bounds) in &second.associated_type_bounds {
            match associated_bounds
                .associated_type_bounds
                .entry(trait_associated.clone())
            {
                std::collections::hash_map::Entry::Occupied(entry) => {
                    return Err(CombinationError::AssociatedTypeBoundConflict(
                        entry.key().clone(),
                    ));
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(bounds.clone());
                }
            }
        }

        for (trait_associated, bounds) in &second.associated_constant_bounds {
            match associated_bounds
                .associated_constant_bounds
                .entry(trait_associated.clone())
            {
                std::collections::hash_map::Entry::Occupied(entry) => {
                    return Err(CombinationError::AssociatedConstantBoundConflict(
                        entry.key().clone(),
                    ));
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(bounds.clone());
                }
            }
        }

        self.flatten_associated_bounds(associated_bounds)
    }

    fn flatten_associated_bounds(
        &self,
        mut associated_bounds: AssociatedBounds,
    ) -> Result<Transformed<AssociatedBounds>, CombinationError> {
        let mut resolved_implements = Vec::new();
        loop {
            let result = self.flatten_associated_bounds_internal(associated_bounds)?;
            associated_bounds = result.0.extract(&mut resolved_implements);

            if !result.1 {
                break;
            }
        }

        Ok(Transformed {
            transformed: associated_bounds,
            resolved_implements,
        })
    }

    #[allow(clippy::too_many_lines)]
    fn flatten_associated_bounds_internal(
        &self,
        mut associated_bounds: AssociatedBounds,
    ) -> Result<(Transformed<AssociatedBounds>, bool), CombinationError> {
        let mut resolved_implements = Vec::new();
        let mut transformed = false;

        let associated_types = associated_bounds
            .associated_type_bounds
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        let associated_constants = associated_bounds
            .associated_constant_bounds
            .keys()
            .cloned()
            .collect::<Vec<_>>();

        for original_associatedd_ty in associated_types {
            let original_bound_ty = associated_bounds
                .associated_type_bounds
                .remove(&original_associatedd_ty)
                .expect("should exist");

            let transformed_bound_ty = self
                .transform_type(
                    original_bound_ty.clone(),
                    &Substitution::default(),
                    &associated_bounds,
                )?
                .extract(&mut resolved_implements);
            let transformed_associated_ty = self
                .transform_type(
                    ty::Type::TraitAssociated(original_associatedd_ty.clone()),
                    &Substitution::default(),
                    &associated_bounds,
                )?
                .extract(&mut resolved_implements);

            match transformed_associated_ty {
                transformed_associated_ty @ ty::Type::TraitAssociated(..) => {
                    if transformed_associated_ty.is_subset_or_equal(&transformed_bound_ty) {
                        return Err(CombinationError::AssociatedTypeBoundSelfReference(
                            transformed_associated_ty.into_trait_associated().unwrap(),
                        ));
                    }

                    let transformed_associated_ty =
                        transformed_associated_ty.into_trait_associated().unwrap();

                    transformed = transformed_associated_ty != original_associatedd_ty
                        || transformed_bound_ty != original_bound_ty;

                    match associated_bounds
                        .associated_type_bounds
                        .entry(transformed_associated_ty.clone())
                    {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            return Err(CombinationError::AssociatedTypeBoundConflict(
                                entry.key().clone(),
                            ));
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert(transformed_bound_ty);
                        }
                    }
                }
                transformed_associated_ty => {
                    if transformed_associated_ty != transformed_bound_ty {
                        return Err(CombinationError::AssociatedTypeBoundAndOperandMismatch {
                            operand: transformed_associated_ty,
                            bound: transformed_bound_ty,
                        });
                    }
                }
            }
        }

        for original_associated_constant in associated_constants {
            let original_bound_constant = associated_bounds
                .associated_constant_bounds
                .remove(&original_associated_constant)
                .expect("should exist");

            let transformed_bound_constant = self
                .transform_constant(
                    original_bound_constant.clone(),
                    &Substitution::default(),
                    &associated_bounds,
                )?
                .extract(&mut resolved_implements);
            let transformed_associated_constant = self
                .transform_constant(
                    constant::Constant::TraitAssociated(original_associated_constant.clone()),
                    &Substitution::default(),
                    &associated_bounds,
                )?
                .extract(&mut resolved_implements);

            match transformed_associated_constant {
                transformed_associated_constant @ constant::Constant::TraitAssociated(..) => {
                    if transformed_associated_constant
                        .is_subset_or_equal(&transformed_bound_constant)
                    {
                        return Err(CombinationError::AssociatedConstantBoundSelfReference(
                            transformed_associated_constant
                                .into_trait_associated()
                                .unwrap(),
                        ));
                    }

                    let transformed_associated_constant = transformed_associated_constant
                        .into_trait_associated()
                        .unwrap();

                    transformed = transformed_associated_constant != original_associated_constant
                        || transformed_bound_constant != original_bound_constant;

                    match associated_bounds
                        .associated_constant_bounds
                        .entry(transformed_associated_constant.clone())
                    {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            return Err(CombinationError::AssociatedConstantBoundConflict(
                                entry.key().clone(),
                            ));
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert(transformed_bound_constant);
                        }
                    }
                }
                transformed_associated_constant => {
                    if transformed_associated_constant != transformed_bound_constant {
                        return Err(
                            CombinationError::AssociatedConstantBoundAndOperandMismatch {
                                operand: transformed_associated_constant,
                                bound: transformed_bound_constant,
                            },
                        );
                    }
                }
            }
        }

        Ok((
            Transformed {
                transformed: associated_bounds,
                resolved_implements,
            },
            transformed,
        ))
    }

    /// Performs a substitution/mapping on the given [`symbol::Bounds`].
    #[allow(clippy::missing_errors_doc, clippy::too_many_lines)]
    pub fn transform_bounds(
        &self,
        source: symbol::Bounds,
        substitution: &Substitution,
        associated_bounds: &AssociatedBounds,
    ) -> Result<Bounds, SubstitutionError> {
        let mut resolved_implements = Vec::new();

        let mut lifetime_bounds: HashMap<LifetimeBoundOperand, HashSet<Lifetime>> =
            HashMap::with_capacity(source.lifetime_bounds.len());
        let mut trait_bounds = HashMap::with_capacity(source.trait_bounds.len());
        let mut elided_lifetime_bounds: HashMap<ElidedLifetime, HashSet<Lifetime>> = HashMap::new();

        for (operand, source_bounds) in source.lifetime_bounds {
            let transformed_operand = match operand {
                LifetimeBoundOperand::LifetimeParameter(lifetime) => {
                    let lifetime = substitution
                        .lifetime_substitutions
                        .get(&lifetime)
                        .copied()
                        .unwrap_or(Lifetime::Parameter(lifetime));

                    match lifetime {
                        // 'static lifetime is always valid
                        Lifetime::Static => continue,
                        Lifetime::Parameter(parameter) => {
                            LifetimeBoundOperand::LifetimeParameter(parameter)
                        }
                        Lifetime::Elided(elided) => {
                            let elided_bounds = elided_lifetime_bounds.entry(elided).or_default();

                            elided_bounds.extend(source_bounds.into_iter().map(|x| {
                                x.into_parameter()
                                    .ok()
                                    .and_then(|x| substitution.lifetime_substitutions.get(&x))
                                    .copied()
                                    .unwrap_or(x)
                            }));

                            continue;
                        }
                    }
                }
                LifetimeBoundOperand::Type(ty) => LifetimeBoundOperand::Type(
                    self.transform_type(ty, substitution, associated_bounds)?
                        .extract(&mut resolved_implements),
                ),
            };

            let bounds = lifetime_bounds.entry(transformed_operand).or_default();

            bounds.extend(source_bounds.into_iter().map(|x| {
                x.into_parameter()
                    .ok()
                    .and_then(|x| substitution.lifetime_substitutions.get(&x))
                    .copied()
                    .unwrap_or(x)
            }));
        }

        for (source_trait_bound, source_is_const) in source.trait_bounds {
            let mut transformed_lifetime_substitution =
                Vec::with_capacity(source_trait_bound.lifetime_substituions.len());
            let mut transformed_type_substitution =
                Vec::with_capacity(source_trait_bound.type_substituions.len());
            let mut transformed_constant_substitution =
                Vec::with_capacity(source_trait_bound.constant_substituions.len());

            // transform the lifetime substitution
            for lifetime in source_trait_bound.lifetime_substituions {
                match lifetime {
                    HigherRankedableLifetime::Regular(regular) => {
                        transformed_lifetime_substitution.push(HigherRankedableLifetime::Regular(
                            regular
                                .into_parameter()
                                .ok()
                                .and_then(|x| substitution.lifetime_substitutions.get(&x).copied())
                                .unwrap_or(regular),
                        ));
                    }
                    higher_ranked @ HigherRankedableLifetime::HigherRanked(..) => {
                        transformed_lifetime_substitution.push(higher_ranked);
                    }
                }
            }

            // transform the type substitution
            for ty in source_trait_bound.type_substituions {
                transformed_type_substitution.push(
                    self.transform_type(ty, substitution, associated_bounds)?
                        .extract(&mut resolved_implements),
                );
            }

            // transform the constant substitution
            for constant in source_trait_bound.constant_substituions {
                transformed_constant_substitution.push(
                    self.transform_constant(constant, substitution, associated_bounds)?
                        .extract(&mut resolved_implements),
                );
            }

            let trait_bound =
                match try_transform_higherankedable_lifetime(transformed_lifetime_substitution) {
                    Ok(transformed) => {
                        let local_substitution = LocalSubstitution {
                            lifetimes: transformed,
                            types: transformed_type_substitution,
                            constants: transformed_constant_substitution,
                        };

                        // if concrete, try to resolve the trait
                        if local_substitution.is_concrete() {
                            let trait_substitution = TraitSubstitution {
                                trait_ref: source_trait_bound.trait_ref,
                                substitution: local_substitution,
                            };

                            let Some(..) = self
                                .resolve_trait_implements(&trait_substitution)
                                .expect("should've been a valid substitution")
                            else {
                                return Err(SubstitutionError::FailedTraitResolution(
                                    trait_substitution,
                                ));
                            };
                            continue;
                        }

                        TraitBound {
                            trait_ref: source_trait_bound.trait_ref,
                            type_substituions: local_substitution.types,
                            lifetime_substituions: local_substitution
                                .lifetimes
                                .into_iter()
                                .map(HigherRankedableLifetime::Regular)
                                .collect(),
                            constant_substituions: local_substitution.constants,
                        }
                    }
                    Err(transformed) => TraitBound {
                        trait_ref: source_trait_bound.trait_ref,
                        type_substituions: transformed_type_substitution,
                        lifetime_substituions: transformed,
                        constant_substituions: transformed_constant_substitution,
                    },
                };

            symbol::Bounds::try_add_trait_bound(&mut trait_bounds, trait_bound, source_is_const);
        }

        Ok(Bounds {
            transformed: symbol::Bounds {
                lifetime_bounds,
                trait_bounds,
            },
            elided_lifetime_bounds,
            resolved_implements,
        })
    }

    /// Performs a substitution/mapping on the given [`ty::Type`].
    #[allow(
        clippy::too_many_lines,
        clippy::too_many_arguments,
        clippy::missing_errors_doc
    )]
    pub fn transform_type(
        &self,
        source: ty::Type,
        substitution: &Substitution,
        associated_bounds: &AssociatedBounds,
    ) -> Result<Transformed<ty::Type>, SubstitutionError> {
        match source {
            ty::Type::Enum(enum_ty) => {
                let transformed_substitution = self.transform_local_substitution(
                    enum_ty.substitution,
                    substitution,
                    associated_bounds,
                )?;

                Ok(Transformed {
                    transformed: ty::Type::Enum(ty::Enum {
                        enum_ref: enum_ty.enum_ref,
                        substitution: transformed_substitution.transformed,
                    }),
                    resolved_implements: transformed_substitution.resolved_implements,
                })
            }
            ty::Type::Reference(reference_ty) => {
                let transformed_ty =
                    self.transform_type(*reference_ty.operand, substitution, associated_bounds)?;

                let lifetime = reference_ty
                    .lifetime
                    .into_parameter()
                    .ok()
                    .and_then(|x| substitution.lifetime_substitutions.get(&x).copied())
                    .unwrap_or(reference_ty.lifetime);

                Ok(Transformed {
                    transformed: ty::Type::Reference(ty::Reference {
                        qualifier: reference_ty.qualifier,
                        lifetime,
                        operand: Box::new(transformed_ty.transformed),
                    }),
                    resolved_implements: transformed_ty.resolved_implements,
                })
            }
            ty::Type::Pointer(pointer_ty) => {
                let transformed_ty =
                    self.transform_type(*pointer_ty.operand, substitution, associated_bounds)?;

                Ok(Transformed {
                    transformed: ty::Type::Pointer(ty::Pointer {
                        qualifier: pointer_ty.qualifier,
                        operand: Box::new(transformed_ty.transformed),
                    }),
                    resolved_implements: transformed_ty.resolved_implements,
                })
            }
            ty::Type::Struct(struct_ty) => {
                let transformed_substitution = self.transform_local_substitution(
                    struct_ty.substitution,
                    substitution,
                    associated_bounds,
                )?;

                Ok(Transformed {
                    transformed: ty::Type::Struct(ty::Struct {
                        struct_ref: struct_ty.struct_ref,
                        substitution: transformed_substitution.transformed,
                    }),
                    resolved_implements: transformed_substitution.resolved_implements,
                })
            }
            ty::Type::Tuple(tuple) => {
                let mut resolved_implements = Vec::new();
                let mut elements = Vec::with_capacity(tuple.elements.len());

                for ty in tuple.elements {
                    match ty {
                        ty::TupleElement::Regular(regular) => {
                            elements.push(ty::TupleElement::Regular(
                                self.transform_type(regular, substitution, associated_bounds)?
                                    .extract(&mut resolved_implements),
                            ));
                        }
                        ty::TupleElement::Unpacked(ty) => {
                            let transformed = match ty {
                                ty::Unpacked::Parameter(ty) => self.transform_type(
                                    ty::Type::Parameter(ty),
                                    substitution,
                                    associated_bounds,
                                ),
                                ty::Unpacked::TraitAssociated(ty) => self.transform_type(
                                    ty::Type::TraitAssociated(ty),
                                    substitution,
                                    associated_bounds,
                                ),
                            }?
                            .extract(&mut resolved_implements);

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

                Ok(Transformed {
                    transformed: ty::Type::Tuple(ty::Tuple { elements }),
                    resolved_implements,
                })
            }
            ty::Type::Parameter(ty_parameter) => substitution
                .type_substitutions
                .get(&ty_parameter)
                .cloned()
                .map_or_else(
                    || {
                        Ok(Transformed {
                            transformed: ty::Type::Parameter(ty_parameter),
                            resolved_implements: Vec::new(),
                        })
                    },
                    |x| self.transform_type(x, substitution, associated_bounds),
                ),
            ty::Type::Primitive(primitive_type) => Ok(Transformed {
                transformed: ty::Type::Primitive(primitive_type),
                resolved_implements: Vec::new(),
            }),
            ty::Type::TraitAssociated(trait_associated_ty) => {
                // try to find the associated type bounds early
                if let Some(transformed) = associated_bounds
                    .associated_type_bounds
                    .get(&trait_associated_ty)
                    .cloned()
                {
                    return self.transform_type(transformed, substitution, associated_bounds);
                }

                let mut resolved_implements = Vec::new();

                let new_trait_associated_ty = ty::TraitAssociated {
                    trait_type_ref: trait_associated_ty.trait_type_ref,
                    trait_substitution: self
                        .transform_local_substitution(
                            trait_associated_ty.trait_substitution,
                            substitution,
                            associated_bounds,
                        )?
                        .extract(&mut resolved_implements),
                    associated_substitution: self
                        .transform_local_substitution(
                            trait_associated_ty.associated_substitution,
                            substitution,
                            associated_bounds,
                        )?
                        .extract(&mut resolved_implements),
                };

                // early return
                if !new_trait_associated_ty.trait_substitution.is_concrete() {
                    if let Some(transformed) = associated_bounds
                        .associated_type_bounds
                        .get(&new_trait_associated_ty)
                        .cloned()
                    {
                        return Ok(Transformed {
                            transformed: self
                                .transform_type(transformed, substitution, associated_bounds)?
                                .extract(&mut resolved_implements),
                            resolved_implements,
                        });
                    }

                    return Ok(Transformed {
                        transformed: ty::Type::TraitAssociated(new_trait_associated_ty),
                        resolved_implements,
                    });
                }

                // perform trait resolution
                let trait_substitution = TraitSubstitution {
                    trait_ref: new_trait_associated_ty.trait_type_ref.trait_ref,
                    substitution: new_trait_associated_ty.trait_substitution,
                };
                let Some(implements) = self
                    .resolve_trait_implements(&trait_substitution)
                    .expect("already is a concrete substitution")
                // failed to resolve the trait
                else {
                    return Err(SubstitutionError::FailedTraitResolution(trait_substitution));
                };

                let implements_generic_item_ref = GenericItemRef::Implements(ImplementsRef {
                    trait_ref: trait_associated_ty.trait_type_ref.trait_ref,
                    local_ref: implements.local_implements_ref,
                });

                let (type_alias, local_implements_type_ref) = {
                    let implements = &self.traits
                        [new_trait_associated_ty.trait_type_ref.trait_ref.0]
                        .implements[implements.local_implements_ref.0];

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

                Ok(Transformed {
                    transformed: self
                        .transform_type(
                            type_alias,
                            &Substitution::combine(
                                &Substitution::from_local(
                                    &implements.deduced_substituion,
                                    implements_generic_item_ref,
                                ),
                                &Substitution::from_local(
                                    &new_trait_associated_ty.associated_substitution,
                                    GenericItemRef::ImplementsType(ImplementsTypeRef {
                                        implements_ref: ImplementsRef {
                                            trait_ref: new_trait_associated_ty
                                                .trait_type_ref
                                                .trait_ref,
                                            local_ref: implements.local_implements_ref,
                                        },
                                        local_ref: local_implements_type_ref,
                                    }),
                                ),
                            )
                            .unwrap(),
                            associated_bounds,
                        )?
                        .extract(&mut resolved_implements),
                    resolved_implements,
                })
            }
        }
    }

    /// Performs a substitution/mapping on the given [`constant::Constant`].
    #[allow(clippy::too_many_lines, clippy::missing_errors_doc)]
    pub fn transform_constant(
        &self,
        constant: constant::Constant,
        substitution: &Substitution,
        associated_bounds: &AssociatedBounds,
    ) -> Result<Transformed<constant::Constant>, SubstitutionError> {
        match constant {
            normal @ constant::Constant::Primitive(..) => Ok(Transformed {
                transformed: normal,
                resolved_implements: Vec::new(),
            }),
            constant::Constant::Struct(struct_val) => {
                let mut resolved_implements = Vec::new();

                let transformed_struct_ty = self
                    .transform_type(
                        ty::Type::Struct(struct_val.struct_ty),
                        substitution,
                        associated_bounds,
                    )?
                    .extract(&mut resolved_implements);

                let mut fields = Vec::with_capacity(struct_val.fields.len());

                for field in struct_val.fields {
                    fields.push(
                        self.transform_constant(field, substitution, associated_bounds)?
                            .extract(&mut resolved_implements),
                    );
                }

                Ok(Transformed {
                    transformed: constant::Constant::Struct(constant::Struct {
                        struct_ty: transformed_struct_ty
                            .into_struct()
                            .expect("should be some of struct"),
                        fields,
                    }),
                    resolved_implements,
                })
            }
            constant::Constant::Enum(enum_val) => {
                let mut resolved_implements = Vec::new();

                let transformed_enum_ty = self
                    .transform_type(
                        ty::Type::Enum(enum_val.enum_ty),
                        substitution,
                        associated_bounds,
                    )?
                    .extract(&mut resolved_implements);

                let value = if let Some(value) = enum_val.value {
                    Some(Box::new(
                        self.transform_constant(*value, substitution, associated_bounds)?
                            .extract(&mut resolved_implements),
                    ))
                } else {
                    None
                };

                Ok(Transformed {
                    transformed: constant::Constant::Enum(constant::Enum {
                        enum_ty: transformed_enum_ty
                            .into_enum()
                            .expect("should be some of enum"),
                        local_variant_ref: enum_val.local_variant_ref,
                        value,
                    }),
                    resolved_implements,
                })
            }
            constant::Constant::Array(array_val) => {
                let mut elements = Vec::with_capacity(array_val.elements.len());
                let mut resolved_implements = Vec::new();

                let element_ty = self
                    .transform_type(array_val.element_ty, substitution, associated_bounds)?
                    .extract(&mut resolved_implements);

                for element in array_val.elements {
                    elements.push(
                        self.transform_constant(element, substitution, associated_bounds)?
                            .extract(&mut resolved_implements),
                    );
                }

                Ok(Transformed {
                    transformed: constant::Constant::Array(constant::Array {
                        element_ty,
                        elements,
                    }),
                    resolved_implements,
                })
            }
            constant::Constant::Tuple(tuple_val) => {
                let mut resolved_implements = Vec::new();
                let mut elements = Vec::with_capacity(tuple_val.elements.len());

                for element in tuple_val.elements {
                    match element {
                        constant::TupleElement::Regular(regular) => {
                            elements.push(constant::TupleElement::Regular(
                                self.transform_constant(regular, substitution, associated_bounds)?
                                    .extract(&mut resolved_implements),
                            ));
                        }
                        constant::TupleElement::Unpacked(constant) => {
                            let transformed = match constant {
                                constant::Unpacked::Parameter(ty) => self.transform_constant(
                                    constant::Constant::Parameter(ty),
                                    substitution,
                                    associated_bounds,
                                ),
                                constant::Unpacked::TraitAssociated(ty) => self.transform_constant(
                                    constant::Constant::TraitAssociated(ty),
                                    substitution,
                                    associated_bounds,
                                ),
                            }?
                            .extract(&mut resolved_implements);

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

                Ok(Transformed {
                    transformed: constant::Constant::Tuple(constant::Tuple { elements }),
                    resolved_implements,
                })
            }
            constant::Constant::Parameter(parameter) => substitution
                .constant_substitutions
                .get(&parameter)
                .cloned()
                .map_or_else(
                    || {
                        Ok(Transformed {
                            transformed: constant::Constant::Parameter(parameter),
                            resolved_implements: Vec::new(),
                        })
                    },
                    |x| self.transform_constant(x, substitution, associated_bounds),
                ),
            constant::Constant::TraitAssociated(trait_associated_val) => {
                // try to find the associated type bounds early
                if let Some(transformed) = associated_bounds
                    .associated_constant_bounds
                    .get(&trait_associated_val)
                    .cloned()
                {
                    return self.transform_constant(transformed, substitution, associated_bounds);
                }

                let mut resolved_implements = Vec::new();

                let new_trait_associated_val = constant::TraitAssociated {
                    trait_constant_ref: trait_associated_val.trait_constant_ref,
                    trait_substitution: self
                        .transform_local_substitution(
                            trait_associated_val.trait_substitution,
                            substitution,
                            associated_bounds,
                        )?
                        .extract(&mut resolved_implements),
                };

                // early return
                if !new_trait_associated_val.trait_substitution.is_concrete() {
                    if let Some(transformed) = associated_bounds
                        .associated_constant_bounds
                        .get(&new_trait_associated_val)
                        .cloned()
                    {
                        return Ok(Transformed {
                            transformed: self
                                .transform_constant(transformed, substitution, associated_bounds)?
                                .extract(&mut resolved_implements),
                            resolved_implements,
                        });
                    }

                    return Ok(Transformed {
                        transformed: constant::Constant::TraitAssociated(new_trait_associated_val),
                        resolved_implements,
                    });
                }

                // perform trait resolution
                let trait_substitution = TraitSubstitution {
                    trait_ref: new_trait_associated_val.trait_constant_ref.trait_ref,
                    substitution: new_trait_associated_val.trait_substitution,
                };
                let Some(implements) = self
                    .resolve_trait_implements(&trait_substitution)
                    .expect("already is a concrete substitution")
                // failed to resolve the trait
                else {
                    return Err(SubstitutionError::FailedTraitResolution(trait_substitution));
                };

                let implements_generic_item_ref = GenericItemRef::Implements(ImplementsRef {
                    trait_ref: trait_associated_val.trait_constant_ref.trait_ref,
                    local_ref: implements.local_implements_ref,
                });

                let constant = {
                    let implements = &self.traits
                        [new_trait_associated_val.trait_constant_ref.trait_ref.0]
                        .implements[implements.local_implements_ref.0];

                    let local_implements_constant_ref = implements
                        .implements_constant_ref_by_trait_constant_ref
                        .get(&new_trait_associated_val.trait_constant_ref.local_ref)
                        .copied()
                        .ok_or(SubstitutionError::IncompleteImplements)?;

                    implements.constants[local_implements_constant_ref.0]
                        .constant
                        .clone()
                };

                Ok(Transformed {
                    transformed: self
                        .transform_constant(
                            constant,
                            &Substitution::from_local(
                                &implements.deduced_substituion,
                                implements_generic_item_ref,
                            ),
                            associated_bounds,
                        )?
                        .extract(&mut resolved_implements),
                    resolved_implements,
                })
            }
        }
    }

    /// Performs a substitution/mapping on the given [`LocalSubstitution`].
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_local_substitution(
        &self,
        source: LocalSubstitution,
        substitution: &Substitution,
        associated_bounds: &AssociatedBounds,
    ) -> Result<Transformed<LocalSubstitution>, SubstitutionError> {
        let mut resolved_implements = Vec::new();
        let mut type_substitutions = Vec::with_capacity(source.types.len());
        let mut constant_substitutions = Vec::with_capacity(source.constants.len());
        let mut lifetime_substitutions = Vec::with_capacity(source.lifetimes.len());

        for ty in source.types {
            type_substitutions.push(
                self.transform_type(ty, substitution, associated_bounds)?
                    .extract(&mut resolved_implements),
            );
        }

        for constant in source.constants {
            constant_substitutions.push(
                self.transform_constant(constant, substitution, associated_bounds)?
                    .extract(&mut resolved_implements),
            );
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

        Ok(Transformed {
            transformed: LocalSubstitution {
                types: type_substitutions,
                constants: constant_substitutions,
                lifetimes: lifetime_substitutions,
            },
            resolved_implements,
        })
    }
}
