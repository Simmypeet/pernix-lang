//! Contains code related to logic applied to the entities.

use std::collections::{HashMap, HashSet};

use crate::{
    entity::{
        constant::{self, Constant},
        r#type::{self, Type},
        region::Region,
        Entity, GenericArguments, Model,
    },
    table::{Index, Table},
};

pub mod equality;
pub mod r#trait;
pub mod unification;

/// Is used to store the requested queries to prevent infinite recursion.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct QueryRecords<S: Model> {
    type_equals: HashSet<(Type<S>, Type<S>)>,
    constant_equals: HashSet<(Constant<S>, Constant<S>)>,
    region_equals: HashSet<(Region<S>, Region<S>)>,

    type_unifies: HashSet<(Type<S>, Type<S>)>,
    constant_unifies: HashSet<(Constant<S>, Constant<S>)>,

    type_is_definite: HashSet<Type<S>>,
    constant_is_definite: HashSet<Constant<S>>,
}

/// Represents a mapping of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Mapping<S: Model> {
    regions: HashMap<Region<S>, HashSet<Region<S>>>,
    types: HashMap<Type<S>, HashSet<Type<S>>>,
    constants: HashMap<Constant<S>, HashSet<Constant<S>>>,
}

macro_rules! insert_item {
    ($map:expr, $expr:expr) => {{
        for (lhs, rhs) in $expr {
            $map.entry(lhs.clone()).or_default().insert(rhs.clone());
            $map.entry(rhs).or_default().insert(lhs);
        }
    }};
}

impl<S: Model> Mapping<S> {
    /// Creates a new mapping from the given equality pairs.
    pub fn from_pairs(
        regions: impl IntoIterator<Item = (Region<S>, Region<S>)>,
        types: impl IntoIterator<Item = (Type<S>, Type<S>)>,
        constants: impl IntoIterator<Item = (Constant<S>, Constant<S>)>,
    ) -> Self {
        let mut mappings = Self::default();

        insert_item!(mappings.regions, regions);
        insert_item!(mappings.types, types);
        insert_item!(mappings.constants, constants);

        mappings
    }

    /// Creates a new mapping from the given equality pairs.
    pub fn insert_type(&mut self, lhs: Type<S>, rhs: Type<S>) {
        self.types
            .entry(lhs.clone())
            .or_default()
            .insert(rhs.clone());
        self.types.entry(rhs).or_default().insert(lhs);
    }

    /// Creates a new mapping from the given equality pairs.
    pub fn insert_constant(&mut self, lhs: Constant<S>, rhs: Constant<S>) {
        self.constants
            .entry(lhs.clone())
            .or_default()
            .insert(rhs.clone());
        self.constants.entry(rhs).or_default().insert(lhs);
    }
}

macro_rules! normalization_body {
    ($obj:path, $kind:ident, $trait_field:ident, $term:ident, $table:ident, $arguments_field:ident,
        |
        $implementation:ident,
        $normalizable:ident,
        $implementation_symbol:ident
        | $substitution:expr) => {
        #[allow(clippy::significant_drop_tightening)]
        fn normalize_internal<'a>(
            &'a $normalizable,
            premise_mapping: &Mapping<S>,
            $table: &'a Table,
            records: &mut QueryRecords<S>,
        ) -> Option<impl Iterator<Item = $obj> + 'a> {
            // gets the implementation of the trait
            let Some(trait_id) = $table.get($normalizable.$trait_field).map(|x| x.parent_trait_id)
            else {
                return None;
            };

            let implementations = $table.resolve_implementation_internal(
                trait_id,
                &$normalizable.$arguments_field,
                premise_mapping,
                records,
            );

            Some(implementations.into_iter().filter_map(|$implementation| {
                let implementation_id = $implementation.implementation_id;

                let Some($implementation_symbol) = $table.get(implementation_id) else {
                    return None;
                };

                let Some(mut equivalent) = $implementation_symbol
                    .$term
                    .get(&$normalizable.$trait_field)
                    .copied()
                    .and_then(|x| $table.get(x))
                    .map(|x| x.$kind.clone().into_other_model())
                else {
                    return None;
                };

                let substitution = $substitution;
                equivalent.apply(&substitution);

                Some(equivalent)
            }))
        }
    };
}

impl<S: Model> constant::TraitMember<S> {
    normalization_body!(
        Constant<S>,
        constant,
        trait_constant_id,
        implementation_constant_ids_by_trait_constant_id,
        table,
        trait_arguments,
        |implementation, self, implementation_symbol| { implementation.deduced_unification }
    );
}

impl<S: Model> r#type::TraitMember<S> {
    normalization_body!(
        Type<S>,
        r#type,
        trait_type_id,
        implementation_type_ids_by_trait_type_id,
        table,
        trait_generic_arguments,
        |implementation, self, implementation_symbol| {
            implementation
                .deduced_unification
                .append_from_generic_arguments(
                    self.member_generic_arguments.clone(),
                    crate::symbol::GenericID::ImplementationType(
                        implementation_symbol
                            .implementation_type_ids_by_trait_type_id
                            .get(&self.trait_type_id)
                            .copied()
                            .unwrap(),
                    ),
                )?
        }
    );

    /// Normalizes the trait member into concrete types.
    #[must_use]
    pub fn normalize<'a>(
        &'a self,
        premise_mapping: &Mapping<S>,
        table: &'a Table,
    ) -> Option<impl Iterator<Item = r#type::Type<S>> + 'a> {
        self.normalize_internal(premise_mapping, table, &mut QueryRecords::default())
    }
}

impl<S: Model> GenericArguments<S> {
    fn is_definite_internal(
        &self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &mut QueryRecords<S>,
    ) -> bool {
        self.types
            .iter()
            .all(|x| x.is_definite_internal(premise_mapping, table, records))
            && self
                .constants
                .iter()
                .all(|x| x.is_definite_internal(premise_mapping, table, records))
    }
}

impl<S: Model> Constant<S> {
    fn is_definite_internal(
        &self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &mut QueryRecords<S>,
    ) -> bool {
        // checks if the constant is already in the records
        if records.constant_is_definite.contains(self) {
            return false;
        }

        records.constant_is_definite.insert(self.clone());

        let mut result = match self {
            Self::Primitive(_) => true,
            Self::Inference(_) | Self::Parameter(_) => false,
            Self::Struct(constant) => {
                constant
                    .generic_arguments
                    .is_definite_internal(premise_mapping, table, records)
                    && constant
                        .fields
                        .iter()
                        .all(|x| x.is_definite_internal(premise_mapping, table, records))
            }
            Self::Enum(constant) => {
                constant
                    .generic_arguments
                    .is_definite_internal(premise_mapping, table, records)
                    && constant.associated_value.as_ref().map_or(true, |x| {
                        x.is_definite_internal(premise_mapping, table, records)
                    })
            }
            Self::Array(constant) => {
                constant
                    .element_ty
                    .is_definite_internal(premise_mapping, table, records)
                    && constant
                        .elements
                        .iter()
                        .all(|x| x.is_definite_internal(premise_mapping, table, records))
            }
            Self::TraitMember(constant) => {
                let mut result = false;

                let Some(normalized) = constant.normalize_internal(premise_mapping, table, records)
                else {
                    return false;
                };

                for normalized in normalized {
                    if normalized.is_definite_internal(premise_mapping, table, records) {
                        result = true;
                        break;
                    }
                }

                result
            }
            Self::Tuple(constant) => constant.elements.iter().all(|x| match x {
                constant::TupleElement::Regular(regular) => {
                    regular.is_definite_internal(premise_mapping, table, records)
                }
                constant::TupleElement::Unpacked(constant::Unpacked::Parameter(param)) => {
                    Self::Parameter(*param).is_definite_internal(premise_mapping, table, records)
                }
                constant::TupleElement::Unpacked(constant::Unpacked::TraitMember(trait_member)) => {
                    Self::TraitMember(trait_member.clone()).is_definite_internal(
                        premise_mapping,
                        table,
                        records,
                    )
                }
            }),
        };

        if !result {
            'outer: for (lhs, rhss) in &premise_mapping.constants {
                if Self::equals_internal(self, lhs, premise_mapping, table, records) {
                    for rhs in rhss {
                        if rhs.is_definite_internal(premise_mapping, table, records) {
                            result = true;
                            break 'outer;
                        }
                    }
                }
            }
        }

        records.constant_is_definite.remove(self);
        result
    }
}

impl<S: Model> Type<S> {
    fn is_definite_internal(
        &self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &mut QueryRecords<S>,
    ) -> bool {
        // checks if the type is already in the records
        if records.type_is_definite.contains(self) {
            return false;
        }

        records.type_is_definite.insert(self.clone());

        let mut result = match self {
            Self::Primitive(_) => true,
            Self::Inference(_) | Self::Parameter(_) => false,
            Self::Algebraic(ty) => {
                ty.generic_arguments
                    .is_definite_internal(premise_mapping, table, records)
            }
            Self::Pointer(ty) => ty
                .pointee
                .is_definite_internal(premise_mapping, table, records),
            Self::Reference(ty) => ty
                .pointee
                .is_definite_internal(premise_mapping, table, records),
            Self::Array(ty) => {
                ty.length
                    .is_definite_internal(premise_mapping, table, records)
                    && ty
                        .element
                        .is_definite_internal(premise_mapping, table, records)
            }
            Self::TraitMember(ty) => {
                let mut result = false;

                let Some(normalized) = ty.normalize_internal(premise_mapping, table, records)
                else {
                    return false;
                };

                for normalized in normalized {
                    if normalized.is_definite_internal(premise_mapping, table, records) {
                        result = true;
                        break;
                    }
                }

                result
            }
            Self::Tuple(param) => param.elements.iter().all(|x| match x {
                r#type::TupleElement::Regular(regular) => {
                    regular.is_definite_internal(premise_mapping, table, records)
                }
                r#type::TupleElement::Unpacked(r#type::Unpacked::Parameter(param)) => {
                    Self::Parameter(*param).is_definite_internal(premise_mapping, table, records)
                }
                r#type::TupleElement::Unpacked(r#type::Unpacked::TraitMember(trait_member)) => {
                    Self::TraitMember(trait_member.clone()).is_definite_internal(
                        premise_mapping,
                        table,
                        records,
                    )
                }
            }),
        };

        if !result {
            'outer: for (lhs, rhss) in &premise_mapping.types {
                if Self::equals_internal(self, lhs, premise_mapping, table, records) {
                    for rhs in rhss {
                        if rhs.is_definite_internal(premise_mapping, table, records) {
                            result = true;
                            break 'outer;
                        }
                    }
                }
            }
        }

        records.type_is_definite.remove(self);
        result
    }
}
