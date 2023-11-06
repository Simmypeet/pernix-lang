//! Contains code related to logic applied to the entities.

use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
};

use pernixc_base::extension::{CellExt, SafeIndex};

use crate::{
    arena::ID,
    entity::{
        constant::{self, Constant},
        r#type::{self, Type},
        region::Region,
        GenericArguments, Model, Never,
    },
    symbol::{
        ConstantParameterID, GenericID, ImplementationID, LifetimeParameterID, MemberID,
        TypeParameterID,
    },
    table::Table,
};

pub mod equality;
pub mod substitution;
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

/// Represents a substitution of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Substitution<S: Model> {
    pub types: HashMap<Type<S>, Type<S>>,
    pub constants: HashMap<Constant<S>, Constant<S>>,
    pub regions: HashMap<Region<S>, Region<S>>,
}

impl<S: Model> Substitution<S> {
    /// Converts this substitution into another model.
    #[must_use]
    pub fn into_other_model<T: Model>(self) -> Substitution<T>
    where
        S: Model<TypeInference = Never, ConstantInference = Never, RegionContext = Never>,
    {
        todo!()
    }

    /// Appends the given generic arguments as a substitution.
    ///
    /// # Returns
    ///
    /// Returns `None` if the substitution already contains the given generic arguments.
    #[must_use]
    pub fn append_from_generic_arguments(
        mut self,
        generic_arguments: GenericArguments<S>,
        generic_id: GenericID,
    ) -> Option<Self> {
        for (index, term) in generic_arguments.types.into_iter().enumerate() {
            if self
                .types
                .insert(
                    Type::Parameter(TypeParameterID {
                        parent: generic_id,
                        id: ID::new(index),
                    }),
                    term,
                )
                .is_some()
            {
                return None;
            }
        }

        for (index, term) in generic_arguments.constants.into_iter().enumerate() {
            if self
                .constants
                .insert(
                    Constant::Parameter(ConstantParameterID {
                        parent: generic_id,
                        id: ID::new(index),
                    }),
                    term,
                )
                .is_some()
            {
                return None;
            }
        }

        for (index, term) in generic_arguments.regions.into_iter().enumerate() {
            if self
                .regions
                .insert(
                    Region::Named(LifetimeParameterID {
                        parent: generic_id,
                        id: ID::new(index),
                    }),
                    term,
                )
                .is_some()
            {
                return None;
            }
        }

        Some(self)
    }

    /// Converts the given generic arguments into a substitution.
    #[must_use]
    pub fn from_generic_arguments(
        generic_arguments: GenericArguments<S>,
        generic_id: GenericID,
    ) -> Self {
        Self::default()
            .append_from_generic_arguments(generic_arguments, generic_id)
            .unwrap()
    }
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
}

macro_rules! normalization_body {
    ($obj:path, $kind:ident, $trait_field:ident, $term:ident, $arguments_field:ident,
        |
        $implementation:ident,
        $normalizable:ident,
        $implementation_symbol:ident
        | $substitution:expr) => {
        fn normalize<'a>(
            &'a $normalizable,
            premise_mapping: &Mapping<S>,
            table: &'a Table,
            records: &Cell<QueryRecords<S>>,
        ) -> impl Iterator<Item = $obj> + 'a {
            // gets the implementation of the trait
            let implementations = table.resolve_implementation_internal(
                $normalizable.$trait_field.parent,
                &$normalizable.$arguments_field,
                premise_mapping,
                records,
            );

            implementations.into_iter().filter_map(|$implementation| {
                let implementation_id = ImplementationID {
                    parent: $normalizable.$trait_field.parent,
                    id: $implementation.implementation_id,
                };

                let Some($implementation_symbol) = table.get(implementation_id) else {
                    return None;
                };

                let Some(mut equivalent) = $implementation_symbol
                    .$term
                    .get_id(&$normalizable.$trait_field.id)
                    .and_then(|x| $implementation_symbol.$term.get(x))
                    .map(|x| x.$kind.clone().into_other_model())
                else {
                    return None;
                };

                let substitution = $substitution;
                equivalent.apply(&substitution);

                Some(equivalent)
            })
        }
    };
}

impl<S: Model> constant::TraitMember<S> {
    normalization_body!(
        Constant<S>,
        constant,
        trait_constant_id,
        constants,
        trait_arguments,
        |implementation, self, implementation_symbol| {
            implementation.deduced_unification.into_other_model::<S>()
        }
    );
}

impl<S: Model> r#type::TraitMember<S> {
    normalization_body!(
        Type<S>,
        r#type,
        trait_type_id,
        types,
        trait_generic_arguments,
        |implementation, self, implementation_symbol| {
            let implementation_id = ImplementationID {
                parent: self.trait_type_id.parent,
                id: implementation.implementation_id,
            };

            let substitution = implementation.deduced_unification.into_other_model::<S>();

            substitution.append_from_generic_arguments(
                self.member_generic_arguments.clone(),
                crate::symbol::GenericID::ImplementationType(MemberID {
                    parent: implementation_id,
                    id: match implementation_symbol.types.get_id(&self.trait_type_id.id) {
                        Some(id) => id,
                        None => return None,
                    },
                }),
            )?
        }
    );
}

impl<S: Model> GenericArguments<S> {
    fn is_definite_internal(
        &self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &Cell<QueryRecords<S>>,
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
        records: &Cell<QueryRecords<S>>,
    ) -> bool {
        // checks if the constant is already in the records
        if records.visit(|x| x.constant_is_definite.contains(self)) {
            return false;
        }

        records.visit_mut(|x| x.constant_is_definite.insert(self.clone()));

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

                for normalized in constant.normalize(premise_mapping, table, records) {
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

        records.visit_mut(|x| x.constant_is_definite.remove(self));
        result
    }
}

impl<S: Model> Type<S> {
    fn is_definite_internal(
        &self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &Cell<QueryRecords<S>>,
    ) -> bool {
        // checks if the type is already in the records
        if records.visit(|x| x.type_is_definite.contains(self)) {
            return false;
        }

        records.visit_mut(|x| x.type_is_definite.insert(self.clone()));

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

                for normalized in ty.normalize(premise_mapping, table, records) {
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

        records.visit_mut(|x| x.type_is_definite.remove(self));
        result
    }
}
