use super::{r#trait::RegionConstraint, unification, QueryRecords};
use crate::{
    entity::{
        constant,
        predicate::{
            ConstantEquals, ConstantType, NonEquality, Premises, RegionOutlives, Trait, TypeEquals,
            TypeOutlives,
        },
        r#type,
        region::Region,
        GenericArguments, Model, Never,
    },
    table::Table,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct HigherRankedLifetimeUnificationConfig;

impl<S: Model> unification::Config<S> for HigherRankedLifetimeUnificationConfig {
    fn type_mappable(&self, _: &r#type::Type<S>, _: &r#type::Type<S>) -> bool { false }

    fn constant_mappable(&self, _: &constant::Constant<S>, _: &constant::Constant<S>) -> bool {
        false
    }

    fn region_mappable(&self, _: &Region<S>, rhs: &Region<S>) -> bool { rhs.is_forall() }
}

impl<S: Model<LocalRegion = Never>> TypeOutlives<S> {
    /// Returns `true` if the predicate is satisfied based on the given premises.
    #[must_use]
    pub fn satisfies(&self, premises: &Premises<S>, table: &Table) -> bool { true }
}

impl<S: Model<LocalRegion = Never>> RegionOutlives<S> {
    /// Returns `true` if the predicate is satisfied based on the given premises.
    #[must_use]
    pub fn satisfies(&self, premises: &Premises<S>, table: &Table) -> bool {
        // search in the premises
        for predicate in &premises.non_equality_predicates {
            let NonEquality::RegionOutlives(region_outlives) = predicate else {
                continue;
            };

            if region_outlives
                .operand
                .equals(&self.operand, premises, table)
                && region_outlives
                    .argument
                    .equals(&self.argument, premises, table)
            {
                return true;
            }
        }

        match (&self.operand, &self.argument) {
            // impossible
            (Region::Local(never), _) | (_, Region::Local(never)) => match *never {},

            // always unsatisfied
            (Region::Forall(_), _) | (_, Region::Forall(_)) => false,

            // if operand is 'static, then it is satisfied
            (operand, _) => operand.equals(&Region::Static, premises, table),
        }
    }
}

/// Results of checking whether a trait bound is satifiable or not.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitSatisfiability<S: Model> {
    /// The trait bound is satisfiable if and only if the given region constraints are satisfied.
    Satisfiable(Vec<RegionConstraint<S>>),

    /// The trait bound is not satisfiable.
    Unsatisfiable,
}

impl<S: Model> Trait<S> {
    /// Returns `true` if the predicate is satisfied based on the given premises.
    #[must_use]
    pub fn satisfies(&self, premises: &Premises<S>, table: &Table) -> TraitSatisfiability<S> {
        for premise in &premises.non_equality_predicates {
            let NonEquality::Trait(trait_predicate) = premise else {
                continue;
            };

            // not-applicable
            if (self.const_trait && !trait_predicate.const_trait)
                || (self.trait_id != trait_predicate.trait_id)
            {
                continue;
            }

            if GenericArguments::unify(
                &self.generic_arguments,
                &trait_predicate.generic_arguments,
                &Premises::default(),
                table,
                &HigherRankedLifetimeUnificationConfig,
            )
            .is_ok()
            {
                // no additional constraint
                return TraitSatisfiability::Satisfiable(Vec::new());
            }
        }

        if let Ok(ok) =
            table.resolve_implementation(self.trait_id, &self.generic_arguments, premises)
        {
            TraitSatisfiability::Satisfiable(ok.region_constraints)
        } else {
            TraitSatisfiability::Unsatisfiable
        }
    }
}

impl<S: Model> TypeEquals<S> {
    /// Returns `true` if the predicate is satisfied based on the given premises.
    #[must_use]
    pub fn satisfies(&self, premises: &Premises<S>, table: &Table) -> bool {
        Self::satisfies_internal(
            &self.lhs,
            &self.rhs,
            premises,
            table,
            &mut QueryRecords::default(),
        )
    }

    pub(super) fn satisfies_internal(
        lhs: &r#type::Type<S>,
        rhs: &r#type::Type<S>,
        premises: &Premises<S>,
        table: &Table,
        query_records: &mut QueryRecords<S>,
    ) -> bool {
        lhs.equals_internal(rhs, premises, table, query_records)
    }
}

impl<S: Model> ConstantEquals<S> {
    /// Returns `true` if the predicate is satisfied based on the given premises.
    #[must_use]
    pub fn satisfies(&self, premises: &Premises<S>, table: &Table) -> bool {
        Self::satisfies_internal(
            &self.lhs,
            &self.rhs,
            premises,
            table,
            &mut QueryRecords::default(),
        )
    }

    pub(super) fn satisfies_internal(
        lhs: &constant::Constant<S>,
        rhs: &constant::Constant<S>,
        premises: &Premises<S>,
        table: &Table,
        query_records: &mut QueryRecords<S>,
    ) -> bool {
        lhs.equals_internal(rhs, premises, table, query_records)
    }
}

impl<S: Model> ConstantType<S> {
    /// Returns `true` if the predicate is satisfied based on the given premises.
    #[must_use]
    pub fn satisfies(&self, premises: &Premises<S>, table: &Table) -> bool {
        Self::satisfies_internal(&self.r#type, premises, table, &mut QueryRecords::default())
    }

    pub(super) fn satisfies_internal(
        required_type: &r#type::Type<S>,
        premises: &Premises<S>,
        table: &Table,
        query_records: &mut QueryRecords<S>,
    ) -> bool {
        if query_records
            .constant_type_satisfies
            .contains(required_type)
        {
            return false;
        }

        query_records
            .constant_type_satisfies
            .insert(required_type.clone());

        // search in the premises
        for predicate in &premises.non_equality_predicates {
            let NonEquality::ConstantType(constant_type) = predicate else {
                continue;
            };

            if constant_type
                .r#type
                .equals_internal(required_type, premises, table, query_records)
            {
                query_records.constant_type_satisfies.remove(required_type);
                return true;
            }
        }

        let result = match required_type {
            r#type::Type::Pointer(_)
            | r#type::Type::Reference(_)
            | r#type::Type::Inference(_)
            | r#type::Type::TraitMember(_)
            | r#type::Type::Parameter(_) => false,

            r#type::Type::Primitive(_) => true,

            // don't have to decide that
            r#type::Type::Algebraic(algebraic) => 'a: {
                // no borrowing in constant type context
                if !algebraic.generic_arguments.regions.is_empty() {
                    break 'a false;
                };

                algebraic
                    .generic_arguments
                    .types
                    .iter()
                    .all(|ty| Self::satisfies_internal(ty, premises, table, query_records))
            }

            r#type::Type::Array(array) => {
                Self::satisfies_internal(&array.element, premises, table, query_records)
            }

            r#type::Type::Tuple(tuple) => tuple.elements.iter().all(|x| match x {
                r#type::TupleElement::Regular(x) => {
                    Self::satisfies_internal(x, premises, table, query_records)
                }
                r#type::TupleElement::Unpacked(r#type::Unpacked::Parameter(x)) => {
                    Self::satisfies_internal(
                        &r#type::Type::Parameter(*x),
                        premises,
                        table,
                        query_records,
                    )
                }
                r#type::TupleElement::Unpacked(r#type::Unpacked::TraitMember(x)) => {
                    Self::satisfies_internal(
                        &r#type::Type::TraitMember(x.clone()),
                        premises,
                        table,
                        query_records,
                    )
                }
            }),
        };

        if result {
            query_records.constant_type_satisfies.remove(required_type);
            return result;
        }

        let r#type::Type::TraitMember(trait_member) = required_type else {
            query_records.constant_type_satisfies.remove(required_type);
            return false;
        };

        let Some(tys) = trait_member.normalize_internal(premises, table, query_records) else {
            query_records.constant_type_satisfies.remove(required_type);
            return false;
        };

        for ty in tys {
            if Self::satisfies_internal(&ty, premises, table, query_records) {
                query_records.constant_type_satisfies.remove(required_type);
                return true;
            }
        }

        query_records.constant_type_satisfies.remove(required_type);
        false
    }
}
