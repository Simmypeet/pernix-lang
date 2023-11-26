//! Contains the code related to checking equality of entities.

use super::QueryRecords;
use crate::{
    entity::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments, Model, Substitution,
    },
    logic::{unification::All, Premises},
    table::Table,
};

macro_rules! equals_by_mapping_body {
    ($kind:ident) => {
        #[allow(clippy::ptr_arg)]
        fn equals_by_mapping(
            lhs: &Self,
            rhs: &Self,
            premises: &Premises<S>,
            table: &Table,
            records: &mut QueryRecords<S>,
        ) -> bool {
            for (source, targets) in &premises.mapping.$kind {
                for target in targets {
                    // avoid recursion
                    if source == rhs || (rhs == lhs && target == rhs) {
                        continue;
                    }

                    // check if the source and target are equal
                    if Self::equals_internal(lhs, source, premises, table, records)
                        && Self::equals_internal(rhs, target, premises, table, records)
                    {
                        return true;
                    }
                }
            }

            false
        }
    };
}

macro_rules! equals_body {
    ($kind:ident, $query_set:ident) => {
        #[allow(clippy::ptr_arg)]
        pub(super) fn equals_internal(
            &self,
            rhs: &Self,
            premises: &Premises<S>,
            table: &Table,
            records: &mut QueryRecords<S>,
        ) -> bool {
            // trivial case
            if self == rhs {
                return true;
            }

            let terms = (self.clone(), rhs.clone());

            // check if the terms are already being checked (recursion)
            if records.$query_set.contains(&terms) {
                return false;
            }

            records.$query_set.insert(terms.clone());

            // check if the terms are equal by mapping
            if Self::equals_by_mapping(&self, &rhs, premises, table, records) {
                records.$query_set.remove(&terms);
                return true;
            }

            // check if the terms are equal by unification
            if Self::equals_by_unification(&self, &rhs, premises, table, records) {
                records.$query_set.remove(&terms);
                return true;
            }

            // equal by normalization
            if Self::equals_by_normalization(&self, &rhs, premises, table, records) {
                records.$query_set.remove(&terms);
                return true;
            }

            records.$query_set.remove(&terms);

            return false;
        }
    };
}

macro_rules! equals_by_normalization {
    () => {
        fn equals_by_normalization(
            lhs: &Self,
            rhs: &Self,
            premises: &Premises<S>,
            table: &Table,
            records: &mut QueryRecords<S>,
        ) -> bool {
            let (normalizable, target) = match (lhs, rhs) {
                (Self::TraitMember(lhs), rhs) => (lhs, rhs),
                (lhs, Self::TraitMember(rhs)) => (rhs, lhs),
                (_, _) => return false,
            };

            let Some(normalized) = normalizable.normalize_internal(premises, table, records) else {
                return false;
            };

            Self::equals_internal(&normalized, target, premises, table, records)
        }
    };
}

macro_rules! equals_by_unification_body {
    () => {
        fn equals_by_unification(
            lhs: &Self,
            rhs: &Self,
            premises: &Premises<S>,
            table: &Table,
            records: &mut QueryRecords<S>,
        ) -> bool {
            let Ok(unifier) = Self::sub_structural_unify_internal(
                lhs,
                rhs,
                premises,
                table,
                records,
                &All,
                Substitution::default(),
            ) else {
                return false;
            };

            for (source, target) in &unifier.types {
                if !Type::equals_internal(source, target, premises, table, records) {
                    return false;
                }
            }

            for (source, target) in &unifier.constants {
                if !Constant::equals_internal(source, target, premises, table, records) {
                    return false;
                }
            }

            for (source, target) in &unifier.lifetimes {
                if !Lifetime::equals_internal(source, target, premises, table, records) {
                    return false;
                }
            }

            true
        }
    };
}

impl<S: Model> Type<S> {
    equals_body!(Type, type_equals);

    equals_by_mapping_body!(types);

    equals_by_unification_body!();

    equals_by_normalization!();

    /// Checks if the two type terms can be considered equal under the given mapping as a premise
    /// and normalization.
    ///
    /// # Parameters
    ///
    /// * `rhs`: The rhs type term to compare with.
    /// * `premises`: The mapping that is used as a premise.
    /// * `table`: The table that is used for normalization.
    pub fn equals(&self, rhs: &Self, premises: &Premises<S>, table: &Table) -> bool {
        Self::equals_internal(self, rhs, premises, table, &mut QueryRecords::default())
    }
}

impl<S: Model> Constant<S> {
    equals_body!(Constant, constant_equals);

    equals_by_mapping_body!(constants);

    equals_by_unification_body!();

    equals_by_normalization!();

    /// Checks if the two constant terms can be considered equal under the given mapping as a
    /// premise and normalization.
    ///
    /// # Parameters
    ///
    /// * `rhs`: The rhs constant term to compare with.
    /// * `premises`: The mapping that is used as a premise.
    /// * `table`: The table that is used for normalization.
    pub fn equals(&self, rhs: &Self, premises: &Premises<S>, table: &Table) -> bool {
        Self::equals_internal(self, rhs, premises, table, &mut QueryRecords::default())
    }
}

impl<S: Model> Lifetime<S> {
    equals_by_mapping_body!(lifetimes);

    /// Checks if the two type terms can be considered equal under the given mapping as a premise
    /// and normalization.
    ///
    /// # Parameters
    ///
    /// * `rhs`: The rhs type term to compare with.
    /// * `premises`: The mapping that is used as a premise.
    /// * `table`: The table that is used for normalization.
    pub fn equals(&self, rhs: &Self, premises: &Premises<S>, table: &Table) -> bool {
        Self::equals_internal(self, rhs, premises, table, &mut QueryRecords::default())
    }

    pub(super) fn equals_internal(
        &self,
        rhs: &Self,
        premises: &Premises<S>,
        table: &Table,
        records: &mut QueryRecords<S>,
    ) -> bool {
        if self == rhs {
            return true;
        }

        let terms = (self.clone(), rhs.clone());

        // check if the terms are already being checked (recursion)
        if records.lifetime_equals.contains(&terms) {
            return false;
        }

        records.lifetime_equals.insert(terms.clone());

        if Self::equals_by_mapping(self, rhs, premises, table, records) {
            records.lifetime_equals.remove(&terms);
            return true;
        }

        records.lifetime_equals.remove(&terms);
        false
    }
}

impl<S: Model> GenericArguments<S> {
    /// Checks if the two generic arguments terms can be considered equal under the given mapping as
    /// a premise and normalization.
    #[must_use]
    pub fn equals(&self, rhs: &Self, premises: &Premises<S>, table: &Table) -> bool {
        if self.types.len() != rhs.types.len()
            || self.constants.len() != rhs.constants.len()
            || self.lifetimes.len() != rhs.lifetimes.len()
        {
            return false;
        }

        for (lhs, rhs) in self.types.iter().zip(rhs.types.iter()) {
            if !Type::equals(lhs, rhs, premises, table) {
                return false;
            }
        }

        for (lhs, rhs) in self.constants.iter().zip(rhs.constants.iter()) {
            if !Constant::equals(lhs, rhs, premises, table) {
                return false;
            }
        }

        for (lhs, rhs) in self.lifetimes.iter().zip(rhs.lifetimes.iter()) {
            if !Lifetime::equals(lhs, rhs, premises, table) {
                return false;
            }
        }

        true
    }
}

#[cfg(test)]
pub mod tests;
