//! Contains the code related to checking equality of entities.

// Is this algorithm sound? I DON'T KNOW! HELP ME!

use std::cell::Cell;

use pernixc_base::extension::CellExt;

use super::{Mapping, QueryRecords};
use crate::{
    entity::{constant::Constant, r#type::Type, region::Region, Model},
    logic::{unification::All, Substitution},
    table::Table,
};

macro_rules! equals_by_mapping_body {
    ($kind:ident) => {
        #[allow(clippy::ptr_arg)]
        fn equals_by_mapping(
            lhs: &Self,
            rhs: &Self,
            premise_mapping: &Mapping<S>,
            table: &Table,
            records: &Cell<QueryRecords<S>>,
        ) -> bool {
            for (source, targets) in &premise_mapping.$kind {
                for target in targets {
                    // avoid recursion
                    if source == rhs || (rhs == lhs && target == rhs) {
                        continue;
                    }

                    // check if the source and target are equal
                    if Self::equals_internal(lhs, source, premise_mapping, table, records)
                        && Self::equals_internal(rhs, target, premise_mapping, table, records)
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
            lhs: &Self,
            rhs: &Self,
            premise_mapping: &Mapping<S>,
            table: &Table,
            records: &Cell<QueryRecords<S>>,
        ) -> bool {
            // trivial case
            if lhs == rhs {
                return true;
            }

            // mostly it should be just reference copying
            let terms = (lhs.clone(), rhs.clone());

            // check if the terms are already being checked (recursion)
            if records.visit(|x| x.$query_set.contains(&terms)) {
                return false;
            }

            records.visit_mut(|x| x.$query_set.insert(terms.clone()));

            // check if the terms are equal by mapping
            if Self::equals_by_mapping(&lhs, &rhs, premise_mapping, table, records) {
                records.visit_mut(|x| x.$query_set.remove(&terms));
                return true;
            }

            // check if the terms are equal by unification
            if Self::equals_by_unification(&lhs, &rhs, premise_mapping, table, records) {
                records.visit_mut(|x| x.$query_set.remove(&terms));
                return true;
            }

            // equal by normalization
            if Self::equals_by_normalization(&lhs, &rhs, premise_mapping, table, records) {
                records.visit_mut(|x| x.$query_set.remove(&terms));
                return true;
            }

            records.visit_mut(|x| x.$query_set.remove(&terms));

            return false;
        }
    };
}

macro_rules! equals_by_normalization {
    () => {
        fn equals_by_normalization(
            lhs: &Self,
            rhs: &Self,
            premise_mapping: &Mapping<S>,
            table: &Table,
            records: &Cell<QueryRecords<S>>,
        ) -> bool {
            let (normalizable, target) = match (lhs, rhs) {
                (Self::TraitMember(lhs), rhs) => (lhs, rhs),
                (lhs, Self::TraitMember(rhs)) => (rhs, lhs),
                (_, _) => return false,
            };

            let Some(normalized) = normalizable.normalize(premise_mapping, table, records) else {
                return false;
            };

            for normalized in normalized {
                if Self::equals_internal(&normalized, target, premise_mapping, table, records) {
                    return true;
                }
            }

            return false;
        }
    };
}

macro_rules! equals_by_unification_body {
    () => {
        fn equals_by_unification(
            lhs: &Self,
            rhs: &Self,
            premise_mapping: &Mapping<S>,
            table: &Table,
            records: &Cell<QueryRecords<S>>,
        ) -> bool {
            let Ok(unifier) = Self::sub_structural_unify_internal(
                lhs,
                rhs,
                premise_mapping,
                table,
                records,
                &All,
                Substitution::default(),
            ) else {
                return false;
            };

            for (source, target) in &unifier.types {
                if !Type::equals_internal(source, target, premise_mapping, table, records) {
                    return false;
                }
            }

            for (source, target) in &unifier.constants {
                if !Constant::equals_internal(source, target, premise_mapping, table, records) {
                    return false;
                }
            }

            for (source, target) in &unifier.regions {
                if !Region::equals_internal(source, target, premise_mapping, table, records) {
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
    /// * `premise_mapping`: The mapping that is used as a premise.
    /// * `table`: The table that is used for normalization.
    pub fn equals(&self, rhs: &Self, premise_mapping: &Mapping<S>, table: &Table) -> bool {
        Self::equals_internal(
            self,
            rhs,
            premise_mapping,
            table,
            &Cell::new(QueryRecords::default()),
        )
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
    /// * `premise_mapping`: The mapping that is used as a premise.
    /// * `table`: The table that is used for normalization.
    pub fn equals(&self, rhs: &Self, premise_mapping: &Mapping<S>, table: &Table) -> bool {
        Self::equals_internal(
            self,
            rhs,
            premise_mapping,
            table,
            &Cell::new(QueryRecords::default()),
        )
    }
}

impl<S: Model> Region<S> {
    equals_by_mapping_body!(regions);

    /// Checks if the two type terms can be considered equal under the given mapping as a premise
    /// and normalization.
    ///
    /// # Parameters
    ///
    /// * `rhs`: The rhs type term to compare with.
    /// * `premise_mapping`: The mapping that is used as a premise.
    /// * `table`: The table that is used for normalization.
    pub fn equals(&self, rhs: &Self, premise_mapping: &Mapping<S>, table: &Table) -> bool {
        Self::equals_internal(
            self,
            rhs,
            premise_mapping,
            table,
            &Cell::new(QueryRecords::default()),
        )
    }

    #[allow(clippy::ptr_arg)]
    pub(super) fn equals_internal(
        lhs: &Self,
        rhs: &Self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &Cell<QueryRecords<S>>,
    ) -> bool {
        if lhs == rhs {
            return true;
        }

        let terms = (lhs.clone(), rhs.clone());

        // check if the terms are already being checked (recursion)
        if records.visit(|x| x.region_equals.contains(&terms)) {
            return false;
        }

        records.visit_mut(|x| x.region_equals.insert(terms.clone()));

        if Self::equals_by_mapping(lhs, rhs, premise_mapping, table, records) {
            records.visit_mut(|x| x.region_equals.remove(&terms));
            return true;
        }

        records.visit_mut(|x| x.region_equals.remove(&terms));
        false
    }
}

#[cfg(test)]
pub mod tests;
