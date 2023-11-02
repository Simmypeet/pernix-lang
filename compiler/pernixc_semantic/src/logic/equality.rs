//! Contains the code related to checking equality of entities.

use super::{unification::All, Mapping, QueryRecords};
use crate::{
    entity::{constant::Constant, r#type::Type, region::Region, Model},
    table::Table,
};

macro_rules! equals_by_mapping_body {
    ($kind:ident) => {
        fn equals_by_mapping(
            &self,
            other: &Self,
            premise_mapping: &Mapping<S>,
            table: &Table,
            records: &mut QueryRecords<S>,
        ) -> bool {
            for (source, targets) in &premise_mapping.$kind {
                for target in targets {
                    // avoid recursion
                    if source.as_ref() == other || (other == self && target.as_ref() == other) {
                        continue;
                    }
                    // check if the source and target are equal
                    if self.equals_internal(source.as_ref(), premise_mapping, table, records)
                        && other.equals_internal(target.as_ref(), premise_mapping, table, records)
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
        pub(super) fn equals_internal(
            &self,
            other: &Self,
            premise_mapping: &Mapping<S>,
            table: &Table,
            records: &mut QueryRecords<S>,
        ) -> bool {
            // trivial case
            if self == other {
                return true;
            }

            let terms = (self.clone(), other.clone());

            // check if the terms are already being checked (recursion)
            if records.$query_set.contains(&terms) {
                return false;
            }

            records.$query_set.insert(terms.clone());

            // check if the terms are equal by mapping
            if self.equals_by_mapping(other, premise_mapping, table, records) {
                records.$query_set.remove(&terms);
                return true;
            }

            // check if the terms are equal by unification
            if self.equals_by_unification(other, premise_mapping, table, records) {
                records.$query_set.remove(&terms);
                return true;
            }

            records.$query_set.remove(&terms);

            return false;
        }
    };
}

macro_rules! equals_by_unification_body {
    () => {
        fn equals_by_unification(
            &self,
            other: &Self,
            premise_mapping: &Mapping<S>,
            table: &Table,
            records: &mut QueryRecords<S>,
        ) -> bool {
            let Ok(unifier) = Self::sub_structural_unify(self, other, &All) else {
                return false;
            };

            for (source, target) in &unifier.types {
                if !source.equals_internal(target.as_ref(), premise_mapping, table, records) {
                    return false;
                }
            }

            for (source, target) in &unifier.constants {
                if !source.equals_internal(target.as_ref(), premise_mapping, table, records) {
                    return false;
                }
            }
            for (source, target) in &unifier.regions {
                if !source.equals_internal(target.as_ref(), premise_mapping, table, records) {
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

    /// Checks if the two type terms can be considered equal under the given mapping as a premise
    /// and normalization.
    ///
    /// # Parameters
    ///
    /// * `other`: The other type term to compare with.
    /// * `premise_mapping`: The mapping that is used as a premise.
    /// * `table`: The table that is used for normalization.
    pub fn equals(&self, other: &Self, premise_mapping: &Mapping<S>, table: &Table) -> bool {
        self.equals_internal(other, premise_mapping, table, &mut QueryRecords::default())
    }
}

impl<S: Model> Constant<S> {
    equals_body!(Constant, constant_equals);

    equals_by_mapping_body!(constants);

    equals_by_unification_body!();

    /// Checks if the two constant terms can be considered equal under the given mapping as a
    /// premise and normalization.
    ///
    /// # Parameters
    ///
    /// * `other`: The other constant term to compare with.
    /// * `premise_mapping`: The mapping that is used as a premise.
    /// * `table`: The table that is used for normalization.
    pub fn equals(&self, other: &Self, premise_mapping: &Mapping<S>, table: &Table) -> bool {
        self.equals_internal(other, premise_mapping, table, &mut QueryRecords::default())
    }
}

impl<S: Model> Region<S> {
    equals_by_mapping_body!(regions);

    /// Checks if the two type terms can be considered equal under the given mapping as a premise
    /// and normalization.
    ///
    /// # Parameters
    ///
    /// * `other`: The other type term to compare with.
    /// * `premise_mapping`: The mapping that is used as a premise.
    /// * `table`: The table that is used for normalization.
    pub fn equals(&self, other: &Self, premise_mapping: &Mapping<S>, table: &Table) -> bool {
        self.equals_internal(other, premise_mapping, table, &mut QueryRecords::default())
    }

    fn equals_internal(
        &self,
        other: &Self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &mut QueryRecords<S>,
    ) -> bool {
        if self == other {
            return true;
        }

        let terms = (self.clone(), other.clone());

        // check if the terms are already being checked (recursion)
        if records.region_equals.contains(&terms) {
            return false;
        }

        records.region_equals.insert(terms.clone());

        if self.equals_by_mapping(other, premise_mapping, table, records) {
            records.region_equals.remove(&terms);
            return true;
        }

        records.region_equals.remove(&terms);
        false
    }
}

#[cfg(test)]
pub mod tests;
