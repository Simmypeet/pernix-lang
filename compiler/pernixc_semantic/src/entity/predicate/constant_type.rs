use super::{ConstantType, Premises, QueryReocrds};
use crate::{
    entity::{predicate::NonEquality, r#type, Model},
    table::Table,
};

impl<S: Model> ConstantType<S> {
    /// Returns `true` if the predicate is satisfied based on the given premises.
    #[must_use]
    pub fn satisfies(&self, premises: &Premises<S>, table: &Table) -> bool {
        Self::satisfies_internal(&self.r#type, premises, table, &mut QueryReocrds::default())
    }

    fn satisfies_internal(
        required_type: &r#type::Type<S>,
        premises: &Premises<S>,
        table: &Table,
        query_records: &mut QueryReocrds<S>,
    ) -> bool {
        if query_records.constant_types.contains(required_type) {
            return false;
        }

        query_records.constant_types.insert(required_type.clone());

        // search in the premises
        for predicate in &premises.non_equality_predicates {
            let NonEquality::ConstantType(constant_type) = predicate else {
                continue;
            };

            if constant_type
                .r#type
                .equals(required_type, &premises.mapping, table)
            {
                query_records.constant_types.remove(required_type);
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
            query_records.constant_types.remove(required_type);
            return result;
        }

        let r#type::Type::TraitMember(trait_member) = required_type else {
            query_records.constant_types.remove(required_type);
            return false;
        };

        let Some(tys) = trait_member.normalize(&premises.mapping, table) else {
            query_records.constant_types.remove(required_type);
            return false;
        };

        for ty in tys {
            if Self::satisfies_internal(&ty, premises, table, query_records) {
                query_records.constant_types.remove(required_type);
                return true;
            }
        }

        return false;
    }
}
