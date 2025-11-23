use pernixc_hash::HashMap;
use pernixc_symbol::name::get_name;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::{GenericArguments, TraitMember},
    predicate::Predicate,
    r#type::Type,
};

use crate::formatter::Formatter;

impl Formatter<'_, '_> {
    async fn format_outlives_clause<
        'a,
        T: std::hash::Hash + Eq + pernixc_term::display::Display + 'a,
    >(
        &mut self,
        outlives_clause: impl Iterator<
            Item = &'a pernixc_term::predicate::Outlives<T>,
        >,
    ) -> Result<(), super::Error> {
        let mut ty = HashMap::<_, Vec<_>>::default();

        for predicate in outlives_clause {
            ty.entry(&predicate.operand).or_default().push(&predicate.bound);
        }

        for (operand, bounds) in ty {
            self.new_line(async |mut x| {
                x.write_term(operand).await?;
                write!(x.buffer, ": ").unwrap();

                let mut is_first = true;
                for bound in bounds {
                    if !is_first {
                        write!(x.buffer, " + ").unwrap();
                    }
                    is_first = false;

                    x.write_lifetime(bound).await?;
                }

                Ok(())
            })
            .await?;
        }

        Ok(())
    }

    async fn trait_type_compatible_clause(
        &mut self,
        trait_type_compatible_clause: impl Iterator<
            Item = &'_ pernixc_term::predicate::Compatible<TraitMember, Type>,
        >,
    ) -> Result<(), super::Error> {
        for predicate in trait_type_compatible_clause {
            self.new_line(async |mut x| {
                x.write_term(&predicate.lhs).await?;
                write!(x.buffer, " == ").unwrap();
                x.write_term(&predicate.rhs).await?;

                Ok(())
            })
            .await?;
        }

        Ok(())
    }

    async fn constant_type_clause(
        &mut self,
        mut constant_type_clause: impl Iterator<
            Item = &'_ pernixc_term::predicate::ConstantType,
        >,
    ) -> Result<(), super::Error> {
        let Some(first) = constant_type_clause.next() else {
            return Ok(());
        };

        self.new_line(async |mut x| {
            write!(x.buffer, "const ").unwrap();
            x.write_term(&first.0).await?;

            for predicate in constant_type_clause {
                write!(x.buffer, " + ").unwrap();
                x.write_term(&predicate.0).await?;
            }

            Ok(())
        })
        .await
    }

    async fn tuple_type_clause(
        &mut self,
        mut tuple_type_clause: impl Iterator<
            Item = &'_ pernixc_term::predicate::Tuple<Type>,
        >,
    ) -> Result<(), super::Error> {
        let Some(first) = tuple_type_clause.next() else {
            return Ok(());
        };

        self.new_line(async |mut x| {
            write!(x.buffer, "tuple ").unwrap();
            x.write_term(&first.0).await?;

            for predicate in tuple_type_clause {
                write!(x.buffer, " + ").unwrap();
                x.write_term(&predicate.0).await?;
            }

            Ok(())
        })
        .await
    }

    async fn qualified_identifier_bound<'x>(
        &mut self,
        ident: &str,
        mut bounds: impl Iterator<
            Item = (bool, Global<pernixc_symbol::ID>, &'x GenericArguments),
        >,
    ) -> Result<(), super::Error> {
        let Some((first_negative, first_id, first_arguments)) = bounds.next()
        else {
            return Ok(());
        };

        self.new_line(async |mut x| {
            write!(x.buffer, "{ident} ").unwrap();

            let name = x.engine.get_name(first_id).await;
            if first_negative {
                write!(x.buffer, "not ").unwrap();
            }
            write!(x.buffer, "{name}").unwrap();
            x.write_term(first_arguments).await?;

            for (is_negative, id, arguments) in bounds {
                write!(x.buffer, " + ").unwrap();

                let name = x.engine.get_name(id).await;
                if is_negative {
                    write!(x.buffer, "not ").unwrap();
                }
                write!(x.buffer, "{name}").unwrap();
                x.write_term(arguments).await?;
            }

            Ok(())
        })
        .await
    }

    /// Formats a where clause if it exists.
    pub async fn format_where_clause(
        &mut self,
        where_clause: &[pernixc_semantic_element::where_clause::Predicate],
    ) -> Result<bool, super::Error> {
        if where_clause.is_empty() {
            return Ok(false);
        }

        self.new_line(async |mut x| {
            write!(x.buffer, " where").unwrap();

            x.indent(async |x| {
                x.format_outlives_clause(
                    where_clause
                        .iter()
                        .filter_map(|x| x.predicate.as_type_outlives()),
                )
                .await?;
                x.format_outlives_clause(
                    where_clause
                        .iter()
                        .filter_map(|x| x.predicate.as_lifetime_outlives()),
                )
                .await?;
                x.trait_type_compatible_clause(
                    where_clause
                        .iter()
                        .filter_map(|x| x.predicate.as_trait_type_compatible()),
                )
                .await?;
                x.constant_type_clause(
                    where_clause
                        .iter()
                        .filter_map(|x| x.predicate.as_constant_type()),
                )
                .await?;
                x.tuple_type_clause(
                    where_clause
                        .iter()
                        .filter_map(|x| x.predicate.as_tuple_type()),
                )
                .await?;
                x.qualified_identifier_bound(
                    "trait",
                    where_clause.iter().filter_map(|x| match &x.predicate {
                        Predicate::PositiveTrait(x) => {
                            Some((false, x.trait_id, &x.generic_arguments))
                        }
                        Predicate::NegativeTrait(x) => {
                            Some((true, x.trait_id, &x.generic_arguments))
                        }
                        _ => None,
                    }),
                )
                .await?;
                x.qualified_identifier_bound(
                    "marker",
                    where_clause.iter().filter_map(|x| match &x.predicate {
                        Predicate::PositiveMarker(x) => {
                            Some((false, x.marker_id, &x.generic_arguments))
                        }
                        Predicate::NegativeMarker(x) => {
                            Some((true, x.marker_id, &x.generic_arguments))
                        }
                        _ => None,
                    }),
                )
                .await?;

                Ok(())
            })
            .await?;

            Ok(())
        })
        .await?;

        Ok(true)
    }
}
