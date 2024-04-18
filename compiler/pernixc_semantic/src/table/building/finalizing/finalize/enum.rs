use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{build_flag, Finalize};
use crate::{
    arena::ID,
    error,
    semantic::{
        session::{self, Limit},
        simplify, Environment,
    },
    symbol::Enum,
    table::{
        building::finalizing::{occurrences::Occurrences, Finalizer},
        Index, Table,
    },
};

build_flag! {
    pub enum Flag {
        /// Generic parameters are built
        GenericParameter,
        /// Where clause predicates are built
        WhereClause,
        /// All the enum variants are built
        Complete,
        /// Bounds check are performed
        Check,
    }
}

impl Finalize for Enum {
    type SyntaxTree = syntax_tree::item::EnumSignature;
    type Flag = Flag;
    type Data = Occurrences;

    #[allow(
        clippy::significant_drop_in_scrutinee,
        clippy::significant_drop_tightening
    )]
    fn finalize(
        table: &Table<Finalizer>,
        symbol_id: ID<Self>,
        state_flag: Self::Flag,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match state_flag {
            Flag::GenericParameter => table.create_generic_parameters(
                symbol_id,
                syntax_tree.generic_parameters().as_ref(),
                data,
                handler,
            ),
            Flag::WhereClause => {
                table.create_where_clause_predicates(
                    symbol_id,
                    syntax_tree.where_clause().as_ref(),
                    data,
                    handler,
                );
            }
            Flag::Complete => {
                for variant in table
                    .get(symbol_id)
                    .unwrap()
                    .variant_ids_by_name
                    .values()
                    .copied()
                {
                    let _ = table.build_to(
                        variant,
                        Some(symbol_id.into()),
                        super::variant::Flag::Complete,
                        true,
                        handler,
                    );
                }

                data.build_all_occurrences_to_completion(
                    table,
                    symbol_id.into(),
                    false,
                    handler,
                );

                // build the variance
                let premise =
                    table.get_active_premise(symbol_id.into()).unwrap();
                let mut enum_sym =
                    table.representation.enums.get(symbol_id).unwrap().write();
                let enum_sym = &mut *enum_sym;
                let mut session = session::Default::default();

                // simplify the type of variants
                for mut variant in
                    enum_sym.variant_ids_by_name.values().copied().map(|x| {
                        table.representation.variants.get(x).unwrap().write()
                    })
                {
                    if let Some(associated_ty) = &mut variant.associated_type {
                        if let Ok(simplified) = simplify::simplify(
                            associated_ty,
                            &Environment { premise: &premise, table },
                            &mut Limit::new(&mut session),
                        ) {
                            *associated_ty = simplified;
                        }
                    }
                }

                #[allow(clippy::needless_collect)]
                let type_usages = enum_sym
                    .variant_ids_by_name
                    .values()
                    .filter_map(|f| {
                        table.get(*f).unwrap().associated_type.clone()
                    })
                    .collect::<Vec<_>>();

                table.build_variance(
                    &enum_sym.generic_declaration.parameters,
                    &mut enum_sym.generic_parameter_variances,
                    &premise,
                    symbol_id.into(),
                    type_usages.iter(),
                    handler,
                );
            }
            Flag::Check => {
                table.check_occurrences(symbol_id.into(), data, handler);
            }
        }
    }
}
