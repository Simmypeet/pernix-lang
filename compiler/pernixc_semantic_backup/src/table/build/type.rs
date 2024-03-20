use super::{Build, ResolutionConfigChain};
use crate::{
    semantic::predicate::{Predicate, TypeOutlives},
    symbol::{self, Type},
    table::{
        state::{r#type::Flag, Config},
        IndexMut, Table,
    },
};

impl Build for Type {
    fn build(
        mut config: Config<Self>,
        data: &mut Self::Data,
        build_flag: Self::Flag,
    ) {
        let table = config.table();
        let handler = config.handler();
        let id = config.current_id();
        let syntax_tree = config.syntax_tree();

        // change the build flag
        data.set_curret_flag(build_flag);

        match build_flag {
            Flag::Drafted => unreachable!("should not be called"),

            Flag::GenericParameter => {
                table.create_generic_parameters(
                    id,
                    config
                        .syntax_tree()
                        .signature()
                        .generic_parameters()
                        .as_ref(),
                    &mut ResolutionConfigChain::new(&mut config, data),
                    handler,
                );
            }

            Flag::Body => {
                let ty = table
                    .resolve_type(
                        syntax_tree.definition().ty(),
                        id.into(),
                        &mut ResolutionConfigChain::new(&mut config, data),
                        handler,
                    )
                    .unwrap_or_default();

                table.get_mut(id).unwrap().r#type = ty;
            }

            Flag::WhereClauseAndCheck => {
                // only `type` declaration can do implied boud

                for resolution in
                    data.resolution.values().flatten().map(|x| &x.resolution)
                {
                    let predicates =
                        Table::get_required_predicates(&config, resolution);
                    table
                        .get_mut(id)
                        .unwrap()
                        .generic_declaration
                        .predicates
                        .extend(predicates.into_iter().map(|x| {
                            symbol::Predicate {
                                predicate: x,
                                span: None,
                                explicit: false,
                            }
                        }));
                }

                for reference_ty in data
                    .types
                    .values()
                    .flatten()
                    .filter_map(|x| x.r#type.as_reference())
                {
                    let predicate = Predicate::TypeOutlives(TypeOutlives {
                        operand: (*reference_ty.pointee).clone(),
                        argument: reference_ty.lifetime,
                    });

                    table
                        .get_mut(id)
                        .unwrap()
                        .generic_declaration
                        .predicates
                        .push(symbol::Predicate {
                            predicate,
                            span: None,
                            explicit: false,
                        });
                }
            }
        }
    }
}
