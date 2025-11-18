use std::sync::Arc;

use flexstr::SharedStr;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{calculate_implements_id, kind::Kind};
use tokio::task::JoinHandle;

use crate::table::builder::{Builder, MemberBuilder};

impl Builder {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn handle_implements(
        self: &Arc<Self>,
        implements_syntax: pernixc_syntax::item::implements::Implements,
        module_member_builder: &mut MemberBuilder,
    ) -> Option<JoinHandle<()>> {
        let signature = implements_syntax.signature()?;
        let qualified_identifier = signature.qualified_identifier()?;

        let body = implements_syntax.body();
        let qualified_identifier_span = qualified_identifier.span();

        let generic_parameters = signature.generic_parameters();
        let where_clause = match &body {
            Some(pernixc_syntax::item::implements::Body::Negative(
                negative_body,
            )) => negative_body
                .trailing_where_clause()
                .and_then(|x| x.where_clause())
                .and_then(|x| x.predicates()),

            Some(pernixc_syntax::item::implements::Body::Positive(body)) => {
                body.where_clause().and_then(|x| x.predicates())
            }

            None => None,
        };

        let implements_id = self
            .engine()
            .calculate_implements_id(
                &qualified_identifier_span,
                self.target_id(),
            )
            .await;
        let implements_qualified_name = self
            .implements_qualified_identifier_name(&qualified_identifier_span);
        let implements_final_keyword =
            implements_syntax.signature().and_then(|x| x.final_keyword());

        module_member_builder.insert_unnameds(implements_id);

        self.insert_name(
            implements_id,
            self.implements_qualified_identifier_name(
                &qualified_identifier_span,
            ),
        );
        self.insert_span(implements_id, Some(qualified_identifier_span));
        self.insert_generic_parameters_syntax(
            implements_id,
            generic_parameters,
        );
        self.insert_where_clause_syntax(implements_id, where_clause);
        self.insert_final_keyword(implements_id, implements_final_keyword);

        match body {
            Some(pernixc_syntax::item::implements::Body::Negative(_)) => {
                self.insert_kind(implements_id, Kind::NegativeImplementation);

                None
            }

            None => {
                self.insert_kind(implements_id, Kind::PositiveImplementation);
                self.insert_member(implements_id, Arc::default());

                None
            }

            Some(pernixc_syntax::item::implements::Body::Positive(body)) => {
                let builder = self.clone();
                let implements_name_sequence = module_member_builder
                    .extend_qualified_name_sequence(implements_qualified_name);

                Some(tokio::spawn(async move {
                    let member_builder = builder
                        .handle_positive_implementation(
                            implements_id,
                            implements_name_sequence,
                            &body,
                        )
                        .await;

                    builder.insert_member_from_builder(
                        implements_id,
                        member_builder,
                    );
                }))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn handle_positive_implementation(
        &self,
        implements_id: pernixc_symbol::ID,
        implements_qualified_identifier_sequence: Arc<[SharedStr]>,
        body: &pernixc_syntax::item::Body<
            pernixc_syntax::item::implements::Member,
        >,
    ) -> MemberBuilder {
        let mut impl_member_builder = MemberBuilder::new(
            implements_id,
            implements_qualified_identifier_sequence,
            self.target_id(),
        );

        if let Some(members) = body.members() {
            for member in members.members().filter_map(|x| x.into_line().ok()) {
                match member {
                    pernixc_syntax::item::implements::Member::Constant(con) => {
                        let Some(identifier) =
                            con.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = impl_member_builder
                            .add_member(identifier.clone(), self.engine())
                            .await;

                        self.insert_kind(id, Kind::ImplementationConstant);
                        self.insert_name_identifier(id, &identifier);
                        self.insert_implements_access_modifier_syntax(
                            id,
                            con.access_modifier(),
                        );
                        self.insert_generic_parameters_syntax(
                            id,
                            con.signature()
                                .and_then(|x| x.generic_parameters()),
                        );
                        self.insert_where_clause_syntax(
                            id,
                            con.body()
                                .and_then(|x| x.trailing_where_clause())
                                .and_then(|x| x.where_clause())
                                .and_then(|x| x.predicates()),
                        );
                        self.insert_constant_type_annotation_syntax(
                            id,
                            con.signature().and_then(|x| x.r#type()),
                        );
                        self.insert_constant_expression_syntax(
                            id,
                            con.body().and_then(|x| x.expression()),
                        );
                    }

                    pernixc_syntax::item::implements::Member::Function(fun) => {
                        let Some(identifier) = fun
                            .signature()
                            .and_then(|x| x.signature())
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = impl_member_builder
                            .add_member(identifier.clone(), self.engine())
                            .await;

                        self.insert_kind(id, Kind::ImplementationFunction);
                        self.insert_name_identifier(id, &identifier);
                        self.insert_implements_access_modifier_syntax(
                            id,
                            fun.access_modifier(),
                        );
                        self.insert_generic_parameters_syntax(
                            id,
                            fun.signature()
                                .and_then(|x| x.signature())
                                .and_then(|x| x.generic_parameters()),
                        );
                        self.insert_where_clause_syntax(
                            id,
                            fun.body()
                                .and_then(|x| x.where_clause())
                                .and_then(|x| x.predicates()),
                        );
                        self.insert_function_signature_syntax(
                            id,
                            fun.signature()
                                .and_then(|x| x.signature())
                                .and_then(|x| x.parameters()),
                            fun.signature()
                                .and_then(|x| x.signature())
                                .and_then(|x| x.return_type()),
                        );
                        self.insert_function_body_syntax(
                            id,
                            fun.body().and_then(|x| x.members()),
                        );
                        self.insert_function_effect_annotation_syntax(
                            id,
                            fun.signature()
                                .and_then(|x| x.signature())
                                .and_then(|x| x.effect_annotation()),
                        );
                        self.insert_function_unsafe_keyword(
                            id,
                            fun.signature()
                                .and_then(|x| x.signature())
                                .and_then(|x| x.unsafe_keyword()),
                        );
                    }

                    pernixc_syntax::item::implements::Member::Type(ty) => {
                        let Some(identifier) =
                            ty.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = impl_member_builder
                            .add_member(identifier.clone(), self.engine())
                            .await;

                        self.insert_kind(id, Kind::ImplementationType);
                        self.insert_name_identifier(id, &identifier);
                        self.insert_implements_access_modifier_syntax(
                            id,
                            ty.access_modifier(),
                        );
                        self.insert_generic_parameters_syntax(
                            id,
                            ty.signature().and_then(|x| x.generic_parameters()),
                        );
                        self.insert_where_clause_syntax(
                            id,
                            ty.body()
                                .and_then(|x| x.trailing_where_clause())
                                .and_then(|x| x.where_clause())
                                .and_then(|x| x.predicates()),
                        );
                        self.insert_type_alias_syntax(
                            id,
                            ty.body().and_then(|x| x.r#type()),
                        );
                    }
                }
            }
        }

        impl_member_builder
    }
}
