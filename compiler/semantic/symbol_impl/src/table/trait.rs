use std::sync::Arc;

use pernixc_source_file::SourceElement;
use pernixc_symbol::kind::Kind;
use pernixc_syntax::item::r#trait::Member as TraitMemberSyn;
use pernixc_tokio::join_set::JoinSet;

use crate::table::builder::{Builder, MemberBuilder};

impl Builder {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn handle_trait_member(
        self: &Arc<Self>,
        trait_syntax: &pernixc_syntax::item::r#trait::Trait,
        module_member_builder: &mut MemberBuilder,
        join_set: &mut JoinSet<()>,
    ) {
        let Some(signature) = trait_syntax.signature() else {
            return;
        };

        let Some(identifier) = signature.identifier() else {
            return;
        };

        let next_submodule_qualified_name = module_member_builder
            .extend_qualified_name_sequence(identifier.kind.0.clone());

        let trait_id = module_member_builder
            .add_member(identifier.clone(), self.engine())
            .await;

        let trait_body = trait_syntax.body();
        let members =
            trait_body.as_ref().and_then(pernixc_syntax::item::Body::members);

        let access_modifier = trait_syntax.access_modifier();
        let generic_parameters = signature.generic_parameters();
        let where_clause = trait_body
            .and_then(|x| x.where_clause())
            .and_then(|x| x.predicates());

        let parent_module_id = module_member_builder.current_symbol_id();
        let trait_scope_span = trait_syntax.span();

        let builder = self.clone();

        join_set.spawn(async move {
            let mut trait_member_builder = MemberBuilder::new(
                trait_id,
                next_submodule_qualified_name,
                builder.target_id(),
            );

            // add each of the member to the trait member
            for member in members
                .as_ref()
                .iter()
                .flat_map(|x| x.members())
                .filter_map(|x| x.into_line().ok())
            {
                match member {
                    TraitMemberSyn::Type(member) => {
                        let Some(identifier) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = trait_member_builder
                            .add_member(identifier.clone(), builder.engine())
                            .await;

                        builder.insert_kind(id, Kind::TraitType);
                        builder.insert_scope_span(id, member.span());
                        builder.insert_name_identifier(id, &identifier);
                        builder
                            .insert_accessibility_by_access_modifier(
                                id,
                                parent_module_id,
                                member.access_modifier().as_ref(),
                            )
                            .await;
                        builder.insert_generic_parameters_syntax(
                            id,
                            member
                                .signature()
                                .and_then(|x| x.generic_parameters()),
                        );
                        builder.insert_where_clause_syntax(
                            id,
                            member
                                .trailing_where_clause()
                                .and_then(|x| x.where_clause())
                                .and_then(|x| x.predicates()),
                        );
                    }

                    TraitMemberSyn::Function(member) => {
                        let Some(identifier) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = trait_member_builder
                            .add_member(identifier.clone(), builder.engine())
                            .await;

                        builder.insert_kind(id, Kind::TraitFunction);
                        builder.insert_scope_span(id, member.span());
                        builder.insert_name_identifier(id, &identifier);
                        builder
                            .insert_accessibility_by_access_modifier(
                                id,
                                parent_module_id,
                                member.access_modifier().as_ref(),
                            )
                            .await;
                        builder.insert_generic_parameters_syntax(
                            id,
                            member
                                .signature()
                                .and_then(|x| x.generic_parameters()),
                        );
                        builder.insert_where_clause_syntax(
                            id,
                            member
                                .trailing_where_clause()
                                .and_then(|x| x.where_clause())
                                .and_then(|x| x.predicates()),
                        );
                        builder.insert_function_signature_syntax(
                            id,
                            member.signature().and_then(|x| x.parameters()),
                            member.signature().and_then(|x| x.return_type()),
                        );
                        builder.insert_function_effect_annotation_syntax(
                            id,
                            member
                                .signature()
                                .and_then(|x| x.effect_annotation()),
                        );
                        builder.insert_function_unsafe_keyword(
                            id,
                            member.signature().and_then(|x| x.unsafe_keyword()),
                        );
                    }

                    TraitMemberSyn::Constant(member) => {
                        let Some(identifier) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = trait_member_builder
                            .add_member(identifier.clone(), builder.engine())
                            .await;

                        builder.insert_kind(id, Kind::TraitConstant);
                        builder.insert_scope_span(id, member.span());
                        builder.insert_name_identifier(id, &identifier);
                        builder
                            .insert_accessibility_by_access_modifier(
                                id,
                                parent_module_id,
                                member.access_modifier().as_ref(),
                            )
                            .await;
                        builder.insert_generic_parameters_syntax(
                            id,
                            member
                                .signature()
                                .and_then(|x| x.generic_parameters()),
                        );
                        builder.insert_where_clause_syntax(
                            id,
                            member
                                .trailing_where_clause()
                                .and_then(|x| x.where_clause())
                                .and_then(|x| x.predicates()),
                        );
                        builder.insert_constant_type_annotation_syntax(
                            id,
                            member.signature().and_then(|x| x.r#type()),
                        );
                    }
                }
            }

            builder.insert_kind(trait_id, Kind::Trait);
            builder.insert_scope_span(trait_id, trait_scope_span);
            builder.insert_name_identifier(trait_id, &identifier);
            builder
                .insert_generic_parameters_syntax(trait_id, generic_parameters);
            builder.insert_where_clause_syntax(trait_id, where_clause);
            builder
                .insert_accessibility_by_access_modifier(
                    trait_id,
                    parent_module_id,
                    access_modifier.as_ref(),
                )
                .await;
            builder.insert_member_from_builder(trait_id, trait_member_builder);
        });
    }
}
