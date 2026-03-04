use std::sync::Arc;

use pernixc_source_file::SourceElement;
use pernixc_symbol::kind::Kind;
use pernixc_syntax::item::instance::Member as InstanceMemberSyn;
use pernixc_tokio::join_set::JoinSet;

use crate::table::builder::{Builder, MemberBuilder};

impl Builder {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn handle_instance_member(
        self: &Arc<Self>,
        instance_syntax: &pernixc_syntax::item::instance::Instance,
        module_member_builder: &mut MemberBuilder,
        join_set: &mut JoinSet<()>,
    ) {
        let Some(signature) = instance_syntax.signature() else {
            return;
        };

        let Some(identifier) = signature.identifier() else {
            return;
        };

        let next_submodule_qualified_name = module_member_builder
            .extend_qualified_name_sequence(identifier.kind.0.clone());

        let instance_id = module_member_builder
            .add_member(identifier.clone(), self.engine())
            .await;

        let instance_body = instance_syntax.body();
        let members = instance_body
            .as_ref()
            .and_then(pernixc_syntax::item::Body::members);

        let access_modifier = instance_syntax.access_modifier();
        let generic_parameters = signature.generic_parameters();
        let where_clause = instance_body
            .and_then(|x| x.where_clause())
            .and_then(|x| x.predicates());

        let parent_module_id = module_member_builder.current_symbol_id();
        let instance_scope_span = instance_syntax.span();

        let builder = self.clone();

        self.insert_external_instance(
            instance_id,
            instance_syntax.extern_keyword().is_some(),
        );
        self.insert_instance_trait_ref(
            instance_id,
            instance_syntax.signature().and_then(|x| x.trait_ref()),
        );

        join_set.spawn(async move {
            let mut instance_member_builder = MemberBuilder::new(
                instance_id,
                next_submodule_qualified_name,
                builder.target_id(),
            );

            // add each of the member to the instance member
            for member in members
                .as_ref()
                .iter()
                .flat_map(|x| x.members())
                .filter_map(|x| x.into_line().ok())
            {
                match member {
                    InstanceMemberSyn::Type(member) => {
                        let Some(identifier) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = instance_member_builder
                            .add_member(identifier.clone(), builder.engine())
                            .await;

                        builder.insert_kind(id, Kind::InstanceAssociatedType);
                        builder.insert_scope_span(id, member.span());
                        builder.insert_name_identifier(id, &identifier);
                        builder.insert_generic_parameters_syntax(
                            id,
                            member
                                .signature()
                                .and_then(|x| x.generic_parameters()),
                        );
                        builder.insert_where_clause_syntax(
                            id,
                            member
                                .body()
                                .and_then(|x| x.trailing_where_clause())
                                .and_then(|x| x.where_clause())
                                .and_then(|x| x.predicates()),
                        );
                        builder.insert_type_alias_syntax(
                            id,
                            member.body().and_then(|x| x.r#type()),
                        );
                    }

                    InstanceMemberSyn::Function(member) => {
                        let Some(identifier) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = instance_member_builder
                            .add_member(identifier.clone(), builder.engine())
                            .await;

                        builder
                            .insert_kind(id, Kind::InstanceAssociatedFunction);
                        builder.insert_scope_span(id, member.span());
                        builder.insert_name_identifier(id, &identifier);
                        builder.insert_generic_parameters_syntax(
                            id,
                            member
                                .signature()
                                .and_then(|x| x.generic_parameters()),
                        );
                        builder.insert_where_clause_syntax(
                            id,
                            member
                                .body()
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
                        builder.insert_function_body_syntax(
                            id,
                            member.body().and_then(|x| x.members()),
                        );
                    }

                    InstanceMemberSyn::Constant(member) => {
                        let Some(identifier) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = instance_member_builder
                            .add_member(identifier.clone(), builder.engine())
                            .await;

                        builder
                            .insert_kind(id, Kind::InstanceAssociatedConstant);
                        builder.insert_scope_span(id, member.span());
                        builder.insert_name_identifier(id, &identifier);
                        builder.insert_generic_parameters_syntax(
                            id,
                            member
                                .signature()
                                .and_then(|x| x.generic_parameters()),
                        );
                        builder.insert_where_clause_syntax(
                            id,
                            member
                                .body()
                                .and_then(|x| x.trailing_where_clause())
                                .and_then(|x| x.where_clause())
                                .and_then(|x| x.predicates()),
                        );
                        builder.insert_constant_type_annotation_syntax(
                            id,
                            member.signature().and_then(|x| x.r#type()),
                        );
                        builder.insert_constant_expression_syntax(
                            id,
                            member.body().and_then(|x| x.expression()),
                        );
                    }

                    InstanceMemberSyn::Instance(inst) => {
                        let Some(identifier) =
                            inst.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = instance_member_builder
                            .add_member(identifier.clone(), builder.engine())
                            .await;

                        builder
                            .insert_kind(id, Kind::InstanceAssociatedInstance);
                        builder.insert_scope_span(id, inst.span());
                        builder.insert_name_identifier(id, &identifier);
                        builder.insert_generic_parameters_syntax(
                            id,
                            inst.signature()
                                .and_then(|x| x.generic_parameters()),
                        );
                        builder.insert_where_clause_syntax(
                            id,
                            inst.body()
                                .and_then(|x| x.trailing_where_clause())
                                .and_then(|x| x.where_clause())
                                .and_then(|x| x.predicates()),
                        );
                        builder.insert_instance_trait_ref(
                            id,
                            inst.signature().and_then(|x| x.trait_ref()),
                        );
                        builder.insert_instance_associated_value(
                            id,
                            inst.body()
                                .and_then(|x| x.value())
                                .and_then(|x| x.qualified_identifier()),
                        );
                    }
                }
            }

            builder.insert_kind(instance_id, Kind::Instance);
            builder.insert_scope_span(instance_id, instance_scope_span);
            builder.insert_name_identifier(instance_id, &identifier);
            builder.insert_generic_parameters_syntax(
                instance_id,
                generic_parameters,
            );
            builder.insert_where_clause_syntax(instance_id, where_clause);
            builder
                .insert_accessibility_by_access_modifier(
                    instance_id,
                    parent_module_id,
                    access_modifier.as_ref(),
                )
                .await;
            builder.insert_member_from_builder(
                instance_id,
                instance_member_builder,
            );
        });
    }
}
