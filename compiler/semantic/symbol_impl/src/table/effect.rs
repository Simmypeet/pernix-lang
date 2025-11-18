use std::sync::Arc;

use pernixc_symbol::kind::Kind;
use tokio::task::JoinHandle;

use crate::table::builder::{Builder, MemberBuilder};

impl Builder {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn handle_effect_member(
        self: &Arc<Self>,
        effect_syntax: &pernixc_syntax::item::effect::Effect,
        module_member_builder: &mut MemberBuilder,
    ) -> Option<JoinHandle<()>> {
        let signature = effect_syntax.signature()?;
        let identifier = signature.identifier()?;

        let next_submodule_qualified_name = module_member_builder
            .extend_qualified_name_sequence(identifier.kind.0.clone());

        let effect_id = module_member_builder
            .add_member(identifier.clone(), self.engine())
            .await;

        let effect_body = effect_syntax.body();
        let members =
            effect_body.as_ref().and_then(pernixc_syntax::item::Body::members);

        let access_modifier = effect_syntax.access_modifier();
        let generic_parameters = signature.generic_parameters();
        let where_clause = effect_body
            .and_then(|x| x.where_clause())
            .and_then(|x| x.predicates());

        let parent_module_id = module_member_builder.current_symbol_id();

        let builder = self.clone();

        Some(tokio::spawn(async move {
            let mut effect_member_builder = MemberBuilder::new(
                effect_id,
                next_submodule_qualified_name,
                builder.target_id(),
            );

            // add each of the member to the effect member
            for member in members
                .as_ref()
                .iter()
                .flat_map(|x| x.members())
                .filter_map(|x| x.into_line().ok())
            {
                let Some(identifier) = member.identifier() else {
                    continue;
                };

                let operation_id = effect_member_builder
                    .add_member(identifier.clone(), builder.engine())
                    .await;

                builder.insert_kind(operation_id, Kind::EffectOperation);
                builder.insert_name_identifier(operation_id, &identifier);
                builder.insert_generic_parameters_syntax(
                    operation_id,
                    member.generic_parameters(),
                );
                builder.insert_function_signature_syntax(
                    operation_id,
                    member.parameters(),
                    member.return_type(),
                );
                builder.insert_where_clause_syntax(
                    operation_id,
                    member
                        .trailing_where_clause()
                        .and_then(|x| x.where_clause())
                        .and_then(|x| x.predicates()),
                );
            }

            builder.insert_kind(effect_id, Kind::EffectOperation);
            builder.insert_name_identifier(effect_id, &identifier);
            builder.insert_generic_parameters_syntax(
                effect_id,
                generic_parameters,
            );
            builder.insert_where_clause_syntax(effect_id, where_clause);
            builder
                .insert_member_from_builder(effect_id, effect_member_builder);
            builder
                .insert_accessibility_by_access_modifier(
                    effect_id,
                    parent_module_id,
                    access_modifier.as_ref(),
                )
                .await;
        }))
    }
}
