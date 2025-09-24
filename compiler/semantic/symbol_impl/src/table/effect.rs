use std::sync::Arc;

use pernixc_symbol::{kind::Kind, member::Member};
use tokio::task::JoinHandle;

use crate::{
    diagnostic::Diagnostic,
    table::{Entry, MemberBuilder, Naming, TableContext},
};

impl TableContext {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn handle_effect_member(
        self: &Arc<Self>,
        effect_syntax: &pernixc_syntax::item::effect::Effect,
        module_member_builder: &mut MemberBuilder,
    ) -> Option<JoinHandle<()>> {
        let signature = effect_syntax.signature()?;
        let identifier = signature.identifier()?;

        let next_submodule_qualified_name = module_member_builder
            .symbol_qualified_name
            .iter()
            .cloned()
            .chain(std::iter::once(identifier.kind.0.clone()))
            .collect::<Arc<[_]>>();

        let effect_id = module_member_builder
            .add_member(identifier.clone(), &self.engine)
            .await;

        let effect_body = effect_syntax.body();
        let members =
            effect_body.as_ref().and_then(pernixc_syntax::item::Body::members);

        let access_modifier = effect_syntax.access_modifier();
        let generic_parameters = signature.generic_parameters();
        let where_clause = effect_body
            .and_then(|x| x.where_clause())
            .and_then(|x| x.predicates());

        let parent_module_id = module_member_builder.symbol_id;

        let context = self.clone();

        Some(tokio::spawn(async move {
            let mut effect_member_builder = MemberBuilder::new(
                effect_id,
                next_submodule_qualified_name,
                context.target_id,
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

                let entry = Entry::builder()
                    .kind(Kind::EffectOperation)
                    .naming(Naming::Identifier(identifier))
                    .function_signature_syntax((
                        member.parameters(),
                        member.return_type(),
                    ))
                    .build();

                let member_id = effect_member_builder
                    .add_member(
                        entry.naming.as_identifier().unwrap().clone(),
                        &context.engine,
                    )
                    .await;

                context.add_symbol_entry(member_id, effect_id, entry).await;
            }

            context
                .add_symbol_entry(
                    effect_id,
                    parent_module_id,
                    Entry::builder()
                        .kind(Kind::Effect)
                        .naming(Naming::Identifier(identifier))
                        .accessibility(access_modifier)
                        .generic_parameters_syntax(generic_parameters)
                        .where_clause_syntax(where_clause)
                        .member(Arc::new(Member {
                            member_ids_by_name: effect_member_builder
                                .member_ids_by_name,
                            unnameds: effect_member_builder.unnameds,
                        }))
                        .build(),
                )
                .await;

            context.storage.as_vec_mut().extend(
                effect_member_builder
                    .redefinition_errors
                    .into_iter()
                    .map(Diagnostic::ItemRedefinition),
            );
        }))
    }
}
