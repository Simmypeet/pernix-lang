use std::sync::Arc;

use pernixc_symbol::kind::Kind;
use tokio::task::JoinHandle;

use crate::table::builder::{Builder, MemberBuilder};

impl Builder {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn handle_enum_member(
        self: &Arc<Self>,
        enum_syntax: &pernixc_syntax::item::r#enum::Enum,
        module_member_builder: &mut MemberBuilder,
    ) -> Option<JoinHandle<()>> {
        let signature = enum_syntax.signature()?;
        let identifier = signature.identifier()?;

        let next_submodule_qualified_name = module_member_builder
            .extend_qualified_name_sequence(identifier.kind.0.clone());

        let enum_id = module_member_builder
            .add_member(identifier.clone(), self.engine())
            .await;

        let enum_body = enum_syntax.body();
        let members =
            enum_body.as_ref().and_then(pernixc_syntax::item::Body::members);

        let access_modifier = enum_syntax.access_modifier();
        let generic_parameters = signature.generic_parameters();
        let where_clause = enum_body
            .and_then(|x| x.where_clause())
            .and_then(|x| x.predicates());

        let parent_module_id = module_member_builder.current_symbol_id();

        let builder = self.clone();

        Some(tokio::spawn(async move {
            let mut enum_member_builder = MemberBuilder::new(
                enum_id,
                next_submodule_qualified_name,
                builder.target_id(),
            );

            // add each of the member to the trait member
            for (order, variant) in members
                .as_ref()
                .iter()
                .flat_map(|x| x.members())
                .filter_map(|x| x.into_line().ok())
                .enumerate()
            {
                let Some(identifier) = variant.identifier() else {
                    continue;
                };

                let variant_id = enum_member_builder
                    .add_member(identifier.clone(), builder.engine())
                    .await;

                builder.insert_kind(variant_id, Kind::Variant);
                builder.insert_name_identifier(variant_id, &identifier);
                builder.insert_variant_declaration_order(variant_id, order);
                builder.insert_variant_associated_type_syntax(
                    variant_id,
                    variant.association().and_then(|x| x.r#type()),
                );
            }

            builder.insert_kind(enum_id, Kind::Enum);
            builder.insert_name_identifier(enum_id, &identifier);
            builder.insert_accessibility_by_access_modifier(
                enum_id,
                parent_module_id,
                access_modifier.as_ref(),
            );
            builder
                .insert_generic_parameters_syntax(enum_id, generic_parameters);
            builder.insert_where_clause_syntax(enum_id, where_clause);
            builder.insert_member(enum_id, enum_member_builder);
        }))
    }
}
