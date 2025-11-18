use std::sync::Arc;

use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::Kind,
    linkage::{self, C},
};

use crate::table::builder::{Builder, MemberBuilder};

impl Builder {
    pub(super) async fn handle_extern(
        self: &Arc<Self>,
        extern_syn: &pernixc_syntax::item::r#extern::Extern,
        module_member_builder: &mut MemberBuilder,
    ) {
        let Some(body) = extern_syn.body() else {
            return;
        };

        let Some(convention) = extern_syn.convention() else {
            return;
        };

        let linkage = match convention.kind.as_str() {
            "C" | "c" => linkage::Linkage::C(C { variadic: false }),
            _ => linkage::Linkage::Unknown,
        };

        for function_syntax in
            body.functions().filter_map(|x| x.into_line().ok())
        {
            let Some(identifier) =
                function_syntax.signature().and_then(|x| x.identifier())
            else {
                continue;
            };

            let linkage = match linkage {
                linkage::Linkage::C(mut c) => {
                    c.variadic = function_syntax
                        .signature()
                        .and_then(|x| x.parameters())
                        .is_some_and(|x| {
                            x.parameters().any(|x| x.is_variadic())
                        });

                    linkage::Linkage::C(c)
                }

                linkage::Linkage::Unknown => linkage::Linkage::Unknown,
            };

            let id = module_member_builder
                .add_member(identifier.clone(), self.engine())
                .await;

            self.insert_kind(id, Kind::ExternFunction);
            self.insert_scope_span(id, function_syntax.span());
            self.insert_name_identifier(id, &identifier);
            self.insert_accessibility_by_access_modifier(
                id,
                module_member_builder.current_symbol_id(),
                function_syntax.access_modifier().as_ref(),
            )
            .await;
            self.insert_generic_parameters_syntax(
                id,
                function_syntax
                    .signature()
                    .and_then(|x| x.generic_parameters()),
            );
            self.insert_where_clause_syntax(
                id,
                function_syntax
                    .trailing_where_clause()
                    .and_then(|x| x.where_clause())
                    .and_then(|x| x.predicates()),
            );
            self.insert_function_signature_syntax(
                id,
                function_syntax.signature().and_then(|x| x.parameters()),
                function_syntax.signature().and_then(|x| x.return_type()),
            );
            self.insert_function_effect_annotation_syntax(
                id,
                function_syntax.signature().and_then(|x| x.effect_annotation()),
            );
            self.insert_function_linkage(id, linkage);
        }
    }
}
