//! Handles the implementation of hover functionality for the LSP server.

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{kind::get_kind, name::get_qualified_name};
use pernixc_target::TargetID;

use crate::{
    hover::{
        r#enum::format_enum_signature, function::format_function_signature,
        module::format_module_signature,
        simple_signature_with_where_clause::format_simple_signature_with_where_clause,
        r#struct::format_struct_signature, r#type::format_type_signature,
        variant::format_variant_signature,
    },
    pointing::symbol_at,
};

mod associate_symbols;

mod r#enum;
mod function;
mod markdown;
mod module;
mod simple_signature_with_where_clause;
mod r#struct;
mod r#type;
mod variant;

/// Handles hover requests from the LSP client.
#[extend]
pub async fn handle_hover(
    self: &TrackedEngine,
    target_id: TargetID,
    params: tower_lsp::lsp_types::HoverParams,
) -> Option<String> {
    let symbol = self
        .symbol_at(
            &params.text_document_position_params.position,
            &params.text_document_position_params.text_document.uri,
            target_id,
        )
        .await?;

    let kind = self.get_kind(symbol).await;
    Some(match kind {
        pernixc_symbol::kind::Kind::Enum => {
            self.format_enum_signature(symbol).await
        }

        pernixc_symbol::kind::Kind::Struct => {
            self.format_struct_signature(symbol).await
        }

        pernixc_symbol::kind::Kind::Type
        | pernixc_symbol::kind::Kind::ImplementationType
        | pernixc_symbol::kind::Kind::TraitType => {
            self.format_type_signature(symbol).await
        }

        pernixc_symbol::kind::Kind::Function
        | pernixc_symbol::kind::Kind::ExternFunction
        | pernixc_symbol::kind::Kind::TraitFunction
        | pernixc_symbol::kind::Kind::ImplementationFunction => {
            self.format_function_signature(
                symbol,
                symbol.target_id == target_id,
            )
            .await
        }

        pernixc_symbol::kind::Kind::Variant => {
            self.format_variant_signature(symbol).await
        }

        pernixc_symbol::kind::Kind::Trait => {
            self.format_simple_signature_with_where_clause(symbol, "trait")
                .await
        }

        pernixc_symbol::kind::Kind::Marker => {
            self.format_simple_signature_with_where_clause(symbol, "marker")
                .await
        }

        pernixc_symbol::kind::Kind::Effect => {
            self.format_simple_signature_with_where_clause(symbol, "effect")
                .await
        }

        pernixc_symbol::kind::Kind::Module => {
            self.format_module_signature(symbol).await
        }

        pernixc_symbol::kind::Kind::EffectOperation
        | pernixc_symbol::kind::Kind::Constant
        | pernixc_symbol::kind::Kind::TraitConstant
        | pernixc_symbol::kind::Kind::PositiveImplementation
        | pernixc_symbol::kind::Kind::NegativeImplementation
        | pernixc_symbol::kind::Kind::ImplementationConstant => {
            format!(
                "```pnx\n{} {}\n```",
                kind.kind_str(),
                self.get_qualified_name(symbol).await
            )
        }
    })
}
