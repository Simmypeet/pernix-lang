//! Handles the implementation of hover functionality for the LSP server.

use pernixc_extend::extend;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_symbol::{kind::get_kind, name::get_qualified_name};
use pernixc_target::TargetID;
use tower_lsp::lsp_types::MarkupContent;

use crate::{
    hover::{
        r#enum::format_enum_signature, r#struct::format_struct_signature,
        r#type::format_type_signature,
    },
    pointing::symbol_at,
};

pub mod accessibility;
pub mod r#enum;
pub mod function;
pub mod generic_parameters;
pub mod markdown;
pub mod r#struct;
pub mod r#type;

/// Handles hover requests from the LSP client.
#[extend]
pub async fn handle_hover(
    self: &TrackedEngine,
    target_id: TargetID,
    params: tower_lsp::lsp_types::HoverParams,
) -> Result<Option<tower_lsp::lsp_types::Hover>, CyclicError> {
    let Some(symbol) = self
        .symbol_at(
            &params.text_document_position_params.position,
            &params.text_document_position_params.text_document.uri,
            target_id,
        )
        .await?
    else {
        return Ok(None);
    };

    let kind = self.get_kind(symbol).await;
    Ok(Some(tower_lsp::lsp_types::Hover {
        contents: tower_lsp::lsp_types::HoverContents::Markup(MarkupContent {
            kind: tower_lsp::lsp_types::MarkupKind::Markdown,
            value: match kind {
                pernixc_symbol::kind::Kind::Enum => {
                    self.format_enum_signature(symbol).await?
                }

                pernixc_symbol::kind::Kind::Struct => {
                    self.format_struct_signature(symbol).await?
                }

                pernixc_symbol::kind::Kind::Type
                | pernixc_symbol::kind::Kind::ImplementationType => {
                    self.format_type_signature(symbol, true).await?
                }

                pernixc_symbol::kind::Kind::TraitType => {
                    self.format_type_signature(symbol, false).await?
                }

                pernixc_symbol::kind::Kind::Module
                | pernixc_symbol::kind::Kind::Trait
                | pernixc_symbol::kind::Kind::Constant
                | pernixc_symbol::kind::Kind::Function
                | pernixc_symbol::kind::Kind::ExternFunction
                | pernixc_symbol::kind::Kind::Variant
                | pernixc_symbol::kind::Kind::TraitFunction
                | pernixc_symbol::kind::Kind::TraitConstant
                | pernixc_symbol::kind::Kind::Effect
                | pernixc_symbol::kind::Kind::EffectOperation
                | pernixc_symbol::kind::Kind::Marker
                | pernixc_symbol::kind::Kind::PositiveImplementation
                | pernixc_symbol::kind::Kind::NegativeImplementation
                | pernixc_symbol::kind::Kind::ImplementationFunction
                | pernixc_symbol::kind::Kind::ImplementationConstant => {
                    format!(
                        "```pnx\n{} {}\n```",
                        kind.kind_str(),
                        self.get_qualified_name(symbol).await
                    )
                }
            },
        }),
        range: None,
    }))
}
