//! Offers completion suggestions when user completing qualified identifier

use log::info;
use pernixc_diagnostic::ByteIndex;
use pernixc_extend::extend;
use pernixc_semantic_element::import::get_import_map;
use pernixc_source_file::{GlobalSourceID, SourceElement};
use pernixc_symbol::{
    kind::get_kind,
    member::{get_members, try_get_members},
    parent::get_closest_module_id,
    source_file_module::get_source_file_module,
};
use pernixc_target::TargetID;
use tower_lsp::lsp_types::CompletionItemKind;

use crate::pointing::{
    self, get_symbol_scope_at_byte_index, resolve_qualified_identifier_path,
};

#[extend]
fn allow_completion(self: pernixc_symbol::kind::Kind) -> bool {
    match self {
        pernixc_symbol::kind::Kind::Module
        | pernixc_symbol::kind::Kind::Struct
        | pernixc_symbol::kind::Kind::Trait
        | pernixc_symbol::kind::Kind::Enum
        | pernixc_symbol::kind::Kind::Type
        | pernixc_symbol::kind::Kind::Constant
        | pernixc_symbol::kind::Kind::Function
        | pernixc_symbol::kind::Kind::ExternFunction
        | pernixc_symbol::kind::Kind::Variant
        | pernixc_symbol::kind::Kind::TraitType
        | pernixc_symbol::kind::Kind::TraitFunction
        | pernixc_symbol::kind::Kind::TraitConstant
        | pernixc_symbol::kind::Kind::Effect
        | pernixc_symbol::kind::Kind::EffectOperation
        | pernixc_symbol::kind::Kind::Marker
        | pernixc_symbol::kind::Kind::ImplementationType
        | pernixc_symbol::kind::Kind::ImplementationFunction
        | pernixc_symbol::kind::Kind::ImplementationConstant => true,

        pernixc_symbol::kind::Kind::PositiveImplementation
        | pernixc_symbol::kind::Kind::NegativeImplementation => false,
    }
}

#[extend]
fn kind_to_completion_item_kind(
    self: pernixc_symbol::kind::Kind,
) -> CompletionItemKind {
    match self {
        pernixc_symbol::kind::Kind::Module => CompletionItemKind::MODULE,
        pernixc_symbol::kind::Kind::Struct => CompletionItemKind::STRUCT,

        pernixc_symbol::kind::Kind::Trait
        | pernixc_symbol::kind::Kind::Effect
        | pernixc_symbol::kind::Kind::Marker => CompletionItemKind::INTERFACE,

        pernixc_symbol::kind::Kind::Enum => CompletionItemKind::ENUM,
        pernixc_symbol::kind::Kind::Type => CompletionItemKind::TYPE_PARAMETER,

        pernixc_symbol::kind::Kind::Constant => CompletionItemKind::CONSTANT,

        pernixc_symbol::kind::Kind::Function
        | pernixc_symbol::kind::Kind::ExternFunction => {
            CompletionItemKind::FUNCTION
        }

        pernixc_symbol::kind::Kind::Variant => CompletionItemKind::ENUM_MEMBER,

        pernixc_symbol::kind::Kind::TraitType
        | pernixc_symbol::kind::Kind::ImplementationType => {
            CompletionItemKind::TYPE_PARAMETER
        }

        pernixc_symbol::kind::Kind::TraitConstant
        | pernixc_symbol::kind::Kind::ImplementationConstant => {
            CompletionItemKind::CONSTANT
        }

        pernixc_symbol::kind::Kind::TraitFunction
        | pernixc_symbol::kind::Kind::EffectOperation
        | pernixc_symbol::kind::Kind::ImplementationFunction => {
            CompletionItemKind::METHOD
        }

        pernixc_symbol::kind::Kind::PositiveImplementation
        | pernixc_symbol::kind::Kind::NegativeImplementation => {
            CompletionItemKind::CLASS
        }
    }
}

/// Provides completion suggestions for qualified identifiers at the given
/// byte index.
#[extend]
#[allow(clippy::too_many_lines)]
pub async fn qualified_identifier_completion(
    self: &pernixc_query::TrackedEngine,
    byte_index: ByteIndex,
    source_id: GlobalSourceID,
    syntax_tree: pernixc_syntax::item::module::Content,
    token_tree: &pernixc_lexical::tree::Tree,
    target_id: TargetID,
    completions: &mut Vec<tower_lsp::lsp_types::CompletionItem>,
) -> Result<(), pernixc_query::runtime::executor::CyclicError> {
    let module =
        target_id.make_global(self.get_source_file_module(source_id).await);

    // get the current symbol scope at the byte index for symbol resolution
    let current_site = self
        .get_symbol_scope_at_byte_index(module, source_id, byte_index)
        .await;

    let node = pernixc_parser::concrete_tree::Node::Branch(
        syntax_tree.inner_tree().clone(),
    );

    let (Some(qualified_identifier), token) = (
        node.get_deepest_ast::<pernixc_syntax::QualifiedIdentifier>(
            token_tree, byte_index,
        )
        .or_else(|| {
            // try again by checking one byte before, to handle the case
            // where the cursor is at the end of the identifier
            byte_index.checked_sub(1).and_then(|byte_index| {
                node.get_deepest_ast::<pernixc_syntax::QualifiedIdentifier>(
                    token_tree, byte_index,
                )
            })
        }),
        node.get_pointing_token(token_tree, byte_index),
    ) else {
        info!(" No qualified identifier found at byte index {byte_index} ");
        return Ok(());
    };

    let token_abs_span =
        token.map(|token| token_tree.absolute_span_of(&token.span));

    let Some(resolved_path) = self
        .resolve_qualified_identifier_path(
            current_site,
            &qualified_identifier,
            token_abs_span,
        )
        .await?
    else {
        info!(
            " Qualified identifier at byte index {byte_index} could not be \
             resolved "
        );
        return Ok(());
    };

    info!(
        " Qualified identifier at byte index {byte_index} resolved to \
         {resolved_path:?} "
    );

    // determine the nearest module id for import suggestions
    let nearest_module =
        target_id.make_global(self.get_closest_module_id(current_site).await);

    let prior_scope = match resolved_path {
        pointing::Resolution::Success(success_resolution) => {
            success_resolution.parent_scope
        }
        pointing::Resolution::FailAtCursor(fail_at_cursor) => fail_at_cursor,
        pointing::Resolution::FailedAtLastSegment(_)
        | pointing::Resolution::NoMatchFound(_) => {
            // check if the cursor is at the end of the qualified identifier,
            // only then suggest members of the symbol
            let qual_span = qualified_identifier.span();
            let qual_abs_span = token_tree.absolute_span_of(&qual_span);

            if byte_index == qual_abs_span.end {
                match resolved_path {
                    pointing::Resolution::FailedAtLastSegment(global) => global,
                    pointing::Resolution::NoMatchFound(global) => Some(global),

                    pointing::Resolution::Success(_)
                    | pointing::Resolution::FailAtCursor(_) => {
                        unreachable!()
                    }
                }
            } else {
                // cursor is not at the end, no suggestions
                return Ok(());
            }
        }
    };

    if let Some(symbol) = prior_scope {
        // suggest members of the resolved symbol
        let Some(members) = self.try_get_members(symbol).await else {
            return Ok(());
        };

        for (member_name, id) in &members.member_ids_by_name {
            let kind = self.get_kind(target_id.make_global(*id)).await;

            if !kind.allow_completion() {
                continue;
            }

            completions.push(tower_lsp::lsp_types::CompletionItem {
                label: member_name.to_string(),
                kind: Some(kind.kind_to_completion_item_kind()),
                sort_text: Some(format!("10_member_{member_name}")),
                ..Default::default()
            });
        }
    } else {
        // suggest module members, imports, this keyword, target keyword,
        // etc.

        let module_members = self.get_members(nearest_module).await;
        let module_imports = self.get_import_map(nearest_module).await;

        for (member_name, id) in &module_members.member_ids_by_name {
            let kind =
                self.get_kind(nearest_module.target_id.make_global(*id)).await;

            if !kind.allow_completion() {
                continue;
            }

            completions.push(tower_lsp::lsp_types::CompletionItem {
                label: member_name.to_string(),
                kind: Some(kind.kind_to_completion_item_kind()),
                sort_text: Some(format!("10_member_{member_name}")),
                ..Default::default()
            });
        }

        for (import_name, import) in module_imports.iter() {
            let kind = self.get_kind(import.id).await;
            if !kind.allow_completion() {
                continue;
            }

            completions.push(tower_lsp::lsp_types::CompletionItem {
                label: import_name.to_string(),
                kind: Some(kind.kind_to_completion_item_kind()),
                sort_text: Some(format!("20_import_{import_name}")),
                ..Default::default()
            });
        }

        completions.push(tower_lsp::lsp_types::CompletionItem {
            label: "this".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            sort_text: Some("30_qual_this".to_string()),
            ..Default::default()
        });

        completions.push(tower_lsp::lsp_types::CompletionItem {
            label: "target".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            sort_text: Some("30_qual_target".to_string()),
            ..Default::default()
        });
    }

    Ok(())
}
