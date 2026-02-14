//! Handles the implementation of resolving the symbol being pointed at in the
//! LSP server.

use std::path::Path;

use pernixc_extend::extend;
use pernixc_parser::concrete_tree;
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::qualified_identifier::{
    resolve_in, resolve_simple_qualified_identifier_root,
};
use pernixc_source_file::{
    ByteIndex, GlobalSourceID, SourceElement, Span, get_source_file_by_id,
    get_stable_path_id,
};
use pernixc_symbol::{
    member::try_get_members, scope_span::get_scope_span,
    source_file_module::get_source_file_module, source_map::to_absolute_span,
};
use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::{Global, TargetID};
use qbice::storage::intern::Interned;
use tower_lsp::lsp_types;

use crate::conversion::to_pernix_editor_location;

/// Returns a tuple of the qualified identifier and its span that points to the
/// given byte index in the source file.
#[extend]
pub async fn get_pointing_qualified_identifier(
    self: &TrackedEngine,
    target_id: TargetID,
    source_file_path: Interned<Path>,
    byte_index: ByteIndex,
) -> Option<(QualifiedIdentifier, Span<ByteIndex>)> {
    let Ok((Some(syntax), _)) = self
        .query(&pernixc_syntax::Key {
            path: source_file_path.clone(),
            target_id,
        })
        .await
    else {
        return None;
    };

    let Ok((token_tree, _)) = self
        .query(&pernixc_lexical::Key {
            path: source_file_path.clone(),
            target_id,
        })
        .await
    else {
        return None;
    };

    // get the root concrete node
    let node = concrete_tree::Node::Branch(syntax.inner_tree().clone());

    // resolve the qualified name
    let pointing_token = node.get_pointing_token(&token_tree, byte_index)?;
    let qualified_name = node
        .get_deepest_ast::<pernixc_syntax::QualifiedIdentifier>(
            &token_tree,
            byte_index,
        )?;

    let token_abs_span = token_tree.absolute_span_of(&pointing_token.span);

    // check if the pointing token is actually a part of the qualified name
    for part in qualified_name
        .root()
        .into_iter()
        .filter_map(|x| match x {
            pernixc_syntax::QualifiedIdentifierRoot::Target(token)
            | pernixc_syntax::QualifiedIdentifierRoot::This(token) => {
                Some(token.span)
            }
            pernixc_syntax::QualifiedIdentifierRoot::GenericIdentifier(
                generic_identifier,
            ) => generic_identifier.identifier().map(|x| x.span),
        })
        .chain(qualified_name.subsequences().filter_map(|x| {
            x.generic_identifier().and_then(|x| x.identifier()).map(|x| x.span)
        }))
    {
        let abs_span = token_tree.absolute_span_of(&part);

        if abs_span == token_abs_span {
            return Some((qualified_name.clone(), token_abs_span));
        }
    }

    None
}

/// The resolution succeeded at the cursor position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SuccessResolution {
    /// The symbol being pointed at.
    pub pointing_symbol: Global<pernixc_symbol::ID>,

    /// The parent scope of the pointing symbol (None if the cursor is pointing
    /// to the first root of the qualified identifier).
    pub parent_scope: Option<Global<pernixc_symbol::ID>>,
}

/// The resolution failed before reaching or at the cursor position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Failed {
    /// The prior scope before the failure (None if failed at the first root).
    pub prior_scope: Option<Global<pernixc_symbol::ID>>,

    /// Whether the failure occurred exactly at the cursor position.
    ///
    /// This is beneficial to determine whether to suggest completions or not.
    pub fail_at_cursor: bool,
}

/// The result of resolving a qualified identifier at the cursor position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Resolution {
    Success(SuccessResolution),
    Failed(Failed),

    /// The symbol resolve to the end without matching the cursor position.
    NoMatchFound(Global<pernixc_symbol::ID>),
}

/// Resolves a qualified identifier to its symbol ID.
#[extend]
pub async fn resolve_qualified_identifier_path(
    self: &TrackedEngine,
    current_site: Global<pernixc_symbol::ID>,
    qualified_identifier: &QualifiedIdentifier,
    pointing_span: Option<Span<ByteIndex>>,
) -> Option<Resolution> {
    let root_syn = qualified_identifier.root()?;

    let Some(root) = self
        .resolve_simple_qualified_identifier_root(current_site, &root_syn)
        .await
    else {
        let root_span = self.to_absolute_span(&root_syn.span()).await;

        return Some(Resolution::Failed(Failed {
            prior_scope: None,
            fail_at_cursor: Some(root_span) == pointing_span,
        }));
    };

    let root_match = match root_syn {
        pernixc_syntax::QualifiedIdentifierRoot::Target(token)
        | pernixc_syntax::QualifiedIdentifierRoot::This(token) => {
            Some(self.to_absolute_span(&token.span).await) == pointing_span
        }
        pernixc_syntax::QualifiedIdentifierRoot::GenericIdentifier(
            generic_identifier,
        ) => {
            if let Some(identifier) = generic_identifier.identifier() {
                Some(self.to_absolute_span(&identifier.span).await)
                    == pointing_span
            } else {
                false
            }
        }
    };

    // shows as pointing to the root
    if root_match {
        return Some(Resolution::Success(SuccessResolution {
            pointing_symbol: root,
            parent_scope: None,
        }));
    }

    let mut current_symbol_id = root;

    for subsequence in qualified_identifier.subsequences() {
        let identifier =
            subsequence.generic_identifier().and_then(|x| x.identifier())?;

        let identifier_span = self.to_absolute_span(&identifier.span).await;

        let Some(resolved_id) = self
            .resolve_in(
                current_symbol_id,
                &identifier.kind.0,
                /* consider_adt_implements */ true,
            )
            .await
        else {
            // resolution failed at this segment
            return Some(Resolution::Failed(Failed {
                prior_scope: Some(current_symbol_id),
                fail_at_cursor: Some(identifier_span) == pointing_span,
            }));
        };

        // shows as pointing to this subsequence
        if Some(identifier_span) == pointing_span {
            return Some(Resolution::Success(SuccessResolution {
                pointing_symbol: resolved_id,
                parent_scope: Some(current_symbol_id),
            }));
        }

        current_symbol_id = resolved_id;
    }

    // no match at the cursor position
    Some(Resolution::NoMatchFound(current_symbol_id))
}

/// Resolves the symbol at the given LSP position in the specified URI.
#[extend]
pub async fn symbol_at(
    self: &TrackedEngine,
    position: &lsp_types::Position,
    uri: &lsp_types::Url,
    target_id: pernixc_target::TargetID,
) -> Option<Global<pernixc_symbol::ID>> {
    let source_file_path = uri.to_file_path().unwrap();
    let source_file_path: Interned<Path> =
        self.intern_unsized(source_file_path);

    let source_id = target_id.make_global(
        self.get_stable_path_id(source_file_path.clone(), target_id)
            .await
            .expect("lsp URL should've been valid"),
    );
    let source_file = self.get_source_file_by_id(source_id).await;

    // get the module under the source file
    let module_id =
        target_id.make_global(self.get_source_file_module(source_id).await);

    // determine the byte position
    let byte_index = source_file.get_byte_index_from_editor_location(
        &position.to_pernix_editor_location(),
    );

    // get the most specific symbol scope at the byte index
    let symbol_scope_id = self
        .get_symbol_scope_at_byte_index(module_id, source_id, byte_index)
        .await;

    // get the qualified name of the symbol scope
    let (qualified_identifier, pointing_span) = self
        .get_pointing_qualified_identifier(
            target_id,
            source_file_path.clone(),
            byte_index,
        )
        .await?;

    let resolved_symbol_id = self
        .resolve_qualified_identifier_path(
            symbol_scope_id,
            &qualified_identifier,
            Some(pointing_span),
        )
        .await?;

    match resolved_symbol_id {
        Resolution::Success(success) => Some(success.pointing_symbol),

        Resolution::Failed(_) | Resolution::NoMatchFound(_) => None,
    }
}

/// Retrieves the most specific symbol scope that contains the given byte
/// index.
#[extend]
pub async fn get_symbol_scope_at_byte_index(
    self: &TrackedEngine,
    current_scope_id: Global<pernixc_symbol::ID>,
    current_souce_file_id: GlobalSourceID,
    byte_index: pernixc_source_file::ByteIndex,
) -> Global<pernixc_symbol::ID> {
    let Some(members) = self.try_get_members(current_scope_id).await else {
        return current_scope_id;
    };

    for member in members
        .member_ids_by_name
        .values()
        .chain(members.unnameds.iter())
        .copied()
        .map(|x| current_scope_id.target_id.make_global(x))
    {
        // if the member's span contains the byte index, recurse into it
        let Some(span) = self.get_scope_span(member).await else {
            continue;
        };

        let abs_span = self.to_absolute_span(&span).await;
        if abs_span.range().contains(&byte_index)
            && abs_span.source_id == current_souce_file_id
        {
            return Box::pin(self.get_symbol_scope_at_byte_index(
                member,
                current_souce_file_id,
                byte_index,
            ))
            .await;
        }
    }

    current_scope_id
}
