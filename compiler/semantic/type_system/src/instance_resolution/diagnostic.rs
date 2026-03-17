//! Defines diagnostic types for instance resolution errors.
//!
//! The main entry point is [`InstanceResolutionError`] whose
//! [`InstanceResolutionError::generate_diagnostics`] method produces **one
//! [`Rendered`] diagnostic per leaf error**, each accompanied by a note chain
//! that traces from the root resolution call down to the exact failure.
//!
//! This mirrors the `generate_marker_error` pattern in `wf_check.rs`:
//! the "context stack" (note chain) is built **top-down** as the recursion
//! descends, and exactly one diagnostic is emitted each time a leaf error
//! (`NotFound`, `Cyclic`, or `Ambiguous`) is encountered.

use pernixc_diagnostic::{Highlight, Note, Rendered};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{source_map::to_absolute_span, span::get_span};
use pernixc_target::Global;
use pernixc_term::{
    display::Display, generic_parameters::get_generic_parameters,
    instance::TraitRef,
};

use super::{InstanceSource, RecursiveError, ResolveError};

// ─── internal helpers
// ─────────────────────────────────────────────────────────

/// Tries to retrieve and convert a symbol's declaration span into an absolute-
/// span [`Highlight`], returning `None` when no span is available.
async fn symbol_highlight(
    engine: &TrackedEngine,
    symbol_id: Global<pernixc_symbol::ID>,
    message: impl Into<Option<String>>,
) -> Option<Highlight<ByteIndex>> {
    let span = engine.get_span(symbol_id).await?;
    let abs = engine.to_absolute_span(&span).await;
    Some(Highlight::new(abs, message.into()))
}

// ─── core recursive generator
// ─────────────────────────────────────────────────

/// Mirrors `generate_marker_error` in `wf_check.rs`.
///
/// Walks the [`ResolveError`] tree depth-first, building up a `note_stack` as
/// it descends.  Whenever a **leaf** error is reached (`NotFound`, `Cyclic`,
/// or `Ambiguous`), a complete [`Rendered`] diagnostic is pushed into
/// `diagnostics` with:
/// - a primary highlight at `instantiation_span`,
/// - a message that names the leaf failure,
/// - the full `note_stack` as trace notes (root → leaf).
///
/// Multiple leaves therefore produce multiple distinct diagnostics, each with
/// the same root context but a different failure note at the end.
#[allow(clippy::too_many_lines)]
async fn generate_instance_resolution_diagnostics_inner(
    engine: &TrackedEngine,
    expected_trait_ref: &TraitRef,
    instantiation_span: RelativeSpan,
    error: &ResolveError,
    // Accumulated note chain from the root down to the current recursion
    // level.
    note_stack: Vec<Note<ByteIndex>>,
    diagnostics: &mut Vec<Rendered<ByteIndex>>,
) {
    let trait_ref_str = expected_trait_ref
        .write_to_string(engine)
        .await
        .unwrap_or_else(|_| "<?>".to_string());

    let primary_span = engine.to_absolute_span(&instantiation_span).await;

    match error {
        // ── leaf: NotFound ────────────────────────────────────────────────
        ResolveError::NotFound => {
            diagnostics.push(
                Rendered::builder()
                    .message(format!("no instance found for `{trait_ref_str}`"))
                    .primary_highlight(
                        Highlight::builder()
                            .span(primary_span)
                            .message(format!(
                                "an instance of `{trait_ref_str}` is required \
                                 here"
                            ))
                            .build(),
                    )
                    .help_message(
                        "consider adding an `instance` parameter or importing \
                         a suitable instance implementation into scope",
                    )
                    .notes(note_stack)
                    .build(),
            );
        }

        // ── leaf: Cyclic ──────────────────────────────────────────────────
        ResolveError::Cyclic => {
            diagnostics.push(
                Rendered::builder()
                    .message(format!(
                        "cyclic instance dependency while resolving \
                         `{trait_ref_str}`"
                    ))
                    .primary_highlight(
                        Highlight::builder()
                            .span(primary_span)
                            .message(format!(
                                "resolving `{trait_ref_str}` requires itself"
                            ))
                            .build(),
                    )
                    .help_message(
                        "instance resolution encountered a cycle; try \
                         restructuring the instance hierarchy to break the \
                         cycle",
                    )
                    .notes(note_stack)
                    .build(),
            );
        }

        // ── leaf: Ambiguous ───────────────────────────────────────────────
        ResolveError::Ambiguous(sources) => {
            let mut related = Vec::new();

            for source in sources.iter() {
                let candidate_id = match source {
                    InstanceSource::FromGlobalInstance(id)
                    | InstanceSource::FromAssociatedInstance(id)
                    | InstanceSource::FromInstanceScope(id) => Some(*id),

                    InstanceSource::FromInstanceParameterID(_) => None,
                };

                if let Some(id) = candidate_id
                    && let Some(h) = symbol_highlight(
                        engine,
                        id,
                        "this candidate instance also matches".to_string(),
                    )
                    .await
                {
                    related.push(h);
                }
            }

            diagnostics.push(
                Rendered::builder()
                    .message(format!(
                        "ambiguous instance resolution for `{trait_ref_str}`"
                    ))
                    .primary_highlight(
                        Highlight::builder()
                            .span(primary_span)
                            .message(format!(
                                "multiple instances match `{trait_ref_str}`"
                            ))
                            .build(),
                    )
                    .related(related)
                    .help_message(
                        "the resolution is ambiguous because more than one \
                         instance satisfies the required trait reference; \
                         consider providing a more specific instance or \
                         removing the ambiguity",
                    )
                    .notes(note_stack)
                    .build(),
            );
        }

        // ── recursive: descend one level, extend the note stack ───────────
        ResolveError::Recursive(recursive) => {
            Box::pin(generate_recursive_diagnostics(
                engine,
                expected_trait_ref,
                instantiation_span,
                recursive,
                note_stack,
                diagnostics,
            ))
            .await;
        }
    }
}

/// Handles the [`ResolveError::Recursive`] variant by iterating over every
/// failed instance-parameter sub-resolution, appending a context note to the
/// stack, and then recursing.
async fn generate_recursive_diagnostics(
    engine: &TrackedEngine,
    // The trait ref from the *parent* call (used in the context note).
    parent_trait_ref: &TraitRef,
    instantiation_span: RelativeSpan,
    recursive: &RecursiveError,
    note_stack: Vec<Note<ByteIndex>>,
    diagnostics: &mut Vec<Rendered<ByteIndex>>,
) {
    let resolving_symbol = recursive.resolving_symbol();

    // Get generic parameters so we can look up instance parameter names.
    let generic_params = engine.get_generic_parameters(resolving_symbol).await;

    let parent_trait_ref_str = parent_trait_ref
        .write_to_string(engine)
        .await
        .unwrap_or_else(|_| "<?>".to_string());

    for (param_id, sub_error) in recursive.errors() {
        let param_name =
            generic_params.get_instance_parameter(*param_id).name().to_string();

        // Build the context note that describes *this* level of the recursion:
        // "required because resolving instance parameter `<name>` needed by
        //  `<parent_trait_ref>` through `<resolving_symbol>`".
        let note_msg = format!(
            "required because resolving instance parameter `{param_name}` for \
             `{parent_trait_ref_str}`"
        );

        let related: Vec<Highlight<ByteIndex>> = symbol_highlight(
            engine,
            resolving_symbol,
            "this instance requires the parameter to be resolved".to_string(),
        )
        .await
        .into_iter()
        .collect();

        // Clone the stack and append the new context note before passing it
        // down, so each sibling sub-error gets its own independent stack.
        let mut child_stack = note_stack.clone();
        child_stack
            .push(Note::builder().message(note_msg).related(related).build());

        // The sub-error's "expected trait ref" is the trait ref of the
        // sub-instance parameter declaration (if available).
        let sub_trait_ref = generic_params
            .get_instance_parameter(*param_id)
            .trait_ref()
            .map_or_else(
                || recursive.current_trait_ref().clone(),
                |tr| (**tr).clone(),
            );

        Box::pin(generate_instance_resolution_diagnostics_inner(
            engine,
            &sub_trait_ref,
            instantiation_span,
            sub_error,
            child_stack,
            diagnostics,
        ))
        .await;
    }
}

// ─── public API
// ───────────────────────────────────────────────────────────────

/// A value that represents a failure during instance resolution.
///
/// Call [`InstanceResolutionError::generate_diagnostics`] to obtain the full
/// set of [`Rendered`] diagnostics, one per leaf failure in the error tree.
///
/// ## Why not `Report`?
///
/// [`pernixc_diagnostic::Report`] models a single diagnostic.  Instance
/// resolution may produce **multiple** leaf errors (e.g. when resolving a
/// symbol that requires two different instance parameters and both fail).
/// Using a `Vec<Rendered>` output lets each leaf error be its own diagnostic
/// with its own complete note chain, exactly as `generate_marker_error` in
/// `wf_check.rs` produces multiple entries in a `Vec<Diagnostic>`.
#[derive(Debug, Clone)]
pub struct InstanceResolutionError {
    /// The trait reference whose instance couldn't be resolved.
    pub expected_trait_ref: TraitRef,

    /// The span at the call-site that triggered the resolution (used as the
    /// primary highlight in every emitted diagnostic).
    pub instantiation_span: RelativeSpan,

    /// The underlying resolution error.
    pub error: ResolveError,
}

impl InstanceResolutionError {
    /// Creates a new [`InstanceResolutionError`].
    #[must_use]
    pub const fn new(
        expected_trait_ref: TraitRef,
        instantiation_span: RelativeSpan,
        error: ResolveError,
    ) -> Self {
        Self { expected_trait_ref, instantiation_span, error }
    }

    /// Produces one [`Rendered`] diagnostic for **each leaf failure** in the
    /// error tree.
    ///
    /// Each diagnostic carries:
    /// - a primary highlight at [`Self::instantiation_span`],
    /// - the leaf error message as the main message,
    /// - a note chain (root → leaf) tracing exactly why that leaf was reached.
    ///
    /// This means a single [`InstanceResolutionError`] may produce more than
    /// one entry when the underlying [`ResolveError`] is
    /// [`ResolveError::Recursive`] with multiple failed instance parameters.
    pub async fn generate_diagnostics(
        &self,
        engine: &TrackedEngine,
    ) -> Vec<Rendered<ByteIndex>> {
        let mut diagnostics = Vec::new();

        generate_instance_resolution_diagnostics_inner(
            engine,
            &self.expected_trait_ref,
            self.instantiation_span,
            &self.error,
            Vec::new(), // start with an empty note stack at the root
            &mut diagnostics,
        )
        .await;

        diagnostics
    }
}
