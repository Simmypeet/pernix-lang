//! Defines diagnostic types for instance resolution errors.
//!
//! The main entry point is [`InstanceResolutionError`] whose
//! [`InstanceResolutionError::generate_diagnostics`] method produces
//! **one [`InstanceResolutionLeafError`] per leaf failure** in the error tree.
//! Each [`InstanceResolutionLeafError`] stores un-rendered raw data and
//! implements [`Report`], so callers can either render it immediately or
//! inspect/store it first.
//!
//! The recursion pattern mirrors `generate_marker_error` in `wf_check.rs`:
//! the context stack (`Vec<InstanceResolutionFrame>`) is built top-down, and
//! one output value is emitted per leaf.

use std::sync::Arc;

use pernixc_arena::ID;
use pernixc_diagnostic::{Highlight, Note, Rendered, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::ByteIndex;
use pernixc_symbol::{
    kind::get_kind, source_map::to_absolute_span, span::get_span,
};
use pernixc_target::Global;
use pernixc_term::{
    display::Display,
    generic_parameters::{
        InstanceParameter, InstanceParameterID, get_generic_parameters,
    },
    instance::TraitRef,
};
use qbice::{Decode, Encode, StableHash};

use super::{InstanceSource, RecursiveError, ResolveError};

// ─── context frame
// ────────────────────────────────────────────────────────────

/// One frame in the resolution trace stack.
///
/// Stores the raw data needed to render a single "note" in the chain that leads
/// from a root [`InstanceResolutionError`] down to a leaf failure.  The
/// rendering happens lazily inside [`Report::report`].
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, StableHash, Encode, Decode,
)]
pub struct InstanceResolutionFrame {
    /// The trait reference that was being resolved at this level.
    parent_trait_ref: TraitRef,

    /// The instance symbol candidate that was selected at this level of
    /// resolution (the symbol that requires a sub-instance parameter).
    resolving_symbol: Global<pernixc_symbol::SymbolID>,

    /// The ID of the instance parameter (on `resolving_symbol`) that could not
    /// be resolved, triggering the descent to the next level.
    param_id: ID<InstanceParameter>,

    base: bool,
}

// ─── leaf error kinds
// ─────────────────────────────────────────────────────────

/// The concrete reason for the leaf failure, stored in an un-rendered form.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, StableHash, Encode, Decode,
)]
pub enum InstanceResolutionErrorKind {
    /// No instance was found that satisfies the required trait reference.
    NotFound,

    /// The resolution would have required itself, forming a cycle.
    Cyclic,

    /// Multiple instances matched, making the resolution ambiguous.
    Ambiguous {
        /// The set of ambiguous instance sources.
        sources: Arc<[InstanceSource]>,
    },
}

// ─── public leaf diagnostic type ─────────────────────────────────────────────

/// A single leaf-level instance resolution failure.
///
/// This is the intermediary type produced by
/// [`ResolveError::generate_diagnostics`].  It stores the raw
/// data (trait references, symbol IDs, spans) without any async rendering.
/// Call [`Report::report`] to convert it to a [`Rendered`] diagnostic.
///
/// The `context_stack` carries the trace from the root call down to this leaf,
/// ordered outermost → innermost.  Each frame becomes a `note` in the rendered
/// diagnostic.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, StableHash, Encode, Decode,
)]
pub struct InstanceResolutionError {
    /// The trait reference that failed to resolve at this leaf.
    expected_trait_ref: TraitRef,

    /// The span at the original call-site that triggered instance resolution.
    instantiation_span: RelativeSpan,

    /// The specific reason this leaf failed.
    kind: InstanceResolutionErrorKind,

    /// The resolution trace from root → this leaf.
    ///
    /// Each entry describes one level of the recursion: which symbol was being
    /// resolved, and which of its instance parameters caused the descent.
    context_stack: Vec<InstanceResolutionFrame>,
}

enum SymbolOrParameter {
    Symbol(Global<pernixc_symbol::SymbolID>),
    Parameter(InstanceParameterID),
}

impl Report for InstanceResolutionError {
    #[allow(clippy::too_many_lines)]
    async fn report(&self, engine: &TrackedEngine) -> Rendered<ByteIndex> {
        let trait_ref_str =
            self.expected_trait_ref.write_to_string(engine).await.unwrap();

        let primary_span =
            engine.to_absolute_span(&self.instantiation_span).await;

        // Render each frame in the context stack into a Note.
        let mut notes: Vec<Note<ByteIndex>> = Vec::new();

        for frame in &self.context_stack {
            let generic_params =
                engine.get_generic_parameters(frame.resolving_symbol).await;

            let param_name = generic_params
                .get_instance_parameter(frame.param_id)
                .name()
                .to_string();

            let parent_trait_ref_str =
                frame.parent_trait_ref.write_to_string(engine).await.unwrap();

            let highlight: Option<Highlight<ByteIndex>> = {
                let generic_params =
                    engine.get_generic_parameters(frame.resolving_symbol).await;

                if let Some(span) = generic_params[frame.param_id].span() {
                    let abs = engine.to_absolute_span(span).await;
                    let kind = engine
                        .get_kind(frame.resolving_symbol)
                        .await
                        .kind_str();

                    Some(Highlight::new(
                        abs,
                        Some(format!(
                            "this {kind} requires the parameter to be resolved"
                        )),
                    ))
                } else {
                    None
                }
            };

            notes.push(
                Note::builder()
                    .message(format!(
                        "required because resolving instance parameter \
                         `{param_name}` for `{parent_trait_ref_str}`"
                    ))
                    .maybe_primary_highlight(highlight)
                    .build(),
            );
        }

        match &self.kind {
            InstanceResolutionErrorKind::NotFound => Rendered::builder()
                .message(format!("no instance found for `{trait_ref_str}`"))
                .primary_highlight(
                    Highlight::builder()
                        .span(primary_span)
                        .message(format!(
                            "an instance of `{trait_ref_str}` is required here"
                        ))
                        .build(),
                )
                .help_message(
                    "consider adding an `instance` parameter or importing a \
                     suitable instance implementation into scope",
                )
                .notes(notes)
                .build(),

            InstanceResolutionErrorKind::Cyclic => Rendered::builder()
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
                     restructuring the instance hierarchy to break the cycle",
                )
                .notes(notes)
                .build(),

            InstanceResolutionErrorKind::Ambiguous { sources } => {
                let mut related = Vec::new();

                for source in sources.iter() {
                    let candidate_id = match source {
                        InstanceSource::FromGlobalInstance(id)
                        | InstanceSource::FromAssociatedInstance(id)
                        | InstanceSource::FromInstanceScope(id) => {
                            SymbolOrParameter::Symbol(*id)
                        }
                        InstanceSource::FromInstanceParameterID(id) => {
                            SymbolOrParameter::Parameter(*id)
                        }
                    };

                    match candidate_id {
                        SymbolOrParameter::Symbol(global) => {
                            if let Some(span) = engine.get_span(global).await {
                                let abs = engine.to_absolute_span(&span).await;
                                related.push(Highlight::new(
                                    abs,
                                    Some(
                                        "this candidate instance also matches"
                                            .to_string(),
                                    ),
                                ));
                            }
                        }
                        SymbolOrParameter::Parameter(member_id) => {
                            let generic_params = engine
                                .get_generic_parameters(member_id.parent_id())
                                .await;

                            if let Some(span) =
                                generic_params[member_id.id()].span()
                            {
                                let abs = engine.to_absolute_span(span).await;
                                related.push(Highlight::new(
                                    abs,
                                    Some(
                                        "this candidate instance parameter \
                                         also matches"
                                            .to_string(),
                                    ),
                                ));
                            }
                        }
                    }
                }

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
                    .notes(notes)
                    .build()
            }
        }
    }
}

// ─── internal recursive collector ────────────────────────────────────────────

async fn collect_leaf_diagnostics_inner(
    engine: &TrackedEngine,
    expected_trait_ref: &TraitRef,
    instantiation_span: RelativeSpan,
    error: &ResolveError,
    // Accumulated (un-rendered) context stack — cloned per branch.
    context_stack: Vec<InstanceResolutionFrame>,
    output: &mut Vec<InstanceResolutionError>,
) {
    match error {
        ResolveError::NotFound => {
            output.push(InstanceResolutionError {
                expected_trait_ref: expected_trait_ref.clone(),
                instantiation_span,
                kind: InstanceResolutionErrorKind::NotFound,
                context_stack,
            });
        }

        ResolveError::Cyclic => {
            output.push(InstanceResolutionError {
                expected_trait_ref: expected_trait_ref.clone(),
                instantiation_span,
                kind: InstanceResolutionErrorKind::Cyclic,
                context_stack,
            });
        }

        ResolveError::Ambiguous(sources) => {
            output.push(InstanceResolutionError {
                expected_trait_ref: expected_trait_ref.clone(),
                instantiation_span,
                kind: InstanceResolutionErrorKind::Ambiguous {
                    sources: sources.clone(),
                },
                context_stack,
            });
        }

        ResolveError::Recursive(recursive) => {
            collect_recursive_inner(
                engine,
                instantiation_span,
                recursive,
                &context_stack,
                output,
            )
            .await;
        }
    }
}

async fn collect_recursive_inner(
    engine: &TrackedEngine,
    instantiation_span: RelativeSpan,
    recursive: &RecursiveError,
    context_stack: &[InstanceResolutionFrame],
    output: &mut Vec<InstanceResolutionError>,
) {
    let resolving_symbol = recursive.resolving_symbol();

    Box::pin(async move {
        for (param_id, sub_error, trait_ref) in recursive.errors() {
            // Build the new frame for this level and push it onto a per-branch
            // clone of the stack.
            let mut child_stack = context_stack.to_vec();
            child_stack.push(InstanceResolutionFrame {
                parent_trait_ref: trait_ref.clone(),
                resolving_symbol,
                param_id: *param_id,
                base: false,
            });

            collect_leaf_diagnostics_inner(
                engine,
                trait_ref,
                instantiation_span,
                sub_error,
                child_stack,
                output,
            )
            .await;
        }
    })
    .await;
}

impl ResolveError {
    /// Generates a list of [`InstanceResolutionLeafError`] from the given
    /// [`ResolveError`].
    #[must_use]
    pub async fn generate_diagnostics(
        &self,
        engine: &TrackedEngine,
        expected_trait_ref: &TraitRef,
        instance_parameter_id: InstanceParameterID,
        instantiation_span: RelativeSpan,
    ) -> Vec<InstanceResolutionError> {
        let mut output = Vec::new();

        collect_leaf_diagnostics_inner(
            engine,
            expected_trait_ref,
            instantiation_span,
            self,
            vec![InstanceResolutionFrame {
                parent_trait_ref: expected_trait_ref.clone(),
                resolving_symbol: instance_parameter_id.parent_id(),
                param_id: instance_parameter_id.id(),
                base: true,
            }],
            &mut output,
        )
        .await;

        output
    }
}
