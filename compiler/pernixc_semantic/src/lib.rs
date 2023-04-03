//! Semantic analysis for the Pernix compiler. This phase is responsible for checking the syntax
//! tree for semantic errors and building control flow graphs.

#![deny(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

use std::sync::Arc;

use derive_new::new;
use pernixc_common::source_file::{SourceFile, Span};

pub mod binding;
pub mod control_flow_graph;
pub mod errors;
pub mod symbol;

/// Is a structure containing the [`Span`] and the [`SourceFile`] where the span is located.
#[derive(Debug, Clone, PartialEq, Eq, Hash, new)]
pub struct SourceSpan {
    /// The source file where the span is located.
    pub source_file: Arc<SourceFile>,

    /// The span.
    pub span: Span,
}
