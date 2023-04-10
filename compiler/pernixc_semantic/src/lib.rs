//! Semantic analysis for the Pernix compiler.

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

use pernixc_common::source_file::{SourceFile, Span};

pub mod control_flow_graph;
pub mod errors;
pub mod hir;
pub mod infer;
pub mod symbol;

/// Is a structure containing the [`Span`] and the [`SourceFile`] where the span is located.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceSpan {
    /// The source file where the span is located.
    pub source_file: Arc<SourceFile>,

    /// The span.
    pub span: Span,
}

impl SourceSpan {
    /// Gets the string slice of the source code that is located in the span.
    #[must_use]
    pub fn source_code(&self) -> &str { &self.source_file[self.span] }
}

/// Is a structure used in [`SemanticResult`] [`Ok`] variant in order to return a value and a list
/// of non-fatal errors.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OkResult<T, RE> {
    /// The value that is returned in the [`Ok`] variant.
    pub value: T,

    /// The list of non-fatal errors that is returned in the [`Ok`] variant.
    pub errors: Vec<RE>,
}

impl<T, RE> OkResult<T, RE> {
    /// Unwraps the [`OkResult`] and returns the value and propagates the non-fatal errors.
    ///
    /// # Parameters
    /// - `errors`: The list of non-fatal errors that is returned in the [`Ok`] variant.
    pub fn unwrap_extend<RE2>(self, errors: &mut Vec<RE2>) -> T
    where
        RE: Into<RE2>,
    {
        errors.extend(self.errors.into_iter().map(Into::into));
        self.value
    }

    /// Unwraps the [`OkResult`] and returns the value.
    pub fn unwrap_drop(self) -> T { self.value }

    /// Maps the value of the [`OkResult`] using the given function.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> OkResult<U, RE> {
        OkResult {
            value: f(self.value),
            errors: self.errors,
        }
    }

    /// Unwraps the [`OkResult`] and returns the list of non-fatal errors.
    pub fn unwrap_err(self) -> Vec<RE> { self.errors }

    /// Unwraps the [`OkResult`] and returns the list of non-fatal errors with an additional error.
    pub fn unwrap_err_push(mut self, error: RE) -> Vec<RE> {
        self.errors.push(error);
        self.errors
    }
}

impl<T, E> From<T> for OkResult<T, E> {
    fn from(value: T) -> Self {
        Self {
            value,
            errors: Vec::new(),
        }
    }
}

/// Is an alias for the [`Result`] type used in the semantic analysis.
///
/// In semantic analysis, some errors are not fatal and the analysis can continue. In order to
/// return a value and a list of non-fatal errors, the [`Ok`] variant is wrapped in a [`OkResult`]
/// structure.
///
/// # Generics Parameters
/// - `T`: The type of the value that is returned in the [`Ok`] variant.
/// - `E`: The type of the error that is returned in the [`Err`] variant.
/// - `RE`: The type of the list of non-fatal errors that is returned in the [`Ok`] variant.
pub type SemanticResult<T, E, RE = E> = std::result::Result<OkResult<T, RE>, Vec<E>>;
