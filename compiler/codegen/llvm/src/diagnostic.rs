//! Contains the diagnostics used in the code generation process.

use pernixc_diagnostic::{ByteIndex, Highlight, Rendered, Report};
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_symbol::{source_map::to_absolute_span, span::get_span};
use pernixc_target::Global;

/// An enumeration of diagnostics that can occur during code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Diagnostic {
    MainIsNotAFunction(MainIsNotAFunction),
    InvalidMainFunctionSignature(InvalidMainFunctionSignature),
    GenericParametersAreNotAllowedInMainFunction(
        GenericParametersAreNotAllowedInMainFunction,
    ),
    WhereClausePredicatesAreNotAllowedInMainFunction(
        WhereClausePredicatesAreNotAllowedInMainFunction,
    ),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        match self {
            Self::MainIsNotAFunction(diag) => diag.report(engine).await,
            Self::InvalidMainFunctionSignature(diag) => {
                diag.report(engine).await
            }
            Self::GenericParametersAreNotAllowedInMainFunction(diag) => {
                diag.report(engine).await
            }
            Self::WhereClausePredicatesAreNotAllowedInMainFunction(diag) => {
                diag.report(engine).await
            }
        }
    }
}

/// The `main` symbol is reserved from the main function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MainIsNotAFunction {
    /// The ID of the main function.
    pub main_function_id: Global<pernixc_symbol::ID>,
}

impl Report for MainIsNotAFunction {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        let span = parameter
            .to_absolute_span(
                &parameter.get_span(self.main_function_id).await.unwrap(),
            )
            .await;

        Ok(Rendered::builder()
            .message(
                "the `main` symbol is reserved for the main function and must \
                 be a function with the signature `function() -> int32`",
            )
            .primary_highlight(Highlight::builder().span(span).build())
            .help_message("consider renaming this symbol to something else")
            .build())
    }
}

/// The main function must have the signature `function(): int32`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidMainFunctionSignature {
    /// The ID of the main function.
    pub main_function_id: Global<pernixc_symbol::ID>,
}

impl Report for InvalidMainFunctionSignature {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        let span = parameter
            .to_absolute_span(
                &parameter.get_span(self.main_function_id).await.unwrap(),
            )
            .await;

        Ok(Rendered::builder()
            .message(
                "the `main` function must have the signature `function() -> \
                 int32`",
            )
            .primary_highlight(Highlight::builder().span(span).build())
            .help_message(
                "consider changing the signature of the main function",
            )
            .build())
    }
}

/// The main  function is not allowed to have generic parameters
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParametersAreNotAllowedInMainFunction {
    /// The ID of the main function
    pub main_function_id: Global<pernixc_symbol::ID>,
}

impl Report for GenericParametersAreNotAllowedInMainFunction {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        let span = parameter
            .to_absolute_span(
                &parameter.get_span(self.main_function_id).await.unwrap(),
            )
            .await;

        Ok(Rendered::builder()
            .message(
                "the `main` function is not allowed to have generic parameters",
            )
            .primary_highlight(Highlight::builder().span(span).build())
            .help_message("consider removing the generic parameters")
            .build())
    }
}

/// The where clause predicates are not allowed in the main function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhereClausePredicatesAreNotAllowedInMainFunction {
    /// The ID of the main function
    pub main_function_id: Global<pernixc_symbol::ID>,
}

impl Report for WhereClausePredicatesAreNotAllowedInMainFunction {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<Rendered<ByteIndex>, CyclicError> {
        let span = parameter
            .to_absolute_span(
                &parameter.get_span(self.main_function_id).await.unwrap(),
            )
            .await;

        Ok(Rendered::builder()
            .message(
                "the `main` function is not allowed to have where clause \
                 predicates",
            )
            .primary_highlight(Highlight::builder().span(span).build())
            .help_message("consider removing the where clause predicates")
            .build())
    }
}
