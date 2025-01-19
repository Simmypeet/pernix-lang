//! Implements various builders for each components of the symbols.

pub mod builder;
pub mod diagnostic;
pub mod function;
pub mod generic_parameters;
pub mod implementation;
pub mod reflector;
pub mod type_alias;
pub mod type_system;
pub mod where_clause;

pub(crate) mod occurrences;

mod build;

pub use build::{build, Compilation, ComponentCallback, SymbolCallback};

/// Macro for shortening the handling of errors in the term resolution. It
/// will report the **cyclic dependency** error to the handler and panic
/// on other unexpected errors.
///
/// # Parameters
///
/// - `$expr:expr`: The expression that yields the `Result<T, Error>`.
/// - `$handler:expr`: The handler to report the error to.
/// - `$diverge:expr`: The alternative value to return in case of an cyclic
///   dependency error.
#[macro_export]
macro_rules! handle_term_resolution_result {
    ($expr:expr, $handler:expr, $diverge:expr $(,)?) => {
        match $expr {
            Ok(value) => value,
            Err(pernixc_resolution::term::Error::Query(
                query::Error::CyclicDependency(error),
            )) => {
                $handler.receive(Box::new(error));

                $diverge
            }

            error @ Err(
                pernixc_resolution::term::Error::InvalidReferringSiteID
                | pernixc_resolution::term::Error::Query(_),
            ) => {
                panic!("unexpected error: {error:?}");
            }
        }
    };
}

/// Macro for shortening the handling of errors in the qualified identifier
/// resolution. It will report the **cyclic dependency** error to the handler
/// and panic on other unexpected errors.
///
/// # Parameters
///
/// - `$expr:expr`: The expression that yields the `Result<T, Error>`.
/// - `$handler:expr`: The handler to report the error to.
/// - `$diverge:expr`: The alternative value to return in case of an cyclic
///   dependency error.
#[macro_export]
macro_rules! handle_qualified_identifer_resolve_result {
    ($expr:expr, $handler:expr, $diverge:expr $(,)?) => {{
        use pernixc_resolution::qualified_identifier;

        match $expr {
            Ok(value) => value,
            Err(qualified_identifier::Error::Fatal) => $diverge,
            Err(qualified_identifier::Error::Query(
                query::Error::CyclicDependency(error),
            )) => {
                $handler.receive(Box::new(error));
                $diverge
            }
            Err(
                qualified_identifier::Error::InvalidReferringSiteID
                | qualified_identifier::Error::Query(_),
            ) => {
                panic!("unexpected error");
            }
        }
    }};
}

/// Macro for shortening the handling of errors `Result<T,
/// pernixc::table::query::Error>`.
///
/// # Parameters
///
/// - `$expr:expr`: The expression that yields the `Result<T, Error>`.
/// - `$handler:expr`: The handler to report the error to.
/// - `$diverge:expr`: The alternative value to return in case of an cyclic
///   dependency error.
#[macro_export]
macro_rules! handle_query_result {
    ($expr:expr, $handle:expr, $diverge:expr $(,)?) => {
        match $expr {
            Ok(value) => value,
            Err(pernixc_table::query::Error::CyclicDependency(error)) => {
                $handle.receive(Box::new(error));
                $diverge
            }

            Err(
                err @(
                pernixc_table::query::Error::SymbolNotFoundOrInvalidComponent
                | pernixc_table::query::Error::NoBuilderFound)
            ) => {
                panic!("unexpected error: {err}");
            }
        }
    };
}
