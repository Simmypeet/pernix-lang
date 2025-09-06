//! Diagnostics emitted during the binding phase of the compiler.

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::binder;

diagnostic_enum! {
    /// An enumeration of all diagnostics that can occur during binding the
    /// IR phase.
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        StableHash,
        Serialize,
        Deserialize,
        derive_more::From,
    )]
    #[allow(clippy::large_enum_variant, missing_docs)]
    pub enum Diagnostic {
        Resolution(pernixc_resolution::diagnostic::Diagnostic),
        TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
        TypeCheck(binder::type_check::diagnostic::Diagnostic),
        Parenthesized(expression::parenthesized::diagnostic::Diagnostic),
        Numeric(expression::numeric::diagnostic::Diagnostic),
        Struct(expression::r#struct::diagnostic::Diagnostic),
    }
}

/// A convenience macro for creating diagnostic enums and implementing the
/// `Report` trait for them.
#[macro_export]
macro_rules! diagnostic_enum {
    {
        $( #[$enum_meta:meta] )*
        $enum_vis:vis enum $ident:ident {
            $(
                $variant:ident($ty:ty)
            ),* $(,)?
        }
    } => {
        $( #[$enum_meta] )*
        $enum_vis enum $ident {
            $(
                $variant($ty),
            )*
        }

        impl pernixc_diagnostic::Report<&pernixc_query::TrackedEngine> for Diagnostic {
            type Location = pernixc_source_file::ByteIndex;

            async fn report(
                &self,
                engine: &pernixc_query::TrackedEngine,
            ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
                match self {
                    $(
                        Diagnostic::$variant(inner) => inner.report(engine).await,
                    )*
                }
            }
        }
    };
}

pub use diagnostic_enum;

use crate::bind::expression;
