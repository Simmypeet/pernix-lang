use pernixc_term::r#type::Type;
use pernixc_type_system::constraints::Constraints;

/// Represents the result of a subtyping check for register assignments.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Subtype {
    /// The subtyping was successful. Contains the accumulated lifetime
    /// constraints.
    Succeeded(Constraints),

    /// The subtyping failed due to incompatible types.
    Incompatible {
        /// The type that was found (actual value type).
        found_type: Type,
        /// The type that was expected.
        expected_type: Type,
    },

    /// The subtyping failed due to constraints on forall lifetimes.
    ForallLifetimeError {
        /// The type that was found (actual value type).
        found_type: Type,
        /// The type that was expected.
        expected_type: Type,
    },
}
