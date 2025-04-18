use serde::{Deserialize, Serialize};
use strum_macros::EnumIter;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumIter,
    Serialize,
    Deserialize,
)]
pub enum DelimiterKind {
    /// A parenthesis: `(`.
    Parenthesis,
    /// A brace: `{`.
    Brace,
    /// A bracket: `[`.
    Bracket,
}
