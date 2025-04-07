//! Contains the definition of the [`Tree`] struct

/// Is an enumeration of the different types of delimiters in the [`Delimiter`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum DelimiterKind {
    Parenthesis,
    Brace,
    Bracket,
}

