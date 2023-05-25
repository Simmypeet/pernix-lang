//! Contains the definition of various `proptest` strategies for testing purposes.

use std::str::FromStr;

use proptest::{prop_oneof, strategy::Strategy};
use strum::IntoEnumIterator;

use super::{KeywordKind, Token};

/// A strategy that generates a valid identifier string.
pub fn identifier() -> impl Strategy<Value = String> {
    proptest::collection::vec(
        proptest::char::any().prop_filter("allows only identifier character", |x| {
            Token::is_identifier_character(*x)
        }),
        0..=10,
    )
    .prop_map(|vec| vec.into_iter().collect::<String>())
    .prop_filter(
        "filter out first strings that have digit as the first character and empty string",
        |x| x.chars().next().map_or(false, |x| !x.is_ascii_digit()),
    )
    .prop_filter(
        "filter out identifiers that can be used as a keyword",
        |x| KeywordKind::from_str(x).is_err(),
    )
}

/// Returns a strategy that generates one of the variants of the [`KeywordKind`]
pub fn keyword_kind() -> impl Strategy<Value = KeywordKind> {
    proptest::sample::select(KeywordKind::iter().collect::<Vec<_>>())
}

/// Returns a strategy that generates a valid numeric literal value string
pub fn numeric_literal_value() -> impl Strategy<Value = String> {
    prop_oneof![
        proptest::num::f64::ANY.prop_filter_map("filter out negative numbers and inf", |x| {
            if x.is_finite() && x.is_sign_positive() {
                Some(x.to_string())
            } else {
                None
            }
        }),
        proptest::num::u64::ANY.prop_map(|x| x.to_string())
    ]
}

/// Returns a strategy that generates a valid delimited comment string
///
/// The `/*` and `*/` are not included in the generated string, just the comment body.
pub fn delimited_comment_body() -> impl Strategy<Value = String> {
    "[^\r]*".prop_filter("must not contain */", |x| !x.contains("*/"))
}
