#![allow(missing_docs)]

use pernixc_arena::ID;
use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_semantic_element::fields::{get_fields, Field};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{name::get_qualified_name, source_map::to_absolute_span};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    display::{Display, InferenceRenderingMap},
    r#type::Type,
};

use crate::diagnostic_enum;

diagnostic_enum! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        StableHash,
        Serialize,
        Deserialize,
    )]
    pub enum Diagnostic {
        MismatchedPatternBindingType(MismatchedPatternBindingType),
        TooLargeNumericLiteral(TooLargeNumericLiteral),
        MoreThanOnePackedTuplePattern(MoreThanOnePackedTuplePattern),
        MismatchedTuplePatternLength(MismatchedTuplePatternLength),
        ExpectedTuplePackPattern(ExpectedTuplePackPattern),
        FieldNotFound(FieldNotFound),
        AlreadyBoundFieldPattern(AlreadyBoundFieldPattern),
        FieldIsNotAccessible(FieldIsNotAccessible),
        UnboundFields(UnboundFields),
        ExpectedAssociatedPattern(ExpectedAssociatedPattern),
        UnexpectedAssociatedPattern(UnexpectedAssociatedPattern),
    }
}

/// The kind of the pattern binding.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum PatternBindingType {
    #[display("struct")]
    Struct,
    #[display("tuple")]
    Tuple,
    #[display("enum")]
    Enum,
    #[display("integer")]
    Integer,
    #[display("boolean")]
    Boolean,
}

/// A pattern expects a certain type but was given a different one.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct MismatchedPatternBindingType {
    /// The type that was found.
    pub found: Type,

    /// The type that was expected.
    pub expected: PatternBindingType,

    /// The span of the pattern.
    pub span: RelativeSpan,

    /// The inference rendering map for constants.
    pub constant_inference_map: InferenceRenderingMap<Constant>,

    /// The inference rendering map for types.
    pub type_inference_map: InferenceRenderingMap<Type>,
}

impl Report<&TrackedEngine> for MismatchedPatternBindingType {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span = engine.to_absolute_span(&self.span).await;

        // Format the found type with inference rendering maps
        let mut found_type_str = String::new();
        let _ = self
            .found
            .write_async_with_mapping(
                engine,
                &mut found_type_str,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message(format!(
                "mismatched pattern binding type: expected {} but found `{}`",
                self.expected, found_type_str
            ))
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "this pattern expects {} type but found `{}`",
                        self.expected, found_type_str
                    ))
                    .span(span)
                    .build(),
            )
            .build()
    }
}

/// Th numeric literal is too large for the pattern. The pattern can only accept
/// up to u64.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct TooLargeNumericLiteral {
    /// The span of the numeric literal.
    pub span: RelativeSpan,
}

impl Report<&TrackedEngine> for TooLargeNumericLiteral {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span = engine.to_absolute_span(&self.span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("numeric literal is too large for pattern")
            .primary_highlight(
                Highlight::builder()
                    .message("this numeric literal is too large")
                    .span(span)
                    .build(),
            )
            .help_message(
                "the pattern can only accept numeric literals up to u64",
            )
            .build()
    }
}

/// The pattern contains more than one packed tuple pattern.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct MoreThanOnePackedTuplePattern {
    /// The span where the illegal tuple pattern was found.
    pub illegal_tuple_pattern_span: RelativeSpan,
}

impl Report<&TrackedEngine> for MoreThanOnePackedTuplePattern {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span =
            engine.to_absolute_span(&self.illegal_tuple_pattern_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("more than one packed tuple pattern found")
            .primary_highlight(
                Highlight::builder()
                    .message("only one packed tuple pattern is allowed")
                    .span(span)
                    .build(),
            )
            .build()
    }
}

/// The pattern contains mismatched element length.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct MismatchedTuplePatternLength {
    /// The span of the pattern.
    pub pattern_span: RelativeSpan,

    /// The number of elements in the pattern.
    pub pattern_element_count: usize,

    /// The number of elements in the type.
    pub type_element_count: usize,
}

impl Report<&TrackedEngine> for MismatchedTuplePatternLength {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span = engine.to_absolute_span(&self.pattern_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("mismatched tuple pattern length")
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "this pattern has {} elements but the type has {} \
                         elements",
                        self.pattern_element_count, self.type_element_count
                    ))
                    .span(span)
                    .build(),
            )
            .build()
    }
}

/// Tuple pack pattern expected.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct ExpectedTuplePackPattern {
    /// Where the tuple pack pattern was expected.
    pub illegal_tuple_span: RelativeSpan,
}

impl Report<&TrackedEngine> for ExpectedTuplePackPattern {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span = engine.to_absolute_span(&self.illegal_tuple_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message("expected a packed tuple pattern here")
            .primary_highlight(
                Highlight::builder()
                    .message(
                        "unpacked tuple type needs to be matched with a tuple \
                         pack pattern",
                    )
                    .span(span)
                    .build(),
            )
            .build()
    }
}

pub use crate::bind::expression::r#struct::diagnostic::FieldNotFound;

/// The field is already bound in the pattern.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct AlreadyBoundFieldPattern {
    /// The span of the pattern.
    pub rebound_span: RelativeSpan,

    /// The span of the first bound occurrence of the field in the pattern.
    pub first_bound_span: RelativeSpan,

    /// The ID of the struct where the field is already bound.
    pub struct_id: Global<pernixc_symbol::ID>,

    /// The ID of the field that is already bound.
    pub field_id: ID<Field>,
}

impl Report<&TrackedEngine> for AlreadyBoundFieldPattern {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span = engine.to_absolute_span(&self.rebound_span).await;
        let fields = engine.get_fields(self.struct_id).await.unwrap();
        let struct_qualified_name =
            engine.get_qualified_name(self.struct_id).await;

        let field_name = fields.fields[self.field_id].name.as_str();

        let first_bound_span =
            engine.to_absolute_span(&self.first_bound_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message(format!(
                "the field `{field_name}` is already bound in the pattern",
            ))
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "the field `{field_name}` is already bound in the \
                         pattern of struct `{struct_qualified_name}`"
                    ))
                    .span(span)
                    .build(),
            )
            .related(vec![Highlight::builder()
                .message(format!(
                    "the field `{field_name}` is first bound here"
                ))
                .span(first_bound_span)
                .build()])
            .build()
    }
}

pub use crate::bind::expression::r#struct::diagnostic::FieldIsNotAccessible;

/// Not all fields are bound in the pattern.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct UnboundFields {
    /// The ID of the struct where the fields are unbound.
    pub field_ids: Vec<ID<Field>>,

    /// The ID of the struct where the fields are unbound.
    pub struct_id: Global<pernixc_symbol::ID>,

    /// The span of the pattern where the fields are unbound.
    pub pattern_span: RelativeSpan,
}

impl Report<&TrackedEngine> for UnboundFields {
    type Location = ByteIndex;

    async fn report(
        &self,
        table: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let fields = table.get_fields(self.struct_id).await.unwrap();
        let field_names = self
            .field_ids
            .iter()
            .map(|&field_id| fields.fields.get(field_id).unwrap().name.clone())
            .collect::<Vec<_>>();

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message(format!(
                "not all fields are bound in the pattern: {}",
                field_names.join(", ")
            ))
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "these fields are not bound in the pattern: {}",
                        field_names.join(", ")
                    ))
                    .span(table.to_absolute_span(&self.pattern_span).await)
                    .build(),
            )
            .help_message(
                "try to handle all fields in the pattern or use the `..` \
                 syntax after the fields to ignore the rest",
            )
            .build()
    }
}

/// The variant expects an associated pattern but none was provided.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct ExpectedAssociatedPattern {
    /// The ID of the variant where the associated pattern is expected.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The span of the variant pattern where the associated pattern is
    /// expected.
    pub pattern_span: RelativeSpan,
}

impl Report<&TrackedEngine> for ExpectedAssociatedPattern {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let variant_name = engine.get_qualified_name(self.variant_id).await;
        let span = engine.to_absolute_span(&self.pattern_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message(format!(
                "the variant `{variant_name}` expects an associated pattern",
            ))
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "the variant `{variant_name}` expects an associated \
                         pattern here",
                    ))
                    .span(span)
                    .build(),
            )
            .help_message(format!(
                "if you want to ignore the associated type, use the `case \
                 {variant_name}(..)` syntax instead"
            ))
            .build()
    }
}

/// The variant doesn't have an associated value but pattern contains an
/// associated value.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct UnexpectedAssociatedPattern {
    /// The variant ID that the pattern matches.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The span of the pattern with the associated pattern.
    pub associated_pattern_span: RelativeSpan,
}

impl Report<&TrackedEngine> for UnexpectedAssociatedPattern {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let variant_name = engine.get_qualified_name(self.variant_id).await;
        let span = engine.to_absolute_span(&self.associated_pattern_span).await;

        pernixc_diagnostic::Diagnostic::builder()
            .severity(Severity::Error)
            .message(format!(
                "the variant `{variant_name}` does not have an associated \
                 value",
            ))
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "the pattern for variant `{variant_name}` cannot have \
                         an associated pattern",
                    ))
                    .span(span)
                    .build(),
            )
            .build()
    }
}
