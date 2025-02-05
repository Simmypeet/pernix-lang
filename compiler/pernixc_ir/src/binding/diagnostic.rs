//! Contains the diagnostic related to the IR binding.

use pernixc_arena::ID;
use pernixc_component::fields::{Field, Fields};
use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_table::{component::Name, DisplayObject, GlobalID, Table};
use pernixc_term::{
    r#type::{Qualifier, Type},
    Model,
};

/// An enumeration of either a `break` or `continue` control flow.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum LoopControlFlow {
    #[display(fmt = "break")]
    Break,

    #[display(fmt = "continue")]
    Continue,
}

/// The loop with the given label name was not found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoopWithGivenLabelNameNotFound {
    /// The span of the label identifier.
    pub span: Span,
}

impl Report<&Table> for LoopWithGivenLabelNameNotFound {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "loop with label named `{}` was not found",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The loop control flow expression can't be used outside of a loop.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoopControlFlowOutsideLoop {
    /// The span of the break expression.
    pub span: Span,

    /// The kind of control flow.
    pub control_flow: LoopControlFlow,
}

impl Report<&Table> for LoopControlFlowOutsideLoop {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "`{}` expression can't be used outside of a loop",
                self.control_flow
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Found a cyclic inference in the type checking.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CyclicInference<M: Model> {
    /// The type that is involved in the cyclic inference.
    pub first: Type<M>,

    /// The type that is involved in the cyclic inference.
    pub second: Type<M>,

    /// The sapn where the type check occurred.
    pub span: Span,
}

impl<M: Model> Report<&Table> for CyclicInference<M>
where
    Type<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "found a cyclic inference: `{}` and `{}`",
                DisplayObject { display: &self.first, table },
                DisplayObject { display: &self.second, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Mismatched type error.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedType<M: Model> {
    /// The expected type.
    pub expected_type: Type<M>,

    /// The found type.
    pub found_type: Type<M>,

    /// The sapn where the type check occurred.
    pub span: Span,
}

impl<M: Model> Report<&Table> for MismatchedType<M>
where
    Type<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "mismatched type: expected `{}` but found `{}`",
                DisplayObject { display: &self.expected_type, table },
                DisplayObject { display: &self.found_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Expected an l-value but found an r-value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectedLValue {
    /// The span of the r-value.
    pub expression_span: Span,
}

impl Report<&Table> for ExpectedLValue {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.expression_span.clone(),
            message: "expected an l-value".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The expression of the given type cannot be dereferenced.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CannotDereference<M: Model> {
    /// The type of the expression that cannot be dereferenced.
    pub found_type: Type<M>,

    /// The span of the expression with dereference operator.
    pub span: Span,
}

impl<M: Model> Report<&Table> for CannotDereference<M>
where
    Type<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the expression of type `{}` cannot be dereferenced",
                DisplayObject { display: &self.found_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Not all flow paths in this block expression express a value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NotAllFlowPathsExpressValue {
    /// The span of the block expression.
    pub span: Span,
}

impl Report<&Table> for NotAllFlowPathsExpressValue {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: "not all flow paths in this block expression express a \
                      value"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The given l-value cannot be referenced as the given one.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedQualifierForReferenceOf {
    /// The span of the reference of expression
    pub reference_of_span: Span,

    /// The qualifier of the l-value.
    pub found_qualifier: Qualifier,

    /// The requested qualifier for the reference of expression.
    pub expected_qualifier: Qualifier,

    /// Whether or not the l-value is behind an another reference.
    pub is_behind_reference: bool,
}

impl Report<&Table> for MismatchedQualifierForReferenceOf {
    fn report(&self, _: &Table) -> Diagnostic {
        let message = if self.is_behind_reference {
            format!(
                "the l-value is behind a reference with qualifier `{}` but \
                 the reference of expression expects a qualifier `{}`",
                self.found_qualifier, self.expected_qualifier
            )
        } else {
            format!(
                "the l-value has qualifier `{}` but the reference of \
                 expression expects a qualifier `{}`",
                self.found_qualifier, self.expected_qualifier
            )
        };

        Diagnostic {
            span: self.reference_of_span.clone(),
            message,
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
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
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum PatternBindingType {
    #[display(fmt = "struct")]
    Struct,
    #[display(fmt = "tuple")]
    Tuple,
    #[display(fmt = "enum")]
    Enum,
    #[display(fmt = "integer")]
    Integer,
    #[display(fmt = "boolean")]
    Boolean,
}

/// Pattern expects a different kind of binding.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedPatternBindingType<M: Model> {
    /// The expected binding type of the pattern.
    pub expected_bindnig_type: PatternBindingType,

    /// The found type of the pattern.
    pub found_type: Type<M>,

    /// The span of the pattern.
    pub pattern_span: Span,
}

impl<M: Model> Report<&Table> for MismatchedPatternBindingType<M>
where
    Type<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.pattern_span.clone(),
            message: format!(
                "the pattern expects a {} type but found {} ",
                self.expected_bindnig_type,
                DisplayObject { display: &self.found_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The pattern contains more than one packed tuple pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoreThanOnePackedTuplePattern {
    /// The span where the illegal tuple pattern was found.
    pub illegal_tuple_pattern_span: Span,
}

impl Report<&Table> for MoreThanOnePackedTuplePattern {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.illegal_tuple_pattern_span.clone(),
            message: "the pattern contains more than one packed tuple pattern"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Tuple pack pattern expected.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectedTuplePackPattern {
    /// Where the tuple pack pattern was expected.
    pub illegal_tuple_span: Span,
}

impl Report<&Table> for ExpectedTuplePackPattern {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.illegal_tuple_span.clone(),
            message: "tuple pack pattern expected".to_string(),
            severity: Severity::Error,
            help_message: Some(
                "unpacked tuple type needs to be matched with a tuple pack \
                 pattern"
                    .to_string(),
            ),
            related: Vec::new(),
        }
    }
}

/// Field with given name was not found in the struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldNotFound {
    /// The span to the identifier that refers to non-existent field.
    pub identifier_span: Span,

    /// The struct ID where the field is not found.
    pub struct_id: GlobalID,
}

impl Report<&Table> for FieldNotFound {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.identifier_span.clone(),
            message: "the field is not found in the struct".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The field is already bound in the pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AlreadyBoundFieldPattern {
    /// The span of the pattern.
    pub pattern_span: Span,

    /// The ID of the struct where the field is already bound.
    pub struct_id: GlobalID,

    /// The ID of the field that is already bound.
    pub field_id: ID<Field>,
}

impl Report<&Table> for AlreadyBoundFieldPattern {
    fn report(&self, table: &Table) -> Diagnostic {
        let fields = table.query::<Fields>(self.struct_id).unwrap();
        let field_sym = &fields.fields[self.field_id];

        Diagnostic {
            span: self.pattern_span.clone(),
            message: format!(
                "the field `{}` is already bound in the pattern",
                field_sym.name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The field is not accessible in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldIsNotAccessible {
    /// The ID of the field that is not accessible.
    pub field_id: ID<Field>,

    /// The struct in which the field is not accessible.
    pub struct_id: GlobalID,

    /// The ID to the scope where the field is not accessible.
    pub referring_site: GlobalID,

    /// The span where the field is referred from.
    pub referring_identifier_span: Span,
}

impl Report<&Table> for FieldIsNotAccessible {
    fn report(&self, table: &Table) -> Diagnostic {
        let fields = table.query::<Fields>(self.struct_id).unwrap();
        let field_sym = &fields.fields[self.field_id];

        let struct_qualified_name = table.get_qualified_name(self.struct_id);
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site);

        Diagnostic {
            span: self.referring_identifier_span.clone(),
            message: format!(
                "the field `{}` of `{struct_qualified_name}` is not \
                 accessible in the scope `{referring_site_qualified_name}`",
                field_sym.name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Not all fields are bound in the pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnboundFields {
    /// The ID of the struct where the fields are unbound.
    pub field_ids: Vec<ID<Field>>,

    /// The ID of the struct where the fields are unbound.
    pub struct_id: GlobalID,

    /// The span of the pattern where the fields are unbound.
    pub pattern_span: Span,
}

impl Report<&Table> for UnboundFields {
    fn report(&self, table: &Table) -> Diagnostic {
        let fields = table.query::<Fields>(self.struct_id).unwrap();
        let field_names = self
            .field_ids
            .iter()
            .map(|&field_id| fields.fields.get(field_id).unwrap().name.clone())
            .collect::<Vec<_>>();

        Diagnostic {
            span: self.pattern_span.clone(),
            message: format!(
                "not all fields are bound in the pattern: {}",
                field_names.join(", ")
            ),
            severity: Severity::Error,
            help_message: Some(
                "try to handle all fields in the pattern or use the `..` \
                 syntax after the fields to ignore the rest"
                    .to_string(),
            ),
            related: Vec::new(),
        }
    }
}

/// The variant expects an associated pattern but none was provided.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectedAssociatedPattern {
    /// The ID of the variant where the associated pattern is expected.
    pub variant_id: GlobalID,

    /// The span of the variant pattern where the associated pattern is
    /// expected.
    pub pattern_span: Span,
}

impl Report<&Table> for ExpectedAssociatedPattern {
    fn report(&self, table: &Table) -> Diagnostic {
        let name = table.get::<Name>(self.variant_id);

        Diagnostic {
            span: self.pattern_span.clone(),
            message: format!(
                "the enum variant `{}` expects an associated pattern",
                name.0
            ),
            severity: Severity::Error,
            help_message: Some(
                "if you want to discard the associated value, use the `case \
                 Name(..)` pattern"
                    .to_string(),
            ),
            related: Vec::new(),
        }
    }
}

/// The variant doesn't have an associated value but pattern contains an
/// associated value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedAssociatedPattern {
    /// The variant ID that the pattern matches.
    pub variant_id: GlobalID,

    /// The span of the pattern with the associated pattern.
    pub associated_pattern_span: Span,
}

impl Report<&Table> for UnexpectedAssociatedPattern {
    fn report(&self, table: &Table) -> Diagnostic {
        let variant_name = table.get_qualified_name(self.variant_id);

        Diagnostic {
            span: self.associated_pattern_span.clone(),
            message: format!(
                "the variant `{variant_name}` doesn't have an associated \
                 value but the matched pattern contains one",
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The pattern contains mismatched element length.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedTuplePatternLength {
    /// The span of the pattern.
    pub pattern_span: Span,

    /// The number of elements in the pattern.
    pub pattern_element_count: usize,

    /// The number of elements in the type.
    pub type_element_count: usize,
}

impl Report<&Table> for MismatchedTuplePatternLength {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.pattern_span.clone(),
            message: format!(
                "the pattern contains mismatched element length: expected {} \
                 element(s) but found {}",
                self.type_element_count, self.pattern_element_count
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Th numeric literal is too large.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TooLargeNumericLiteral {
    /// The span of the numeric literal.
    pub span: Span,
}

impl Report<&Table> for TooLargeNumericLiteral {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: "the numeric literal is too large".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Can't bind a tuple pattern to a reference bound tuple type with unpacked
/// element.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FoundPackTuplePatternInReferenceBoundTupleType {
    /// The span of the pattern.
    pub pattern_span: Span,
}

impl Report<&Table> for FoundPackTuplePatternInReferenceBoundTupleType {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.pattern_span.clone(),
            message: "can't bind a tuple pattern to a reference bound tuple \
                      type with pack element"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The numeric suffix is unknown.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidNumericSuffix {
    /// The span of the numeric suffix.
    pub suffix_span: Span,
}

impl Report<&Table> for InvalidNumericSuffix {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.suffix_span.clone(),
            message: format!(
                "the numeric suffix `{}` is unknown",
                self.suffix_span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Thee numeric expression has a suffix of an integral type but the expression
/// has decimal point.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FloatingPointLiteralHasIntegralSuffix {
    /// The span of the numeric literal.
    pub numeric_literal_span: Span,
}

impl Report<&Table> for FloatingPointLiteralHasIntegralSuffix {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.numeric_literal_span.clone(),
            message: "the numeric expression has a suffix of an integral type \
                      but the expression has decimal point"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
