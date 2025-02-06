//! Contains the diagnostic related to the IR binding.

use std::collections::HashSet;

use pernixc_arena::ID;
use pernixc_component::fields::{Field, Fields};
use pernixc_diagnostic::{Diagnostic, Related, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_table::{
    component::{LocationSpan, Name},
    DisplayObject, GlobalID, Table,
};
use pernixc_term::{
    r#type::{self, Qualifier, Type},
    Model,
};

use crate::model;

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

/// The enum variant expects an associated value but none was provided.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectedAssociatedValue {
    /// The ID of the variant where the associated value is expected.
    pub variant_id: GlobalID,

    /// The span of the variant.
    pub span: Span,
}

impl Report<&Table> for ExpectedAssociatedValue {
    fn report(&self, table: &Table) -> Diagnostic {
        let name = table.get::<Name>(self.variant_id);

        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the enum variant `{}` expects an associated value",
                name.0
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The symbol cannot be used as an expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolCannotBeUsedAsAnExpression {
    /// The span of the symbol reference.
    pub span: Span,

    /// The ID of the symbol that cannot be used as an expression.
    pub symbol: GlobalID,
}

impl Report<&Table> for SymbolCannotBeUsedAsAnExpression {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the symbol `{}` cannot be used as an expression",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Attempts to assign to a non-mutable l-value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssignToNonMutable {
    /// The span of the assignment.
    pub span: Span,
}

impl Report<&Table> for AssignToNonMutable {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "cannot assign to `{}` since it's immutable",
                self.span.str(),
            ),
            severity: Severity::Error,
            help_message: Some(
                "the value must be `mutable` to assign".to_string(),
            ),
            related: Vec::new(),
        }
    }
}

/// Can't perform relational operation on the given type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidRelationalOperation<M: Model> {
    /// The type where the relational operation is not allowed.
    pub found_type: Type<M>,

    /// The span of the relational operation.
    pub span: Span,
}

impl<M: Model> Report<&Table> for InvalidRelationalOperation<M>
where
    Type<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "can't perform relational operation on expression with type \
                 `{}`",
                DisplayObject { display: &self.found_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The unpack operator can only be used once in a tuple expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoreThanOneUnpackedInTupleExpression<M: Model> {
    /// The span of the tuple expression.
    pub span: Span,

    /// The type of the tuple expression.
    pub r#type: Type<M>,
}

impl<M: Model> Report<&Table> for MoreThanOneUnpackedInTupleExpression<M>
where
    Type<M>: pernixc_table::Display,
{
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: "the unpack operator can only be used once in a tuple \
                      expression"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The struct type was expected but the found type is different.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectedStruct {
    /// The span of the qualified identifier.
    pub span: Span,
}

impl Report<&Table> for ExpectedStruct {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: "struct type expected".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The field is initialized more than once.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DuplicatedFieldInitialization {
    /// The ID of the field that is initialized more than once.
    pub field_id: ID<Field>,

    /// The ID of the struct where the field is initialized.
    pub struct_id: GlobalID,

    /// The span of the first initialization.
    pub prior_initialization_span: Span,

    /// The span of the duplicate initialization.
    pub duplicate_initialization_span: Span,
}

impl Report<&Table> for DuplicatedFieldInitialization {
    fn report(&self, table: &Table) -> Diagnostic {
        let fields = table.query::<Fields>(self.struct_id).unwrap();
        let field_sym = &fields.fields[self.field_id];

        Diagnostic {
            span: self.duplicate_initialization_span.clone(),
            message: format!(
                "the field `{}` is initialized more than once",
                field_sym.name
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: self.prior_initialization_span.clone(),
                message: "the field is first initialized here".to_string(),
            }],
        }
    }
}

/// The struct expression contains uninitialized fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UninitializedFields {
    /// The ID of the struct where the fields are uninitialized.
    pub struct_id: GlobalID,

    /// The set of uninitialized fields.
    pub uninitialized_fields: HashSet<ID<Field>>,

    /// The span of the struct expression.
    pub struct_expression_span: Span,
}

impl Report<&Table> for UninitializedFields {
    fn report(&self, table: &Table) -> Diagnostic {
        let fields = table.query::<Fields>(self.struct_id).unwrap();
        Diagnostic {
            span: self.struct_expression_span.clone(),
            message: "the struct expression contains uninitialized fields"
                .to_string(),
            severity: Severity::Error,
            help_message: Some(format!(
                "the following fields are uninitialized: {}",
                self.uninitialized_fields
                    .iter()
                    .map(|&field_id| { fields.fields[field_id].name.clone() })
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
            related: Vec::new(),
        }
    }
}

/// The given type cannot be used in cast expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidCastType<M: Model> {
    /// The span of the type reference.
    pub span: Span,

    /// The type that cannot be used in cast expression.
    pub r#type: Type<M>,
}

impl<M: Model> Report<&Table> for InvalidCastType<M>
where
    Type<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "cannot castt an expression to type type `{}`",
                DisplayObject { display: &self.r#type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The trait method call resolves into multiple candidates.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AmbiguousMethodCall {
    /// The span where the method call was made.
    pub method_call_span: Span,

    /// The candidates that were found.
    pub callable_candidates: Vec<GlobalID>,
}

impl Report<&Table> for AmbiguousMethodCall {
    fn report(&self, table: &Table) -> Diagnostic {
        let candidates = self
            .callable_candidates
            .iter()
            .copied()
            .map(|x| table.get_qualified_name(x))
            .collect::<Vec<_>>();

        Diagnostic {
            span: self.method_call_span.clone(),
            message: format!(
                "the method call resulted into multiple candidates: {}",
                candidates.join(", ")
            ),
            severity: Severity::Error,
            help_message: Some(
                "try using fully qualified name to solve the amguity"
                    .to_string(),
            ),
            related: Vec::new(),
        }
    }
}

/// No method found for the given method call expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MethodNotFound {
    /// The span where the method call was made.
    pub method_call_span: Span,
}

impl Report<&Table> for MethodNotFound {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.method_call_span.clone(),
            message: "no method found for the given method call expression"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Adt implementation function cannot be used as a method.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AdtImplementationFunctionCannotBeUsedAsMethod {
    /// The ID of the ADT implementation function.
    pub adt_implementation_function_id: GlobalID,

    /// The span of the function call.
    pub span: Span,
}

impl Report<&Table> for AdtImplementationFunctionCannotBeUsedAsMethod {
    fn report(&self, table: &Table) -> Diagnostic {
        let adt_implementation_function_symbol =
            table.get_qualified_name(self.adt_implementation_function_id);

        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the implementation function \
                 `{adt_implementation_function_symbol}` cannot be used as a \
                 method",
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The expected argument count does not match the found argument count.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedArgumentCount {
    /// The ID of the symbol that was supplied with the wrong number of
    /// arguments.
    ///
    /// This can be an enum variant, function, trait function, trait
    /// implementation function, and ADT implementation function.
    pub called_id: GlobalID,

    /// The expected argument count.
    pub expected_count: usize,

    /// The found argument count.
    pub found_count: usize,

    /// The span of the function call.
    pub span: Span,
}

impl Report<&Table> for MismatchedArgumentCount {
    fn report(&self, table: &Table) -> Diagnostic {
        let called_symbol = table.get_qualified_name(self.called_id);

        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the symbol `{}` expects {} argument(s) but found {}",
                called_symbol, self.expected_count, self.found_count
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Expected a struct type to access the field but found a different type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectedStructType<M: Model> {
    /// The span of the expression where the field is accessed.
    pub span: Span,

    /// The type that is not a struct type.
    pub r#type: Type<M>,
}

impl<M: Model> Report<&Table> for ExpectedStructType<M>
where
    Type<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "expected a struct type to access the field but found `{}`",
                DisplayObject { display: &self.r#type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The field access is not allowed to have generic arguments.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedGenericArgumentsInField {
    /// The span of the field access.
    pub field_access_span: Span,
}

impl Report<&Table> for UnexpectedGenericArgumentsInField {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.field_access_span.clone(),
            message: "the field access is not allowed to have generic \
                      arguments"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Expected a tuple type to access the field but found a different type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectedTuple<M: Model> {
    /// The span of the expression where the field is accessed.
    pub span: Span,

    /// The type that is not a tuple type.
    pub r#type: Type<M>,
}

impl<M: Model> Report<&Table> for ExpectedTuple<M>
where
    Type<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "expected a tuple type to access the field but found `{}`",
                DisplayObject { display: &self.r#type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The tuple index is too large.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TooLargeTupleIndex {
    /// The span of the tuple index.
    pub access_span: Span,
}

impl Report<&Table> for TooLargeTupleIndex {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.access_span.clone(),
            message: "the tuple index is too large".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The tuple index is out of bounds.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleIndexOutOfBOunds<M: Model> {
    /// The span of the tuple index.
    pub access_span: Span,

    /// The index of the tuple.
    pub tuple_type: r#type::Tuple<M>,
}

impl<M: Model> Report<&Table> for TupleIndexOutOfBOunds<M>
where
    r#type::Tuple<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        let count = self.tuple_type.elements.len();

        Diagnostic {
            span: self.access_span.clone(),
            message: "the tuple index is out of bounds".to_string(),
            severity: Severity::Error,
            help_message: Some(format!(
                "the tuple type is `{}` having {} element(s)",
                DisplayObject { display: &self.tuple_type, table },
                count,
            )),
            related: Vec::new(),
        }
    }
}

/// Indexing past the unpacked element in tuple is not allowed.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CannotIndexPastUnpackedTuple<M: Model> {
    /// The span of the index.
    pub index_span: Span,

    /// The type of the tuple.
    pub tuple_type: r#type::Tuple<M>,
}

impl<M: Model> Report<&Table> for CannotIndexPastUnpackedTuple<M>
where
    r#type::Tuple<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        let unpacked_index = self
            .tuple_type
            .elements
            .iter()
            .position(|x| x.is_unpacked)
            .unwrap();

        Diagnostic {
            span: self.index_span.clone(),
            message: "indexing past the unpacked element in tuple is not \
                      allowed"
                .to_string(),

            severity: Severity::Error,
            help_message: Some(format!(
                "the tuple type is `{}` having {} element(s) and the unpacked \
                 element is at index {}",
                DisplayObject { display: &self.tuple_type, table },
                self.tuple_type.elements.len(),
                unpacked_index,
            )),
            related: Vec::new(),
        }
    }
}

/// Expected an arrary type to index into elements but found a different type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectArray<M: Model> {
    /// The span of the expression where the elements are indexed.
    pub span: Span,

    /// The type that is not an array type.
    pub r#type: Type<M>,
}

impl<M: Model> Report<&Table> for ExpectArray<M>
where
    Type<M>: pernixc_table::Display,
{
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "expected an array type to index into elements but found `{}`",
                DisplayObject { display: &self.r#type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The given expression is not callable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpressionIsNotCallable {
    /// The span of the expression without the call operator.
    pub span: Span,
}

impl Report<&Table> for ExpressionIsNotCallable {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the expression `{}` is not callable",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The given symbol cannot be called.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIsNotCallable {
    /// The ID of the symbol that cannot be called.
    pub called_id: GlobalID,

    /// The span of the call.
    pub span: Span,
}

impl Report<&Table> for SymbolIsNotCallable {
    fn report(&self, table: &Table) -> Diagnostic {
        let called_symbol = table.get_qualified_name(self.called_id);

        Diagnostic {
            span: self.span.clone(),
            message: format!("the symbol `{called_symbol}` cannot be called"),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The block with the given label name was not found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockWithGivenLableNameNotFound {
    /// The span of the label identifier.
    pub span: Span,
}

impl Report<&Table> for BlockWithGivenLableNameNotFound {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "block with label named `{}` was not found",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The `epxress` expression can't be used outside of a block expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpressOutsideBlock {
    /// The span of the expression.
    pub span: Span,
}

impl Report<&Table> for ExpressOutsideBlock {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: "`express` expression can't be used outside of a block \
                      expression"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The `return` expression is not allowed in this context.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnIsNotAllowed {
    /// The span of the return expression.
    pub span: Span,
}

impl Report<&Table> for ReturnIsNotAllowed {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: "`return` expression is not allowed in this context"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The match arm is unreachable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnreachableMatchArm {
    /// The span of the match arm.
    pub match_arm_span: Span,
}

impl Report<&Table> for UnreachableMatchArm {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.match_arm_span.clone(),
            message: "unreachable match arm".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The match expression is non-exhaustive.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonExhaustiveMatch {
    /// The span of the match expression.
    pub match_expression_span: Span,
}

impl Report<&Table> for NonExhaustiveMatch {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.match_expression_span.clone(),
            message: "non-exhaustive match expression".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Not all flow paths in the function return a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NotAllFlowPathsReturnAValue {
    /// The function which contains an error.
    pub callable_id: GlobalID,
}

impl Report<&Table> for NotAllFlowPathsReturnAValue {
    fn report(&self, table: &Table) -> Diagnostic {
        let span =
            table.get::<LocationSpan>(self.callable_id).span.clone().unwrap();

        Diagnostic {
            span,
            message: "not all flow paths in the function return a value"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Couldn't further infer the type, explicit type annotation is required.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeAnnotationRequired {
    /// The span of the expression/declration where the type annotation is
    /// required.
    pub span: Span,

    /// The type that couldn't be further inferred.
    pub r#type: Type<model::Constrained>,
}

impl Report<&Table> for TypeAnnotationRequired {
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.span.clone(),
            message: format!(
                "couldn't further infer the type `{}`, explicit type \
                 annotation is required",
                DisplayObject { display: &self.r#type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
