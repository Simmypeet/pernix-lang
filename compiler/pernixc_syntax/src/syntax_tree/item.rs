//! Contains the definitions of item syntax tree.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};
use pernixc_source::{SourceElement, Span};
use pernixc_system::diagnostic::{Dummy, Handler};

use super::{
    expression::{Expression, Functional},
    pattern,
    statement::Statement,
    AccessModifier, ConnectedList, LifetimeArgument, QualifiedIdentifier, ScopeSeparator,
    TypeAnnotation, TypeSpecifier,
};
use crate::{
    error::{
        AccessModifierExpected, Error, GenericArgumentParameterListCannotBeEmpty, ItemExpected,
        PunctuationExpected,
    },
    parser::Parser,
};

/// Represents a moulde path in used in `module` declarations and `using` statements.
///
/// Syntax Synopsis:
/// ``` txt
/// ModulePath:
///     Identifier ('::' Identifier)*
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct ModulePath {
    #[get = "pub"]
    first: Identifier,
    #[get = "pub"]
    rest: Vec<(ScopeSeparator, Identifier)>,
}

impl ModulePath {
    /// Returns an iterator over the path identifiers.
    pub fn paths(&self) -> impl Iterator<Item = &Identifier> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, id)| id))
    }
}

/// Represents a syntax tree node for a `module` using statement.
///
/// Syntax Synopsis:
/// ``` txt
/// Using:
///     'using' ModulePath ';'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Using {
    #[get = "pub"]
    using_keyword: Keyword,
    #[get = "pub"]
    module_path: ModulePath,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Using {
    fn span(&self) -> Span { self.using_keyword.span.join(&self.semicolon.span).unwrap() }
}

/// Represents a syntax tree node for a `module` signature.
///
/// Syntax Synopsis:
/// ``` txt
/// ModuleSignature:
///     'module' Identifier
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct ModuleSignature {
    #[get = "pub"]
    module_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
}

/// Represents a syntax tree node for a `module` declaration.
///
/// Syntax Synopsis:
/// ``` txt
/// Module:
///     AccessModifier ModuleSignature ModuleContent
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Module {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: ModuleSignature,
    #[get = "pub"]
    content: ModuleContent,
}

impl SourceElement for Module {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.content.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for a module content.
///
/// Syntax Synopsis:
/// ``` txt
/// ModuleContent:
///     ';'
///     | ModuleBody
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner)]
pub enum ModuleContent {
    File(Punctuation),
    Inline(ModuleBody),
}

impl SourceElement for ModuleContent {
    fn span(&self) -> Span {
        match self {
            Self::File(p) => p.span.clone(),
            Self::Inline(m) => m.span(),
        }
    }
}

/// Represents a syntax tree node for a module body.
///
/// Syntax Synopsis:
/// ``` txt
/// ModuleBody:
///    '{' Using* Item* '}'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct ModuleBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    usings: Vec<Using>,
    #[get = "pub"]
    items: Vec<Item>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for ModuleBody {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
}

/// Represents a syntax tree node for a lifetime parameter.
///
/// Syntax Synopsis:
/// ``` text
/// LifetimeParameter:
///     '\'' Identifier
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct LifetimeParameter {
    #[get = "pub"]
    apostrophe: Punctuation,
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for LifetimeParameter {
    fn span(&self) -> Span { self.apostrophe.span.join(&self.identifier.span).unwrap() }
}

/// Represents a syntax tree node for a default generic parameter.
#[derive(Debug, Clone, Getters)]
pub struct DefaultGenericParameter<Value: SourceElement> {
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    value: Value,
}

impl<Value: SourceElement> SourceElement for DefaultGenericParameter<Value> {
    fn span(&self) -> Span { self.equals.span.join(&self.value.span()).unwrap() }
}

/// Represents a syntax tree node for a default generic parameter for a type.
///
/// Syntax Synopsis:
/// ``` txt
/// DefaultTypeParameter:
///     '=' TypeSpecifier
///     ;
/// ```
type DefaultTypeParameter = DefaultGenericParameter<TypeSpecifier>;

/// Represents a syntax tree node for a default generic parameter for a constant.
///
/// Syntax Synopsis:
/// ``` txt
/// DefaultConstParameter:
///     '=' Functional
///     ;
/// ```
type DefaultConstParameter = DefaultGenericParameter<Functional>;

/// Represents a syntax tree node for a type parameter.
///
/// Syntax Synopsis:
/// ```text
/// TypeParameter:
///     Identifier DefaultTypeParameter?
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct TypeParameter {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    default: Option<DefaultTypeParameter>,
}

impl SourceElement for TypeParameter {
    fn span(&self) -> Span {
        self.identifier
            .span
            .join(
                &self
                    .default
                    .as_ref()
                    .map_or_else(|| self.identifier.span.clone(), SourceElement::span),
            )
            .unwrap()
    }
}

/// Represents a syntax tree node for a const parameter.
///
/// Syntax Synopsis:
/// ```text
/// ConstParameter:
///     Identifier TypeAnnotation DefaultConstParameter?
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct ConstParameter {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    type_annotation: TypeAnnotation,
    #[get = "pub"]
    default: Option<DefaultConstParameter>,
}

impl SourceElement for ConstParameter {
    fn span(&self) -> Span {
        self.identifier
            .span
            .join(
                &self
                    .default
                    .as_ref()
                    .map_or_else(|| self.type_annotation.span(), SourceElement::span),
            )
            .unwrap()
    }
}

/// Represents a syntax tree node for a generic parameter.
///
/// Syntax Synopsis:
/// ```text
/// GenericParameter:
///     LifetimeParameter
///     | TypeParameter
///     | ConstParameter
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GenericParameter {
    Lifetime(LifetimeParameter),
    Type(TypeParameter),
    Const(ConstParameter),
}

impl SourceElement for GenericParameter {
    fn span(&self) -> Span {
        match self {
            Self::Lifetime(lifetime_parameter) => lifetime_parameter.span(),
            Self::Type(type_parameter) => type_parameter.span(),
            Self::Const(const_parameter) => const_parameter.span(),
        }
    }
}

/// Represents a syntax tree node for a list of generic parameters separated by commas.
///
/// Syntax Synopsis:
/// ``` text
///  GenericParameterList:
///     GenericParameter (',' GenericParameter)*
///     ;
/// ```
pub type GenericParameterList = ConnectedList<GenericParameter, Punctuation>;

/// Represents a syntax tree node for a generic parameters used in various item definitions.
///
/// Syntax Synopsis:
/// ```text
/// GenericParameters:
///     '<' GenericParameterList '>'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct GenericParameters {
    #[get = "pub"]
    left_angle_bracket: Punctuation,
    #[get = "pub"]
    parameter_list: GenericParameterList,
    #[get = "pub"]
    right_angle_bracket: Punctuation,
}

impl GenericParameters {
    /// Dissolves the [`GenericParameters`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, GenericParameterList, Punctuation) {
        (
            self.left_angle_bracket,
            self.parameter_list,
            self.right_angle_bracket,
        )
    }
}

impl SourceElement for GenericParameters {
    fn span(&self) -> Span {
        self.left_angle_bracket
            .span
            .join(&self.right_angle_bracket.span)
            .unwrap()
    }
}

/// Represents a syntax tree node of a lifetime bound used in a where clause.
///
/// Syntax Synopsis:
/// ``` text
/// LifetimeBound:
///     LifetimeParameter ':' LifetimeArgument ('+' LifetimeArgument)*
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct LifetimeBound {
    #[get = "pub"]
    operand: LifetimeParameter,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    arguments: BoundList<LifetimeArgument>,
}

impl SourceElement for LifetimeBound {
    fn span(&self) -> Span { self.operand.span().join(&self.arguments.span()).unwrap() }
}

/// Represents a syntax tree node of a type bound constraint.
///
/// Syntax Synopsis:
/// ``` text
/// TypeBoundConstraint:
///     TypeSpecifier
///     | LifetimeArgument
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TypeBoundConstraint {
    TypeSpecifier(TypeSpecifier),
    LifetimeArgument(LifetimeArgument),
}

impl SourceElement for TypeBoundConstraint {
    fn span(&self) -> Span {
        match self {
            Self::TypeSpecifier(s) => s.span(),
            Self::LifetimeArgument(s) => s.span(),
        }
    }
}

/// Represents a syntax tree node of a type bound used in a where clause.
///
/// Syntax Synopsis:
/// ``` text
/// TypeBound:
///     TypeSpecifier ':' TypeBoundConstraint ('+' TypeBoundConstraint)*
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct TypeBound {
    #[get = "pub"]
    type_specifier: TypeSpecifier,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    type_bound_constraints: BoundList<TypeBoundConstraint>,
}

impl SourceElement for TypeBound {
    fn span(&self) -> Span {
        self.type_specifier
            .span()
            .join(&self.type_bound_constraints.span())
            .unwrap()
    }
}

/// Similar to [`ConnectedList`] but specifically for list of constraints separated by plus sings
/// and has no trailing separator.
#[derive(Debug, Clone, Getters)]
pub struct BoundList<T> {
    /// The first element of the list.
    #[get = "pub"]
    first: T,

    /// The rest of the elements of the list.
    #[get = "pub"]
    rest: Vec<(Punctuation, T)>,
}

impl<T> BoundList<T> {
    /// Returns an iterator containing references to the elements of the list.
    pub fn elements(&self) -> impl Iterator<Item = &T> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, t)| t))
    }

    /// Returns an iterator containing the elements of the list.
    pub fn into_elements(self) -> impl Iterator<Item = T> {
        std::iter::once(self.first).chain(self.rest.into_iter().map(|(_, t)| t))
    }
}

impl<T: SourceElement> SourceElement for BoundList<T> {
    fn span(&self) -> Span {
        let first = self.first.span();
        match self.rest.last() {
            Some(last) => first.join(&last.1.span()).unwrap(),
            None => first,
        }
    }
}

/// Represents a syntax tree node of a constraint used in a where clause.
///
/// Syntax Synopsis:
/// ``` text
/// Constraint:
///     TraitBound
///     | LifetimeBound
///     | TypeBound
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Constraint {
    Trait(TraitBound),
    Lifetime(LifetimeBound),
    Type(TypeBound),
}

impl SourceElement for Constraint {
    fn span(&self) -> Span {
        match self {
            Self::Trait(s) => s.span(),
            Self::Lifetime(s) => s.span(),
            Self::Type(s) => s.span(),
        }
    }
}

/// Represents a syntax tree node for a trait bound used in a where clause.
///
/// Syntax Synopsis:
/// ``` text
/// TraitBound:
///     'const'? QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct TraitBound {
    #[get = "pub"]
    const_keyword: Option<Keyword>,
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for TraitBound {
    fn span(&self) -> Span { self.qualified_identifier.span() }
}

/// Represents a syntax tree node for a list of constraints separated by commas.
///
/// Syntax Synopsis:
/// ``` text
/// ConstraintList:
///     Constraint (',' Constraint)*
///     ;
/// ```
pub type ConstraintList = ConnectedList<Constraint, Punctuation>;

/// Represents a syntax tree node for a where clause.
///
/// Syntax Synopsis:
/// ``` text
/// WhereClause:
///     'where' ':' ConstraintList
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct WhereClause {
    #[get = "pub"]
    where_keyword: Keyword,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    constraint_list: ConstraintList,
}

impl SourceElement for WhereClause {
    fn span(&self) -> Span {
        self.where_keyword
            .span
            .join(&self.constraint_list.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for a trait signature.
///
/// Syntax Synopsis:
/// ``` text
/// TraitDeclaration:
///     'trait' Identifier GenericParameters? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct TraitSignature {
    #[get = "pub"]
    trait_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
}

impl SourceElement for TraitSignature {
    fn span(&self) -> Span {
        let start = &self.trait_keyword.span;
        self.where_clause.as_ref().map_or_else(
            || {
                self.generic_parameters.as_ref().map_or_else(
                    || start.join(&self.identifier.span).unwrap(),
                    |generic_parameters| start.join(&generic_parameters.span()).unwrap(),
                )
            },
            |where_clause| start.join(&where_clause.span()).unwrap(),
        )
    }
}

/// Represents a syntax tree node for a trait body.
///
/// Syntax Synopsis:
/// ``` text
/// TraitBody:
///     '{' TraitMember* '}'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct TraitBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    members: Vec<TraitMember>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl TraitBody {
    /// Dissolves the [`TraitBody`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Vec<TraitMember>, Punctuation) {
        (self.left_brace, self.members, self.right_brace)
    }
}

impl SourceElement for TraitBody {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
}

/// Represents a syntax tree node for a trait item declaration.
///
/// Syntax Synopsis:
/// ``` text
/// Trait:
///     AccessModifier TraitSignature TraitBody
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Trait {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: TraitSignature,
    #[get = "pub"]
    body: TraitBody,
}

impl Trait {
    /// Dissolves the [`Trait`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (AccessModifier, TraitSignature, TraitBody) {
        (self.access_modifier, self.signature, self.body)
    }
}

impl SourceElement for Trait {
    fn span(&self) -> Span { self.access_modifier.span().join(&self.body.span()).unwrap() }
}

/// Represents a syntax tree node for a trait function member.
///
/// Syntax Synopsis:
/// ``` text
/// TraitFunction:
///     FunctionSignature ';'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct TraitFunction {
    #[get = "pub"]
    signature: FunctionSignature,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl TraitFunction {
    /// Dissolves the [`TraitFunction`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (FunctionSignature, Punctuation) { (self.signature, self.semicolon) }
}

impl SourceElement for TraitFunction {
    fn span(&self) -> Span { self.signature.span().join(&self.semicolon.span).unwrap() }
}

/// Represents a syntax tree node for a trait type member.
///
/// Syntax Synopsis:
/// ``` text
/// TraitType:
///     TypeSignature ';'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct TraitType {
    #[get = "pub"]
    signature: TypeSignature,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for TraitType {
    fn span(&self) -> Span { self.signature.span().join(&self.semicolon.span).unwrap() }
}

/// Represents a syntax tree node for a trait member.
///
/// Syntax Synopsis:
/// ``` text
/// TraitMember:
///     TraitFunction
///     | TraitType
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(clippy::large_enum_variant)]
#[allow(missing_docs)]
pub enum TraitMember {
    Function(TraitFunction),
    Type(TraitType),
}

impl SourceElement for TraitMember {
    fn span(&self) -> Span {
        match self {
            Self::Function(f) => f.span(),
            Self::Type(f) => f.span(),
        }
    }
}

/// Represents a syntax tree node for function parameter.
///
/// Syntax Synopsis:
/// ``` text
/// Parameter:
///     Irrefutable TypeAnnotation
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Parameter {
    #[get = "pub"]
    irrefutable_pattern: pattern::Irrefutable,
    #[get = "pub"]
    type_annotation: TypeAnnotation,
}

impl SourceElement for Parameter {
    fn span(&self) -> Span {
        self.irrefutable_pattern
            .span()
            .join(&self.type_annotation.span())
            .unwrap()
    }
}

/// Represents a list of parameter separated by commas.
///
/// Syntax Synopsis:
/// ``` text
///     ;
/// ```
pub type ParameterList = ConnectedList<Parameter, Punctuation>;

/// Represents a syntax tree node for a list of parameters enclosed in parentheses.
///
/// Syntax Synopsis:
/// ``` text
/// Parameters:
///     '(' ParameterList? ')'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Parameters {
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    parameter_list: Option<ParameterList>,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl Parameters {
    /// Dissolves the [`Parameters`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Option<ParameterList>, Punctuation) {
        (self.left_paren, self.parameter_list, self.right_paren)
    }
}

impl SourceElement for Parameters {
    fn span(&self) -> Span { self.left_paren.span.join(&self.right_paren.span).unwrap() }
}

/// Represents a syntax tree node for a return type in a function signature.
///
/// Syntax Synopsis:
/// ``` text
/// ReturnType:
///     TypeAnnotation
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct ReturnType {
    #[get = "pub"]
    type_annotation: TypeAnnotation,
}

impl SourceElement for ReturnType {
    fn span(&self) -> Span { self.type_annotation.span() }
}

/// Represents a syntax tree node for a function signature.
///
/// Syntax Synopsis:
/// ``` text
/// FunctionSignature:
///     'function' 'const'? Identifier GenericParameters? Parameters ReturnType? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct FunctionSignature {
    #[get = "pub"]
    function_keyword: Keyword,
    #[get = "pub"]
    const_keyword: Option<Keyword>,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
    #[get = "pub"]
    parameters: Parameters,
    #[get = "pub"]
    return_type: Option<ReturnType>,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
}

impl FunctionSignature {
    /// Dissolves the [`FunctionSignature`] into a tuple of its fields.
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn dissolve(
        self,
    ) -> (
        Keyword,
        Option<Keyword>,
        Identifier,
        Option<GenericParameters>,
        Parameters,
        Option<ReturnType>,
        Option<WhereClause>,
    ) {
        (
            self.function_keyword,
            self.const_keyword,
            self.identifier,
            self.generic_parameters,
            self.parameters,
            self.return_type,
            self.where_clause,
        )
    }
}

impl SourceElement for FunctionSignature {
    fn span(&self) -> Span {
        self.where_clause.as_ref().map_or_else(
            || {
                self.return_type.as_ref().map_or_else(
                    || self.identifier.span.join(&self.parameters.span()).unwrap(),
                    |return_type| self.identifier.span.join(&return_type.span()).unwrap(),
                )
            },
            |where_clause| self.identifier.span.join(&where_clause.span()).unwrap(),
        )
    }
}

/// Represents a syntax tree node for a function body.
///
/// Syntax Synopsis:
/// ``` text
/// FunctionBody:
///     '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct FunctionBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    statements: Vec<Statement>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for FunctionBody {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
}

/// Represents a syntax tree node for a function item declaration.
///
/// Syntax Synopsis:
/// ``` text
/// Function:
///     AccessModifier FunctionSignature FunctionBody
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Function {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: FunctionSignature,
    #[get = "pub"]
    body: FunctionBody,
}

impl Function {
    /// Dissolves the [`Function`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (AccessModifier, FunctionSignature, FunctionBody) {
        (self.access_modifier, self.signature, self.body)
    }
}

impl SourceElement for Function {
    fn span(&self) -> Span { self.access_modifier.span().join(&self.body.span()).unwrap() }
}

/// Represents a syntax tree node for a `type` alias signature.
///
/// Syntax Synopsis:
/// ``` text
/// TypeSignature:
///     'type' Identifier GenericParameters?
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct TypeSignature {
    #[get = "pub"]
    type_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
}

impl SourceElement for TypeSignature {
    fn span(&self) -> Span {
        self.generic_parameters.as_ref().map_or_else(
            || {
                self.type_keyword
                    .span
                    .join(&self.identifier.span())
                    .unwrap()
            },
            |generic_parameters| {
                self.type_keyword
                    .span
                    .join(&generic_parameters.span())
                    .unwrap()
            },
        )
    }
}

/// Represents a syntax tree node for a `type` alias declaration.
///
/// Syntax Synopsis:
/// ``` text
/// TypeDeclaration:
///     '=' TypeSpecifier
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct TypeDefinition {
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    type_specifier: TypeSpecifier,
}

impl SourceElement for TypeDefinition {
    fn span(&self) -> Span { self.equals.span.join(&self.type_specifier.span()).unwrap() }
}

/// Represents a syntax tree node for a `type` alias item declaration.
///
/// Syntax Synopsis:
/// ``` text
/// Type:
///     AccessModifier TypeSignature TypeDefinition ';'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Type {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: TypeSignature,
    #[get = "pub"]
    definition: TypeDefinition,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Type {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.semicolon.span)
            .unwrap()
    }
}

/// Represents a syntax tree node for a struct signature.
///
/// Syntax Synopsis:
/// ``` text
/// StructSignature:
///     'struct' Identifier GenericParameters? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct StructSignature {
    #[get = "pub"]
    struct_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
}

impl SourceElement for StructSignature {
    fn span(&self) -> Span {
        self.struct_keyword
            .span
            .join(&self.identifier.span)
            .unwrap()
    }
}

/// Represents a syntax tree node for a struct definition.
///
/// Syntax Synopsis:
/// ``` text
/// StructDefinition:
///     '{' StructMember* '}'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct StructBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    members: Vec<StructMember>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl StructBody {
    /// Dissolves the [`StructBody`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Vec<StructMember>, Punctuation) {
        (self.left_brace, self.members, self.right_brace)
    }
}

impl SourceElement for StructBody {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
}

/// Represents a syntax tree node for a struct item declaration.
///
/// Syntax Synopsis:
/// ``` text
/// Struct:
///     AccessModifier StructSignature StructBody
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Struct {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: StructSignature,
    #[get = "pub"]
    body: StructBody,
}

impl Struct {
    /// Dissolves the [`Struct`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (AccessModifier, StructSignature, StructBody) {
        (self.access_modifier, self.signature, self.body)
    }
}

impl SourceElement for Struct {
    fn span(&self) -> Span { self.access_modifier.span().join(&self.body.span()).unwrap() }
}

/// Represents a syntax tree node for a struct field member.
///
/// Syntax Synopsis:
/// ``` text
/// StructField:
///     AccessModifier Identifier TypeAnnotation ';'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct StructField {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    type_annotation: TypeAnnotation,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for StructField {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.semicolon.span)
            .unwrap()
    }
}

/// Represents a syntax tree node for a struct member.
///
/// Syntax Synopsis:
/// ``` text
/// StructMember:
///     StructField
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum StructMember {
    Field(StructField),
}

impl SourceElement for StructMember {
    fn span(&self) -> Span {
        match self {
            Self::Field(field) => field.span(),
        }
    }
}

/// Represents a syntax tree node for a implements block signature.
///
/// Syntax Synopsis:
/// ``` text
/// ImplementsSignature:
///     'implements' GenericParameters? const? QualifiedIdentifier WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct ImplementsSignature {
    #[get = "pub"]
    implements_keyword: Keyword,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
    #[get = "pub"]
    const_keyword: Option<Keyword>,
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
}

impl SourceElement for ImplementsSignature {
    fn span(&self) -> Span {
        self.implements_keyword
            .span
            .join(&self.qualified_identifier.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for an implements function member.
///
/// Syntax Synopsis:
/// ``` text
/// ImplementsFunction:
///     FunctionSignature FunctionBody
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct ImplementsFunction {
    #[get = "pub"]
    signature: FunctionSignature,
    #[get = "pub"]
    body: FunctionBody,
}

impl ImplementsFunction {
    /// Dissolves the [`ImplementsFunction`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (FunctionSignature, FunctionBody) { (self.signature, self.body) }
}

impl SourceElement for ImplementsFunction {
    fn span(&self) -> Span { self.signature.span().join(&self.body.span()).unwrap() }
}

/// Represents a syntax tree node for an implements type member.
///
/// Syntax Synopsis:
/// ``` text
/// ImplementsType:
///     TypeSignature TypeDefinition ';'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct ImplementsType {
    #[get = "pub"]
    signature: TypeSignature,
    #[get = "pub"]
    definition: TypeDefinition,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for ImplementsType {
    fn span(&self) -> Span { self.signature.span().join(&self.semicolon.span()).unwrap() }
}

/// Represents a syntax tree node for a member in an implements block.
///
/// Syntax Synopsis:
/// ``` text
/// ImplementsMember:
///     ImplementsFunction
///     | ImplementsType
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ImplementsMember {
    Type(ImplementsType),
    Function(ImplementsFunction),
}

impl SourceElement for ImplementsMember {
    fn span(&self) -> Span {
        match self {
            Self::Function(function) => function.span(),
            Self::Type(ty) => ty.span(),
        }
    }
}

/// Represents a syntax tree node for an implements block body.
///
/// Syntax Synopsis:
/// ``` text
/// ImplementsBody:
///     '{' ImplementsMember* '}'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct ImplementsBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    members: Vec<ImplementsMember>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl ImplementsBody {
    /// Dissolves the [`ImplementsBody`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Vec<ImplementsMember>, Punctuation) {
        (self.left_brace, self.members, self.right_brace)
    }
}

impl SourceElement for ImplementsBody {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
}

/// Represents a syntax tree node for an implements block item.
///
/// Syntax Synopsis:
/// ``` text
/// Implements:
///     ImplementsSignature ImplementsBody
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Implements {
    #[get = "pub"]
    signature: ImplementsSignature,
    #[get = "pub"]
    body: ImplementsBody,
}

impl Implements {
    /// Dissolves the [`Implements`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (ImplementsSignature, ImplementsBody) { (self.signature, self.body) }
}

impl SourceElement for Implements {
    fn span(&self) -> Span { self.signature.span().join(&self.body.span()).unwrap() }
}

/// Represents a syntax tree for an enum signature.
///
/// Syntax Synopsis:
/// ```text
/// EnumSignature:
///     'enum' Identifier GenericParameters?
///     ;
/// ``
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct EnumSignature {
    #[get = "pub"]
    enum_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
}

impl SourceElement for EnumSignature {
    fn span(&self) -> Span { self.enum_keyword.span.join(&self.identifier.span).unwrap() }
}

/// Represents a syntax tree for an associated enum value.
///
/// Syntax Synopsis:
/// ``` txt
/// AssociatedValue:
///     '(' TypeSpecifier ')'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct AssociatedValue {
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    type_specifier: TypeSpecifier,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl SourceElement for AssociatedValue {
    fn span(&self) -> Span { self.left_paren.span.join(&self.right_paren.span).unwrap() }
}

/// Represents a syntax tree for an enum variant.
///
/// Syntax Synopsis:
/// ``` text
/// EnumVariant:
///     Identifier AssociatedValue?
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct EnumVariant {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    asscoiated_value: Option<AssociatedValue>,
}

impl SourceElement for EnumVariant {
    fn span(&self) -> Span {
        let end = self
            .asscoiated_value
            .as_ref()
            .map_or_else(|| self.identifier.span.clone(), SourceElement::span);

        self.identifier.span.join(&end).unwrap()
    }
}

/// Represents a syntax tree for a list of enum variant identifiers separated by commas.
///
/// Syntax Synopsis:
/// ``` text
/// EnumVariantList:
///     Identifier (',' Identifier)*
///     ;
/// ```
pub type EnumVariantList = ConnectedList<EnumVariant, Punctuation>;

/// Represents a syntax tree for an enum body.
///
/// Syntax Synopsis:
/// ```text
/// EnumBody:
///     '{' EnumVariantList? '}'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct EnumBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    variant_list: Option<EnumVariantList>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl EnumBody {
    /// Dissolves the [`EnumBody`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Option<EnumVariantList>, Punctuation) {
        (self.left_brace, self.variant_list, self.right_brace)
    }
}

impl SourceElement for EnumBody {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
}

/// Represents a syntax tree for an enum.
///
/// Syntax Synopsis:
/// ```text
/// Enum:
///     AccessModifier EnumSignature EnumBody
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Enum {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: EnumSignature,
    #[get = "pub"]
    body: EnumBody,
}

impl Enum {
    /// Dissolves the [`Enum`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (AccessModifier, EnumSignature, EnumBody) {
        (self.access_modifier, self.signature, self.body)
    }
}

impl SourceElement for Enum {
    fn span(&self) -> Span { self.access_modifier.span().join(&self.body.span()).unwrap() }
}

/// Represents a syntax tree node for a const declaration signature
///
/// Syntax Synopsis:
/// ```text
/// ConstSignature:
///     'const' Identifier TypeAnnotation
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct ConstSignature {
    #[get = "pub"]
    const_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    type_annotation: TypeAnnotation,
}

impl SourceElement for ConstSignature {
    fn span(&self) -> Span {
        self.const_keyword
            .span
            .join(&self.type_annotation.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for a const definition.
///
/// Syntax Synopsis:
/// ```text
/// ConstDefinition:
///     '=' Expression ';'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct ConstDefinition {
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    expression: Expression,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for ConstDefinition {
    fn span(&self) -> Span { self.equals.span.join(&self.semicolon.span).unwrap() }
}

/// Represents a syntax tree node for a const definition.
///
/// Syntax Synopsis:
/// ``` ebnf
/// ConstDefinition:
///     '=' Expression ';'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
pub struct Const {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: ConstSignature,
    #[get = "pub"]
    definition: ConstDefinition,
}

impl SourceElement for Const {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.definition.span())
            .unwrap()
    }
}

/// Represents a syntax tree node for an item.
///
/// Syntax Synopsis:
/// ```text
/// Item:
///     Trait
///     | Function
///     | Type
///     | Struct
///     | Implements
///     | Enum
///     | Module
///     | Const
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(clippy::large_enum_variant)]
#[allow(missing_docs)]
pub enum Item {
    Trait(Trait),
    Function(Function),
    Type(Type),
    Struct(Struct),
    Implements(Implements),
    Enum(Enum),
    Module(Module),
    Const(Const),
}

impl SourceElement for Item {
    fn span(&self) -> Span {
        match self {
            Self::Module(m) => m.span(),
            Self::Trait(t) => t.span(),
            Self::Function(f) => f.span(),
            Self::Type(t) => t.span(),
            Self::Struct(s) => s.span(),
            Self::Implements(i) => i.span(),
            Self::Enum(e) => e.span(),
            Self::Const(c) => c.span(),
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`AccessModifier`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_access_modifier(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<AccessModifier> {
        match self.next_significant_token() {
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Public => {
                Some(AccessModifier::Public(k))
            }
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Private => {
                Some(AccessModifier::Private(k))
            }
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Internal => {
                Some(AccessModifier::Internal(k))
            }
            found => {
                handler.receive(Error::AccessModifierExpected(AccessModifierExpected {
                    found: self.get_actual_found_token(found),
                }));
                None
            }
        }
    }

    fn parse_generic_parameters(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<GenericParameters> {
        let left_angle_bracket = self.parse_punctuation('<', true, handler)?;
        let (generic_parameter_list, right_angle_bracket) = self.parse_enclosed_list_manual(
            '>',
            ',',
            |parser| match parser.stop_at_significant() {
                Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                    // eat apostrophe
                    parser.forward();

                    Some(GenericParameter::Lifetime(LifetimeParameter {
                        apostrophe,
                        identifier: parser.parse_identifier(handler)?,
                    }))
                }
                _ => {
                    let identifier = parser.parse_identifier(handler)?;

                    match parser.stop_at_significant() {
                        // const generic parameter
                        Some(Token::Punctuation(colon)) if colon.punctuation == ':' => {
                            // eat colon
                            parser.forward();

                            let type_specifier = parser.parse_type_specifier(handler)?;
                            let default = match parser.stop_at_significant() {
                                Some(Token::Punctuation(equals)) if equals.punctuation == '=' => {
                                    // eat equals
                                    parser.forward();

                                    Some(DefaultConstParameter {
                                        equals,
                                        value: parser.parse_primary_expression(handler)?,
                                    })
                                }
                                _ => None,
                            };

                            Some(GenericParameter::Const(ConstParameter {
                                identifier,
                                type_annotation: TypeAnnotation {
                                    colon,
                                    type_specifier,
                                },
                                default,
                            }))
                        }

                        // type generic parameter
                        _ => {
                            let default = match parser.stop_at_significant() {
                                Some(Token::Punctuation(equals)) if equals.punctuation == '=' => {
                                    // eat equals
                                    parser.forward();

                                    Some(DefaultTypeParameter {
                                        equals,
                                        value: parser.parse_type_specifier(handler)?,
                                    })
                                }
                                _ => None,
                            };

                            Some(GenericParameter::Type(TypeParameter {
                                identifier,
                                default,
                            }))
                        }
                    }
                }
            },
            handler,
        )?;

        let Some(generic_parameter_list) = generic_parameter_list else {
            handler.receive(Error::GenericArgumentParameterListCannotBeEmpty(
                GenericArgumentParameterListCannotBeEmpty {
                    span: left_angle_bracket
                        .span
                        .join(&right_angle_bracket.span)
                        .expect("Span should be joint successfully"),
                },
            ));
            return None;
        };

        Some(GenericParameters {
            left_angle_bracket,
            parameter_list: generic_parameter_list,
            right_angle_bracket,
        })
    }

    fn parse_type_bound_constraint(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<TypeBoundConstraint> {
        match self.stop_at_significant() {
            Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                // eat apostrophe
                self.forward();

                let lifetime_argument_identifier =
                    self.parse_lifetime_argument_identifier(handler)?;

                Some(TypeBoundConstraint::LifetimeArgument(LifetimeArgument {
                    apostrophe,
                    identifier: lifetime_argument_identifier,
                }))
            }

            _ => Some(TypeBoundConstraint::TypeSpecifier(
                self.parse_type_specifier(handler)?,
            )),
        }
    }

    fn parse_lifetime_argument(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<LifetimeArgument> {
        let apostrophe = self.parse_punctuation('\'', true, handler)?;
        let identifier = self.parse_lifetime_argument_identifier(handler)?;

        Some(LifetimeArgument {
            apostrophe,
            identifier,
        })
    }

    fn parse_cosntraint(&mut self, handler: &impl Handler<Error>) -> Option<Constraint> {
        match self.stop_at_significant() {
            // parse const keyword
            Some(Token::Keyword(const_keyword)) if const_keyword.keyword == KeywordKind::Const => {
                // eat const keyword
                self.forward();

                let qualified_identifier = self.parse_qualified_identifier(false, handler)?;

                Some(Constraint::Trait(TraitBound {
                    const_keyword: Some(const_keyword),
                    qualified_identifier,
                }))
            }

            // parses lifetime argument / bound
            Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                // eat apostrophe
                self.forward();

                let identifier = self.parse_identifier(handler)?;
                let colon = self.parse_punctuation(':', true, handler)?;

                let lhs_lifetime_parameter = LifetimeParameter {
                    apostrophe,
                    identifier,
                };

                let lifetime_bounds = {
                    let first = self.parse_lifetime_argument(handler)?;
                    let mut rest = Vec::new();

                    while let Some(plus) =
                        self.try_parse(|parser| parser.parse_punctuation('+', true, &Dummy))
                    {
                        rest.push((plus, self.parse_lifetime_argument(handler)?));
                    }

                    BoundList { first, rest }
                };

                Some(Constraint::Lifetime(LifetimeBound {
                    operand: lhs_lifetime_parameter,
                    colon,
                    arguments: lifetime_bounds,
                }))
            }
            _ => {
                let type_specifier = self.parse_type_specifier(handler)?;

                match self.stop_at_significant() {
                    Some(Token::Punctuation(colon)) if colon.punctuation == ':' => {
                        // eat colon
                        self.forward();

                        let type_bound_constraints = {
                            let first = self.parse_type_bound_constraint(handler)?;
                            let mut rest = Vec::new();

                            while let Some(plus) =
                                self.try_parse(|parser| parser.parse_punctuation('+', true, &Dummy))
                            {
                                rest.push((plus, self.parse_type_bound_constraint(handler)?));
                            }

                            BoundList { first, rest }
                        };

                        Some(Constraint::Type(TypeBound {
                            type_specifier,
                            colon,
                            type_bound_constraints,
                        }))
                    }

                    found => {
                        if let TypeSpecifier::QualifiedIdentifier(qualified_identifier) =
                            type_specifier
                        {
                            Some(Constraint::Trait(TraitBound {
                                const_keyword: None,
                                qualified_identifier,
                            }))
                        } else {
                            handler.receive(Error::PunctuationExpected(PunctuationExpected {
                                expected: ':',
                                found: self.get_actual_found_token(found),
                            }));

                            None
                        }
                    }
                }
            }
        }
    }

    fn parse_where_clause(&mut self, handler: &impl Handler<Error>) -> Option<WhereClause> {
        let where_keyword = self.parse_keyword(KeywordKind::Where, handler)?;
        let colon = self.parse_punctuation(':', true, handler)?;

        let first = self.parse_cosntraint(handler)?;
        let mut rest = Vec::new();
        let mut trailing_separator = None;

        while let Some(comma) = self.try_parse(|parser| parser.parse_punctuation(',', true, &Dummy))
        {
            if matches!(self.stop_at_significant(), Some(Token::Punctuation(p))
                if p.punctuation == '{' || p.punctuation == ';')
            {
                trailing_separator = Some(comma);
                break;
            }

            let constraint = self.parse_cosntraint(handler)?;
            rest.push((comma, constraint));
        }

        Some(WhereClause {
            where_keyword,
            colon,
            constraint_list: ConnectedList {
                first,
                rest,
                trailing_separator,
            },
        })
    }

    #[allow(clippy::option_option)]
    fn try_parse_where_clause(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<Option<WhereClause>> {
        match self.stop_at_significant() {
            Some(Token::Keyword(where_keyword)) if where_keyword.keyword == KeywordKind::Where => {
                self.parse_where_clause(handler).map(Some)
            }
            _ => Some(None),
        }
    }

    fn parse_function_body(&mut self, handler: &impl Handler<Error>) -> Option<FunctionBody> {
        let left_brace = self.step_into(Delimiter::Brace, handler)?;

        let mut statements = Vec::new();

        while !self.is_exhausted() {
            // parse statements
            if let Some(statement) = self.parse_statement(handler) {
                statements.push(statement);
                continue;
            }

            // try to stop at next statement
            self.stop_at(|token| matches!(token, Token::Punctuation(p) if p.punctuation == ';'));

            // go after the semicolon
            if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == ';')
            {
                self.forward();
            }
        }

        let right_brace = self.step_out(handler)?;

        Some(FunctionBody {
            left_brace,
            statements,
            right_brace,
        })
    }

    fn parse_function_signature(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<FunctionSignature> {
        let function_keyword = self.parse_keyword(KeywordKind::Function, handler)?;
        let const_keyword = match self.stop_at_significant() {
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Const => {
                self.forward();
                Some(k)
            }
            _ => None,
        };

        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;

        let parameters = self.parse_enclosed_tree(
            Delimiter::Parenthesis,
            ',',
            |parser, handler| {
                let irrefutable_pattern = parser.parse_irrefutable_pattern(handler)?;
                let type_annotation = parser.parse_type_annotation(handler)?;

                Some(Parameter {
                    irrefutable_pattern,
                    type_annotation,
                })
            },
            handler,
        )?;

        let parameters = Parameters {
            left_paren: parameters.open,
            parameter_list: parameters.list,
            right_paren: parameters.close,
        };

        let return_type = match self.stop_at_significant() {
            Some(Token::Punctuation(p)) if p.punctuation == ':' => Some(ReturnType {
                type_annotation: self.parse_type_annotation(handler)?,
            }),
            _ => None,
        };

        let where_clause = self.try_parse_where_clause(handler)?;

        Some(FunctionSignature {
            function_keyword,
            const_keyword,
            identifier,
            generic_parameters,
            parameters,
            return_type,
            where_clause,
        })
    }

    #[allow(clippy::option_option)]
    fn try_parse_generic_parameters(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<Option<GenericParameters>> {
        if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == '<')
        {
            Some(Some(self.parse_generic_parameters(handler)?))
        } else {
            Some(None)
        }
    }

    fn parse_implements_member(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<ImplementsMember> {
        match self.stop_at_significant() {
            Some(Token::Keyword(function_keyword))
                if function_keyword.keyword == KeywordKind::Function =>
            {
                let function_signature = self.parse_function_signature(handler)?;
                let function_body = self.parse_function_body(handler)?;

                Some(ImplementsMember::Function(ImplementsFunction {
                    signature: function_signature,
                    body: function_body,
                }))
            }

            Some(Token::Keyword(type_keyword)) if type_keyword.keyword == KeywordKind::Type => {
                let type_signature = self.parse_type_signature(handler)?;
                let type_definition = self.parse_type_definition(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(ImplementsMember::Type(ImplementsType {
                    signature: type_signature,
                    definition: type_definition,
                    semicolon,
                }))
            }

            found => {
                self.forward();
                handler.receive(Error::ImplementsMemberExpected(
                    crate::error::ImplementsMemberExpected {
                        found: self.get_actual_found_token(found),
                    },
                ));
                None
            }
        }
    }

    fn parse_implements_body(&mut self, handler: &impl Handler<Error>) -> Option<ImplementsBody> {
        let left_brace = self.step_into(Delimiter::Brace, handler)?;

        let mut implements_members = Vec::new();

        while !self.is_exhausted() {
            if let Some(member) = self.parse_implements_member(handler) {
                implements_members.push(member);
                continue;
            }

            // try to stop at next function signature
            self.stop_at(|token| matches!(token, Token::Punctuation(p) if p.punctuation == '{'));
            self.forward();
        }

        let right_brace = self
            .step_out(handler)
            .expect("All the tokens should be consumed");

        Some(ImplementsBody {
            left_brace,
            members: implements_members,
            right_brace,
        })
    }

    fn parse_implements(&mut self, handler: &impl Handler<Error>) -> Option<Implements> {
        let implements_keyword = self.parse_keyword(KeywordKind::Implements, handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;
        let const_keyword = match self.stop_at_significant() {
            Some(Token::Keyword(const_keyword)) if const_keyword.keyword == KeywordKind::Const => {
                // eat const keyword
                self.forward();

                Some(const_keyword)
            }
            _ => None,
        };
        let qualified_identifier = self.parse_qualified_identifier(false, handler)?;
        let where_clause = self.try_parse_where_clause(handler)?;
        let implements_body = self.parse_implements_body(handler)?;

        Some(Implements {
            signature: ImplementsSignature {
                implements_keyword,
                generic_parameters,
                const_keyword,
                qualified_identifier,
                where_clause,
            },
            body: implements_body,
        })
    }

    fn parse_trait_signature(&mut self, handler: &impl Handler<Error>) -> Option<TraitSignature> {
        let trait_keyword = self.parse_keyword(KeywordKind::Trait, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;
        let where_clause = self.try_parse_where_clause(handler)?;

        Some(TraitSignature {
            trait_keyword,
            identifier,
            generic_parameters,
            where_clause,
        })
    }

    fn parse_trait_member(&mut self, handler: &impl Handler<Error>) -> Option<TraitMember> {
        match self.stop_at_significant() {
            Some(Token::Keyword(function_keyword))
                if function_keyword.keyword == KeywordKind::Function =>
            {
                let function_signature = self.parse_function_signature(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(TraitMember::Function(TraitFunction {
                    signature: function_signature,
                    semicolon,
                }))
            }

            Some(Token::Keyword(type_keyword)) if type_keyword.keyword == KeywordKind::Type => {
                let type_signature = self.parse_type_signature(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(TraitMember::Type(TraitType {
                    signature: type_signature,
                    semicolon,
                }))
            }

            found => {
                self.forward();
                handler.receive(Error::TraitMemberExpected(
                    crate::error::TraitMemberExpected {
                        found: self.get_actual_found_token(found),
                    },
                ));
                None
            }
        }
    }

    fn parse_trait_body(&mut self, handler: &impl Handler<Error>) -> Option<TraitBody> {
        let left_brace = self.step_into(Delimiter::Brace, handler)?;

        let mut trait_members = Vec::new();

        while !self.is_exhausted() {
            if let Some(trait_member) = self.parse_trait_member(handler) {
                trait_members.push(trait_member);
                continue;
            }

            // try to stop at the next semicolon
            self.stop_at(|token| matches!(token, Token::Punctuation(p) if p.punctuation == ';'));

            // eat semicolon
            self.forward();
        }

        let right_brace = self.step_out(handler)?;

        Some(TraitBody {
            left_brace,
            members: trait_members,
            right_brace,
        })
    }

    fn parse_type_signature(&mut self, handler: &impl Handler<Error>) -> Option<TypeSignature> {
        let type_keyword = self.parse_keyword(KeywordKind::Type, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;

        Some(TypeSignature {
            type_keyword,
            identifier,
            generic_parameters,
        })
    }

    fn parse_type_definition(&mut self, handler: &impl Handler<Error>) -> Option<TypeDefinition> {
        let equals = self.parse_punctuation('=', true, handler)?;
        let type_specifier = self.parse_type_specifier(handler)?;

        Some(TypeDefinition {
            equals,
            type_specifier,
        })
    }

    fn parse_struct_body(&mut self, handler: &impl Handler<Error>) -> Option<StructBody> {
        let left_brace = self.step_into(Delimiter::Brace, handler)?;

        let mut struct_members = Vec::new();

        while !self.is_exhausted() {
            let result: Option<StructMember> = (|| {
                let access_modifier = self.parse_access_modifier(handler)?;
                let identifier = self.parse_identifier(handler)?;
                let type_annotation = self.parse_type_annotation(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(StructMember::Field(StructField {
                    access_modifier,
                    identifier,
                    type_annotation,
                    semicolon,
                }))
            })();

            // pushes a result
            if let Some(struct_member) = result {
                struct_members.push(struct_member);
                continue;
            }

            // stop at next member
            self.stop_at(|token| {
                matches!(token, Token::Keyword(k)
                if matches!(k.keyword,
                    KeywordKind::Public
                    | KeywordKind::Private
                    | KeywordKind::Internal)
                )
            });
        }

        let right_brace = self.step_out(handler)?;

        Some(StructBody {
            left_brace,
            members: struct_members,
            right_brace,
        })
    }

    fn parse_struct_signature(&mut self, handler: &impl Handler<Error>) -> Option<StructSignature> {
        let struct_keyword = self.parse_keyword(KeywordKind::Struct, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;
        let where_clause = self.try_parse_where_clause(handler)?;

        Some(StructSignature {
            struct_keyword,
            identifier,
            generic_parameters,
            where_clause,
        })
    }

    fn parse_enum_signature(&mut self, handler: &impl Handler<Error>) -> Option<EnumSignature> {
        let enum_keyword = self.parse_keyword(KeywordKind::Enum, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;

        Some(EnumSignature {
            enum_keyword,
            identifier,
            generic_parameters,
        })
    }

    fn parse_enum_body(&mut self, handler: &impl Handler<Error>) -> Option<EnumBody> {
        let body = self.parse_enclosed_tree(
            Delimiter::Brace,
            ',',
            |parser, handler| {
                let identifier = parser.parse_identifier(handler)?;

                // parse associated enum value
                let associated_value = if matches!(
                    parser.stop_at_significant(),
                    Some(Token::Punctuation(p)) if p.punctuation == '('
                ) {
                    let left_paren = parser.step_into(Delimiter::Parenthesis, handler)?;
                    let type_specifier = parser.parse_type_specifier(handler)?;
                    let right_paren = parser.step_out(handler)?;
                    Some(AssociatedValue {
                        left_paren,
                        type_specifier,
                        right_paren,
                    })
                } else {
                    None
                };

                Some(EnumVariant {
                    identifier,
                    asscoiated_value: associated_value,
                })
            },
            handler,
        )?;

        Some(EnumBody {
            left_brace: body.open,
            variant_list: body.list,
            right_brace: body.close,
        })
    }

    fn parse_module_signature(&mut self, handler: &impl Handler<Error>) -> Option<ModuleSignature> {
        let module_keyword = self.parse_keyword(KeywordKind::Module, handler)?;
        let identifier = self.parse_identifier(handler)?;

        Some(ModuleSignature {
            module_keyword,
            identifier,
        })
    }

    /// Parses a [`ModulePath`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_module_path(&mut self, handler: &impl Handler<Error>) -> Option<ModulePath> {
        let first_identifier = self.parse_identifier(handler)?;
        let mut rest = Vec::new();

        while let Some(scope_separator) = self.try_parse(|this| this.parse_scope_separator(&Dummy))
        {
            let identifier = self.parse_identifier(handler)?;
            rest.push((scope_separator, identifier));
        }

        Some(ModulePath {
            first: first_identifier,
            rest,
        })
    }

    fn parse_module_body(&mut self, handler: &impl Handler<Error>) -> Option<ModuleBody> {
        let left_brace = self.step_into(Delimiter::Brace, handler)?;

        let mut items = Vec::new();
        let mut usings = Vec::new();

        while !self.is_exhausted() {
            match (items.is_empty(), self.stop_at_significant()) {
                (true, Some(Token::Keyword(using_keyword)))
                    if using_keyword.keyword == KeywordKind::Using =>
                {
                    // eat using keyword
                    self.forward();

                    let module_path = self.parse_module_path(handler)?;
                    let semicolon = self.parse_punctuation(';', true, handler)?;

                    usings.push(Using {
                        using_keyword,
                        module_path,
                        semicolon,
                    });
                    continue;
                }
                _ => {}
            }

            if let Some(item) = self.parse_item(handler) {
                items.push(item);
                continue;
            };

            // try to stop at the next access modifier or usings keyword
            self.stop_at(|token| {
                if let Token::Keyword(keyword) = token {
                    (keyword.keyword == KeywordKind::Public
                        || keyword.keyword == KeywordKind::Private
                        || keyword.keyword == KeywordKind::Internal)
                        || if items.is_empty() {
                            keyword.keyword == KeywordKind::Using
                        } else {
                            false
                        }
                } else {
                    false
                }
            });

            // eat semicolon
            self.forward();
        }

        let right_brace = self.step_out(handler)?;

        Some(ModuleBody {
            left_brace,
            usings,
            items,
            right_brace,
        })
    }

    fn parse_const_signature(&mut self, handler: &impl Handler<Error>) -> Option<ConstSignature> {
        let const_keyword = self.parse_keyword(KeywordKind::Const, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let type_annotation = self.parse_type_annotation(handler)?;

        Some(ConstSignature {
            const_keyword,
            identifier,
            type_annotation,
        })
    }

    fn parse_const_definition(&mut self, handler: &impl Handler<Error>) -> Option<ConstDefinition> {
        let equals = self.parse_punctuation('=', true, handler)?;
        let expression = self.parse_expression(handler)?;
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Some(ConstDefinition {
            equals,
            expression,
            semicolon,
        })
    }

    fn parse_item_with_access_modifier(&mut self, handler: &impl Handler<Error>) -> Option<Item> {
        let access_modifier = self.parse_access_modifier(handler)?;

        match self.stop_at_significant() {
            // parse function
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Function => {
                let function_signature = self.parse_function_signature(handler)?;
                let function_body = self.parse_function_body(handler)?;

                Some(Item::Function(Function {
                    access_modifier,
                    signature: function_signature,
                    body: function_body,
                }))
            }

            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Const => {
                let signature = self.parse_const_signature(handler)?;
                let definition = self.parse_const_definition(handler)?;

                Some(Item::Const(Const {
                    access_modifier,
                    signature,
                    definition,
                }))
            }

            // parse module
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Module => {
                let signature = self.parse_module_signature(handler)?;

                let content = match self.stop_at_significant() {
                    Some(Token::Punctuation(p)) if p.punctuation == ';' => {
                        // eat semi colon
                        self.forward();
                        ModuleContent::File(p)
                    }
                    _ => ModuleContent::Inline(self.parse_module_body(handler)?),
                };

                Some(Item::Module(Module {
                    access_modifier,
                    signature,
                    content,
                }))
            }

            // parse trait
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Trait => {
                let trait_signature = self.parse_trait_signature(handler)?;
                let trait_body = self.parse_trait_body(handler)?;

                Some(Item::Trait(Trait {
                    access_modifier,
                    signature: trait_signature,
                    body: trait_body,
                }))
            }

            // parse struct
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Struct => {
                let struct_signature = self.parse_struct_signature(handler)?;
                let struct_body = self.parse_struct_body(handler)?;

                Some(Item::Struct(Struct {
                    access_modifier,
                    signature: struct_signature,
                    body: struct_body,
                }))
            }

            // parse type
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Type => {
                let type_signature = self.parse_type_signature(handler)?;
                let type_definition = self.parse_type_definition(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(Item::Type(Type {
                    access_modifier,
                    signature: type_signature,
                    definition: type_definition,
                    semicolon,
                }))
            }

            // parse enum
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Enum => {
                let enum_signature = self.parse_enum_signature(handler)?;
                let enum_body = self.parse_enum_body(handler)?;

                Some(Item::Enum(Enum {
                    access_modifier,
                    signature: enum_signature,
                    body: enum_body,
                }))
            }

            found => {
                self.forward();
                handler.receive(Error::ItemExpected(ItemExpected {
                    found: self.get_actual_found_token(found),
                }));
                None
            }
        }
    }

    /// Parses an [`Item`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_item(&mut self, handler: &impl Handler<Error>) -> Option<Item> {
        match self.stop_at_significant() {
            // parses an item with an access modifier
            Some(Token::Keyword(access_modifier))
                if matches!(
                    access_modifier.keyword,
                    KeywordKind::Public | KeywordKind::Private | KeywordKind::Internal
                ) =>
            {
                self.parse_item_with_access_modifier(handler)
            }

            // parses an implements
            Some(Token::Keyword(k)) if k.keyword == KeywordKind::Implements => {
                self.parse_implements(handler).map(Item::Implements)
            }

            found => {
                self.forward();
                handler.receive(Error::ItemExpected(ItemExpected {
                    found: self.get_actual_found_token(found),
                }));
                None
            }
        }
    }
}

#[cfg(test)]
pub(super) mod tests;
