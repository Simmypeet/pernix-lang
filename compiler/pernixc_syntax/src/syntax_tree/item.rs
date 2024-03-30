//! Contains all definition of item syntax trees.

#![allow(missing_docs)]

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    diagnostic::{Dummy, Handler},
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};

use super::{
    expression::Expression, pattern, r#type, statement::Statement,
    AccessModifier, ConnectedList, ConstantArgument, Lifetime,
    LifetimeIdentifier, QualifiedIdentifier, ScopeSeparator,
};
use crate::{
    error::{Error, SyntaxKind},
    parser::{Parser, Reading},
};

/// Syntax Synopsis:
/// ``` txt
/// ModulePath:
///     Identifier ('::' Identifier)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ModulePath {
    #[get = "pub"]
    first: Identifier,
    #[get = "pub"]
    rest: Vec<(ScopeSeparator, Identifier)>,
}

impl SourceElement for ModulePath {
    fn span(&self) -> Span {
        let first = &self.first.span;
        self.rest.last().map_or_else(
            || first.clone(),
            |last| first.join(&last.1.span).unwrap(),
        )
    }
}

impl ModulePath {
    /// Returns an iterator over the path identifiers.
    pub fn paths(&self) -> impl Iterator<Item = &Identifier> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, id)| id))
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Using:
///     'using' ModulePath ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Using {
    #[get = "pub"]
    using_keyword: Keyword,
    #[get = "pub"]
    module_path: ModulePath,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Using {
    fn span(&self) -> Span {
        self.using_keyword.span.join(&self.semicolon.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ModuleSignature:
///     'module' Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ModuleSignature {
    #[get = "pub"]
    module_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for ModuleSignature {
    fn span(&self) -> Span {
        self.module_keyword.span.join(&self.identifier.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Module:
///     AccessModifier ModuleSignature ModuleKind
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Module {
    #[get = "pub"]
    pub(super) access_modifier: AccessModifier,
    #[get = "pub"]
    pub(super) signature: ModuleSignature,
    #[get = "pub"]
    pub(super) kind: ModuleKind,
}

impl Module {
    /// Dissolves the [`Module`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (AccessModifier, ModuleSignature, ModuleKind) {
        (self.access_modifier, self.signature, self.kind)
    }
}

impl SourceElement for Module {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.kind.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ModuleItems:
///     Using*
///     Item*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ModuleContent {
    #[get = "pub"]
    pub(super) usings: Vec<Using>,
    #[get = "pub"]
    pub(super) items: Vec<Item>,
}

impl ModuleContent {
    /// Dissolves the [`ModuleContent`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Vec<Using>, Vec<Item>) {
        (self.usings, self.items)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ModuleContent:
///     ';'
///     | ModuleBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ModuleKind {
    File(Punctuation),
    Inline(ModuleBody),
}

impl SourceElement for ModuleKind {
    fn span(&self) -> Span {
        match self {
            Self::File(p) => p.span.clone(),
            Self::Inline(m) => m.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ModuleBody:
///    '{' Using* Item* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ModuleBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    pub(super) content: ModuleContent,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl ModuleBody {
    /// Dissolves the [`ModuleBody`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, ModuleContent, Punctuation) {
        (self.left_brace, self.content, self.right_brace)
    }
}

impl SourceElement for ModuleBody {
    fn span(&self) -> Span {
        self.left_brace.span.join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// LifetimeParameter:
///     '\'' Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct LifetimeParameter {
    #[get = "pub"]
    apostrophe: Punctuation,
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for LifetimeParameter {
    fn span(&self) -> Span {
        self.apostrophe.span.join(&self.identifier.span).unwrap()
    }
}

/// Represents a syntax tree node for a default generic parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct DefaultGenericParameter<Value: SourceElement> {
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    value: Value,
}

impl<Value: SourceElement> SourceElement for DefaultGenericParameter<Value> {
    fn span(&self) -> Span {
        self.equals.span.join(&self.value.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// DefaultTypeParameter:
///     '=' TypeSpecifier
///     ;
/// ```
type DefaultTypeParameter = DefaultGenericParameter<r#type::Type>;

/// Syntax Synopsis:
/// ``` txt
/// DefaultConstantParameter:
///     '=' Expression
///     ;
/// ```
type DefaultConstantParameter = DefaultGenericParameter<Expression>;

/// Syntax Synopsis:
/// ``` txt
/// TypeParameter:
///     Identifier DefaultTypeParameter?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TypeParameter {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    default: Option<DefaultTypeParameter>,
}

impl TypeParameter {
    /// Dissolves the [`TypeParameter`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Identifier, Option<DefaultTypeParameter>) {
        (self.identifier, self.default)
    }
}

impl SourceElement for TypeParameter {
    fn span(&self) -> Span {
        self.identifier
            .span
            .join(&self.default.as_ref().map_or_else(
                || self.identifier.span.clone(),
                SourceElement::span,
            ))
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ConstantParameter:
///     'const' Identifier TypeAnnotation DefaultConstParameter?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantParameter {
    #[get = "pub"]
    const_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    r#type: r#type::Type,
    #[get = "pub"]
    default: Option<DefaultConstantParameter>,
}

impl ConstantParameter {
    /// Dissolves the [`ConstantParameter`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Keyword,
        Identifier,
        Punctuation,
        r#type::Type,
        Option<DefaultConstantParameter>,
    ) {
        (
            self.const_keyword,
            self.identifier,
            self.colon,
            self.r#type,
            self.default,
        )
    }
}

impl SourceElement for ConstantParameter {
    fn span(&self) -> Span {
        self.const_keyword
            .span
            .join(
                &self
                    .default
                    .as_ref()
                    .map_or_else(|| self.r#type.span(), SourceElement::span),
            )
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// GenericParameter:
///     LifetimeParameter
///     | TypeParameter
///     | ConstantParameter
///     ;
/// ```
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
pub enum GenericParameter {
    Lifetime(LifetimeParameter),
    Type(TypeParameter),
    Constant(ConstantParameter),
}

impl SourceElement for GenericParameter {
    fn span(&self) -> Span {
        match self {
            Self::Lifetime(lifetime_parameter) => lifetime_parameter.span(),
            Self::Type(type_parameter) => type_parameter.span(),
            Self::Constant(const_parameter) => const_parameter.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
///  GenericParameterList:
///     GenericParameter (',' GenericParameter)*
///     ;
/// ```
pub type GenericParameterList = ConnectedList<GenericParameter, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// GenericParameters:
///     '[' GenericParameterList? ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct GenericParameters {
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    parameter_list: Option<GenericParameterList>,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl GenericParameters {
    /// Dissolves the [`GenericParameters`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Punctuation, Option<GenericParameterList>, Punctuation) {
        (self.left_bracket, self.parameter_list, self.right_bracket)
    }
}

impl SourceElement for GenericParameters {
    fn span(&self) -> Span {
        self.left_bracket.span.join(&self.right_bracket.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// LifetimePredicate:
///     LifetimePredicateOperand (+ LifetimePredicateOperand)* ':' LifetimeArgument ('+' LifetimeArgument)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct LifetimePredicate {
    #[get = "pub"]
    operands: BoundList<LifetimePredicateOperand>,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    bounds: BoundList<Lifetime>,
}

impl LifetimePredicate {
    /// Dissolves the [`LifetimeBound`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (BoundList<LifetimePredicateOperand>, Punctuation, BoundList<Lifetime>)
    {
        (self.operands, self.colon, self.bounds)
    }
}

impl SourceElement for LifetimePredicate {
    fn span(&self) -> Span {
        self.operands.span().join(&self.bounds.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// LifetimePredicateOperand:
///     LifetimeParameter
///     | Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum LifetimePredicateOperand {
    LifetimeParameter(LifetimeParameter),
    Type(r#type::Type),
}

impl SourceElement for LifetimePredicateOperand {
    fn span(&self) -> Span {
        match self {
            Self::LifetimeParameter(lifetime_parameter) => {
                lifetime_parameter.span()
            }
            Self::Type(ty) => ty.span(),
        }
    }
}

/// Similar to [`ConnectedList`] but specifically for list of constraints
/// separated by plus sings and has no trailing separator.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// ConstantTypeBound:
///     'const' 'type' Type ('+' Type)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantTypePredicate {
    #[get = "pub"]
    const_keyword: Keyword,
    #[get = "pub"]
    type_keyword: Keyword,
    #[get = "pub"]
    types: BoundList<r#type::Type>,
}

impl SourceElement for ConstantTypePredicate {
    fn span(&self) -> Span {
        self.const_keyword.span.join(&self.types.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TuplePredicate:
///     Type
///     | ConstantArgument
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TuplePredicateOperand {
    Type(r#type::Type),
    Constant(ConstantArgument),
}

impl SourceElement for TuplePredicateOperand {
    fn span(&self) -> Span {
        match self {
            Self::Type(ty) => ty.span(),
            Self::Constant(c) => c.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ConstantPredicate:
///     'tuple' Type ('+' Type)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TuplePredicate {
    #[get = "pub"]
    pub(super) tuple_keyword: Keyword,
    #[get = "pub"]
    pub(super) operands: BoundList<TuplePredicateOperand>,
}

impl SourceElement for TuplePredicate {
    fn span(&self) -> Span {
        self.tuple_keyword.span.join(&self.operands.span()).unwrap()
    }
}

impl TuplePredicate {
    /// Dissolves the [`TuplePredicate`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, BoundList<TuplePredicateOperand>) {
        (self.tuple_keyword, self.operands)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Predicate:
///     TraitMemberPredicate
///     | TraitPredicate
///     | LifetimePredicate
///     | ConstantTypePredicate
///     | TuplePredicate
///     ;
/// ```
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
pub enum Predicate {
    TraitMember(TraitMemberPredicate),
    Trait(TraitPredicate),
    Lifetime(LifetimePredicate),
    ConstantType(ConstantTypePredicate),
    Tuple(TuplePredicate),
}

impl SourceElement for Predicate {
    fn span(&self) -> Span {
        match self {
            Self::TraitMember(s) => s.span(),
            Self::Trait(s) => s.span(),
            Self::Lifetime(s) => s.span(),
            Self::ConstantType(s) => s.span(),
            Self::Tuple(s) => s.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitMemberBoundArgument:
///     Type
///     | TraitConstAssociationBoundArgument
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TraitMemberBound {
    Type(r#type::Type),
    Constant(ConstantArgument),
}

impl SourceElement for TraitMemberBound {
    fn span(&self) -> Span {
        match self {
            Self::Type(ty) => ty.span(),
            Self::Constant(c) => c.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitMemberPredicate:
///     QualifiedIdentifier '=' TraitMemberBound
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitMemberPredicate {
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    bound: TraitMemberBound,
}

impl TraitMemberPredicate {
    /// Dissolves the [`TraitMemberBound`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (QualifiedIdentifier, Punctuation, TraitMemberBound) {
        (self.qualified_identifier, self.equals, self.bound)
    }
}

impl SourceElement for TraitMemberPredicate {
    fn span(&self) -> Span {
        self.qualified_identifier.span().join(&self.bound.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// HigherRankedLifetimeParameters:
///     'for' '[' (LifetimeParameter (',' LifetimeParameter)* ','?)? ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct HigherRankedLifetimeParameters {
    #[get = "pub"]
    for_keyword: Keyword,
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    lifetime_parameter_list:
        Option<ConnectedList<LifetimeParameter, Punctuation>>,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for HigherRankedLifetimeParameters {
    fn span(&self) -> Span {
        self.for_keyword.span.join(&self.right_bracket.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitBound:
///     HigherRankedLifetimeParameters? 'const'? 'trait'
///     QualifiedIdentifier ('+' QualifiedIdentifier)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitPredicate {
    #[get = "pub"]
    higher_ranked_lifetime_parameters: Option<HigherRankedLifetimeParameters>,
    #[get = "pub"]
    const_keyword: Option<Keyword>,
    #[get = "pub"]
    trait_keyword: Keyword,
    #[get = "pub"]
    qualified_identifiers: BoundList<QualifiedIdentifier>,
}

impl SourceElement for TraitPredicate {
    fn span(&self) -> Span {
        let begin =
            self.higher_ranked_lifetime_parameters.as_ref().map_or_else(
                || {
                    self.const_keyword.as_ref().map_or_else(
                        || self.trait_keyword.span.clone(),
                        |const_keyword| const_keyword.span.clone(),
                    )
                },
                SourceElement::span,
            );

        begin.join(&self.qualified_identifiers.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// PredicateList:
///     Predicate (',' Predicate)*
///     ;
/// ```
pub type PredicateList = ConnectedList<Predicate, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// WhereClause:
///     'where' PredicateList
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct WhereClause {
    #[get = "pub"]
    where_keyword: Keyword,
    #[get = "pub"]
    predicate_list: PredicateList,
}

impl WhereClause {
    /// Dissolves the [`WhereClause`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, PredicateList) {
        (self.where_keyword, self.predicate_list)
    }
}

impl SourceElement for WhereClause {
    fn span(&self) -> Span {
        self.where_keyword.span.join(&self.predicate_list.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitDeclaration:
///     'trait' Identifier GenericParameters? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

impl TraitSignature {
    /// Dissolves the [`TraitSignature`] into a tuple of its fields.
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn dissolve(
        self,
    ) -> (Keyword, Identifier, Option<GenericParameters>, Option<WhereClause>)
    {
        (
            self.trait_keyword,
            self.identifier,
            self.generic_parameters,
            self.where_clause,
        )
    }
}

impl SourceElement for TraitSignature {
    fn span(&self) -> Span {
        let start = &self.trait_keyword.span;
        self.where_clause.as_ref().map_or_else(
            || {
                self.generic_parameters.as_ref().map_or_else(
                    || start.join(&self.identifier.span).unwrap(),
                    |generic_parameters| {
                        start.join(&generic_parameters.span()).unwrap()
                    },
                )
            },
            |where_clause| start.join(&where_clause.span()).unwrap(),
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitBody:
///     '{' TraitMember* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
    fn span(&self) -> Span {
        self.left_brace.span.join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Trait:
///     AccessModifier TraitSignature TraitBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.body.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitFunction:
///     AccessModifier FunctionSignature ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitFunction {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: FunctionSignature,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl TraitFunction {
    /// Dissolves the [`TraitFunction`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (AccessModifier, FunctionSignature, Punctuation) {
        (self.access_modifier, self.signature, self.semicolon)
    }
}

impl SourceElement for TraitFunction {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.semicolon.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitType:
///     AccessModifier TypeSignature WhereClause? ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitType {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: TypeSignature,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl TraitType {
    /// Dissolves the [`TraitType`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (AccessModifier, TypeSignature, Option<WhereClause>, Punctuation) {
        (
            self.access_modifier,
            self.signature,
            self.where_clause,
            self.semicolon,
        )
    }
}

impl SourceElement for TraitType {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.semicolon.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitConstant:
///     AccessModifier ConstantSignature WhereClause? ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitConstant {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: ConstantSignature,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for TraitConstant {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.semicolon.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitMember:
///     TraitFunction
///     | TraitType
///     ;
/// ```
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(clippy::large_enum_variant)]
pub enum TraitMember {
    Function(TraitFunction),
    Type(TraitType),
    Constant(TraitConstant),
}

impl SourceElement for TraitMember {
    fn span(&self) -> Span {
        match self {
            Self::Function(f) => f.span(),
            Self::Type(f) => f.span(),
            Self::Constant(f) => f.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Parameter:
///     Irrefutable ':' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Parameter {
    #[get = "pub"]
    irrefutable_pattern: pattern::Irrefutable,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    ty: r#type::Type,
}

impl SourceElement for Parameter {
    fn span(&self) -> Span {
        self.irrefutable_pattern.span().join(&self.ty.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ParameterList:
///     Parameter (',' Parameter)* ','?
///     ;
/// ```
pub type ParameterList = ConnectedList<Parameter, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// Parameters:
///     '(' ParameterList? ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
    fn span(&self) -> Span {
        self.left_paren.span.join(&self.right_paren.span).unwrap()
    }
}

/// Represents a syntax tree node for a return type in a function signature.
///
/// Syntax Synopsis:
/// ``` txt
/// ReturnType:
///     ':' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ReturnType {
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    ty: r#type::Type,
}

impl SourceElement for ReturnType {
    fn span(&self) -> Span { self.colon.span.join(&self.ty.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// FunctionSignature:
///     'function' Identifier GenericParameters? Parameters ReturnType? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct FunctionSignature {
    #[get = "pub"]
    function_keyword: Keyword,
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
        Identifier,
        Option<GenericParameters>,
        Parameters,
        Option<ReturnType>,
        Option<WhereClause>,
    ) {
        (
            self.function_keyword,
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
        let being = self.function_keyword.span.clone();
        let end = self.where_clause.as_ref().map_or_else(
            || {
                self.return_type
                    .as_ref()
                    .map_or_else(|| self.parameters.span(), SourceElement::span)
            },
            SourceElement::span,
        );

        being.join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// FunctionBody:
///     '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct FunctionBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    statements: Vec<Statement>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for FunctionBody {
    fn span(&self) -> Span {
        self.left_brace.span.join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Function:
///     AccessModifier FunctionSignature FunctionBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Function {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    const_keyword: Option<Keyword>,
    #[get = "pub"]
    signature: FunctionSignature,
    #[get = "pub"]
    body: FunctionBody,
}

impl Function {
    /// Dissolves the [`Function`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (AccessModifier, Option<Keyword>, FunctionSignature, FunctionBody)
    {
        (self.access_modifier, self.const_keyword, self.signature, self.body)
    }
}

impl SourceElement for Function {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.body.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TypeSignature:
///     'type' Identifier GenericParameters?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TypeSignature {
    #[get = "pub"]
    type_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
}

impl TypeSignature {
    /// Dissolves the [`TypeSignature`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Identifier, Option<GenericParameters>) {
        (self.type_keyword, self.identifier, self.generic_parameters)
    }
}

impl SourceElement for TypeSignature {
    fn span(&self) -> Span {
        self.generic_parameters.as_ref().map_or_else(
            || self.type_keyword.span.join(&self.identifier.span()).unwrap(),
            |generic_parameters| {
                self.type_keyword.span.join(&generic_parameters.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TypeDeclaration:
///     '=' Type WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TypeDefinition {
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    ty: r#type::Type,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
}

impl TypeDefinition {
    /// Dissolves the [`TypeDefinition`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, r#type::Type, Option<WhereClause>) {
        (self.equals, self.ty, self.where_clause)
    }
}

impl SourceElement for TypeDefinition {
    fn span(&self) -> Span {
        self.equals
            .span
            .join(
                &self
                    .where_clause
                    .as_ref()
                    .map_or_else(|| self.ty.span(), SourceElement::span),
            )
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Type:
///     AccessModifier TypeSignature TypeDefinition ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

impl Type {
    /// Dissolves the [`Type`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (AccessModifier, TypeSignature, TypeDefinition, Punctuation) {
        (self.access_modifier, self.signature, self.definition, self.semicolon)
    }
}

impl SourceElement for Type {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.semicolon.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// StructSignature:
///     'struct' Identifier GenericParameters? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

impl StructSignature {
    /// Dissolves the [`StructSignature`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Keyword, Identifier, Option<GenericParameters>, Option<WhereClause>)
    {
        (
            self.struct_keyword,
            self.identifier,
            self.generic_parameters,
            self.where_clause,
        )
    }
}

impl SourceElement for StructSignature {
    fn span(&self) -> Span {
        self.struct_keyword.span.join(&self.identifier.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// FieldList:
///     Field (',' Field)* ','?
///     ;
/// ```
pub type FieldList = ConnectedList<Field, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// StructDefinition:
///     '{' FieldList* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct StructBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    field_list: Option<FieldList>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl StructBody {
    /// Dissolves the [`StructBody`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Option<FieldList>, Punctuation) {
        (self.left_brace, self.field_list, self.right_brace)
    }
}

impl SourceElement for StructBody {
    fn span(&self) -> Span {
        self.left_brace.span.join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Struct:
///     AccessModifier StructSignature StructBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.body.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// StructField:
///     AccessModifier Identifier ':' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Field {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    r#type: r#type::Type,
}

impl SourceElement for Field {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.r#type.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ImplementationSignature:
///     'implements' GenericParameters? const? QualifiedIdentifier WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ImplementationSignature {
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

impl ImplementationSignature {
    /// Dissolves the [`ImplementationSignature`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Keyword,
        Option<GenericParameters>,
        Option<Keyword>,
        QualifiedIdentifier,
        Option<WhereClause>,
    ) {
        (
            self.implements_keyword,
            self.generic_parameters,
            self.const_keyword,
            self.qualified_identifier,
            self.where_clause,
        )
    }
}

impl SourceElement for ImplementationSignature {
    fn span(&self) -> Span {
        self.implements_keyword
            .span
            .join(&self.qualified_identifier.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ImplementationMember:
///     Function
///     | Type
///     | Constant
///     ;
/// ```
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
pub enum ImplementationMember {
    Type(Type),
    Function(Function),
    Constant(Constant),
}

impl SourceElement for ImplementationMember {
    fn span(&self) -> Span {
        match self {
            Self::Constant(constant) => constant.span(),
            Self::Function(function) => function.span(),
            Self::Type(ty) => ty.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// NegativeImplementation:
///     'delete' ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct NegativeImplementation {
    #[get = "pub"]
    delete_keyword: Keyword,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for NegativeImplementation {
    fn span(&self) -> Span {
        self.delete_keyword.span.join(&self.semicolon.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ImplementationBody:
///     '{' ImplementationMember* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ImplementationBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    members: Vec<ImplementationMember>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl ImplementationBody {
    /// Dissolves the [`ImplementationBody`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Punctuation, Vec<ImplementationMember>, Punctuation) {
        (self.left_brace, self.members, self.right_brace)
    }
}

impl SourceElement for ImplementationBody {
    fn span(&self) -> Span {
        self.left_brace.span.join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ImplementationKind:
///     NegativeImplementation
///     | ImplementationBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ImplementationKind {
    Negative(NegativeImplementation),
    Positive(ImplementationBody),
}

impl SourceElement for ImplementationKind {
    fn span(&self) -> Span {
        match self {
            Self::Negative(negative) => negative.span(),
            Self::Positive(positive) => positive.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Implementation:
///     ImplementationSignature ImplementationKind
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Implementation {
    #[get = "pub"]
    signature: ImplementationSignature,
    #[get = "pub"]
    kind: ImplementationKind,
}

impl Implementation {
    /// Dissolves the [`Implementation`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (ImplementationSignature, ImplementationKind) {
        (self.signature, self.kind)
    }
}

impl SourceElement for Implementation {
    fn span(&self) -> Span {
        self.signature.span().join(&self.kind.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// EnumSignature:
///     'enum' Identifier GenericParameters? WhereClause?
///     ;
/// ``
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct EnumSignature {
    #[get = "pub"]
    enum_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
}

impl EnumSignature {
    /// Dissolves the [`EnumSignature`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Keyword, Identifier, Option<GenericParameters>, Option<WhereClause>)
    {
        (
            self.enum_keyword,
            self.identifier,
            self.generic_parameters,
            self.where_clause,
        )
    }
}

impl SourceElement for EnumSignature {
    fn span(&self) -> Span {
        self.enum_keyword.span.join(&self.identifier.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// VariantAssociation:
///     '(' TypeSpecifier ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct VariantAssociation {
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    ty: r#type::Type,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl VariantAssociation {
    /// Dissolves the [`VariantAssociation`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, r#type::Type, Punctuation) {
        (self.left_paren, self.ty, self.right_paren)
    }
}

impl SourceElement for VariantAssociation {
    fn span(&self) -> Span {
        self.left_paren.span.join(&self.right_paren.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// EnumVariant:
///     Identifier VariantAssociation?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Variant {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    association: Option<VariantAssociation>,
}

impl Variant {
    /// Dissolves the [`Variant`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Identifier, Option<VariantAssociation>) {
        (self.identifier, self.association)
    }
}

impl SourceElement for Variant {
    fn span(&self) -> Span {
        let end = self
            .association
            .as_ref()
            .map_or_else(|| self.identifier.span.clone(), SourceElement::span);

        self.identifier.span.join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// VariantList:
///     Identifier (',' Identifier)*
///     ;
/// ```
pub type VariantList = ConnectedList<Variant, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// EnumBody:
///     '{' VariantList? '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct EnumBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    variant_list: Option<VariantList>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl EnumBody {
    /// Dissolves the [`EnumBody`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Option<VariantList>, Punctuation) {
        (self.left_brace, self.variant_list, self.right_brace)
    }
}

impl SourceElement for EnumBody {
    fn span(&self) -> Span {
        self.left_brace.span.join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Enum:
///     AccessModifier EnumSignature EnumBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.body.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ConstSignature:
///     'const' Identifier GenericParameters? ':' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantSignature {
    #[get = "pub"]
    const_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    ty: r#type::Type,
}

impl SourceElement for ConstantSignature {
    fn span(&self) -> Span {
        self.const_keyword.span.join(&self.ty.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ConstDefinition:
///     '=' Expression WhereClause? ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantDefinition {
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    expression: Expression,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for ConstantDefinition {
    fn span(&self) -> Span {
        self.equals.span.join(&self.semicolon.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Constant:
///     '=' Expression ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Constant {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: ConstantSignature,
    #[get = "pub"]
    definition: ConstantDefinition,
}

impl SourceElement for Constant {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.definition.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Item:
///     Trait
///     | Function
///     | Type
///     | Struct
///     | Implementation
///     | Enum
///     | Module
///     | Const
///     ;
/// ```
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(clippy::large_enum_variant)]
pub enum Item {
    Trait(Trait),
    Function(Function),
    Type(Type),
    Struct(Struct),
    Implementation(Implementation),
    Enum(Enum),
    Module(Module),
    Constant(Constant),
}

impl SourceElement for Item {
    fn span(&self) -> Span {
        match self {
            Self::Module(m) => m.span(),
            Self::Trait(t) => t.span(),
            Self::Function(f) => f.span(),
            Self::Type(t) => t.span(),
            Self::Struct(s) => s.span(),
            Self::Implementation(i) => i.span(),
            Self::Enum(e) => e.span(),
            Self::Constant(c) => c.span(),
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`AccessModifier`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_access_modifier(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<AccessModifier> {
        match self.next_significant_token() {
            Reading::Unit(Token::Keyword(k))
                if k.kind == KeywordKind::Public =>
            {
                Some(AccessModifier::Public(k))
            }
            Reading::Unit(Token::Keyword(k))
                if k.kind == KeywordKind::Private =>
            {
                Some(AccessModifier::Private(k))
            }
            Reading::Unit(Token::Keyword(k))
                if k.kind == KeywordKind::Internal =>
            {
                Some(AccessModifier::Internal(k))
            }
            found => {
                handler.receive(Error {
                    expected: SyntaxKind::AccessModifier,
                    found: found.into_token(),
                    alternatives: Vec::new(),
                });
                None
            }
        }
    }

    fn parse_generic_parameters(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<GenericParameters> {
        let tree = self.parse_enclosed_list(
            Delimiter::Bracket,
            ',',
            |parser| {
                match parser.next_significant_token() {
                    Reading::Unit(Token::Identifier(identifier)) => {
                        Some(GenericParameter::Type(TypeParameter {
                            identifier,
                            default: match parser.stop_at_significant() {
                                Reading::Unit(Token::Punctuation(equals))
                                    if equals.punctuation == '=' =>
                                {
                                    // eat equals
                                    parser.forward();

                                    let value = parser.parse_type(handler)?;

                                    Some(DefaultGenericParameter {
                                        equals,
                                        value,
                                    })
                                }
                                _ => None,
                            },
                        }))
                    }

                    Reading::Unit(Token::Punctuation(apostrophe))
                        if apostrophe.punctuation == '\'' =>
                    {
                        let identifier = parser.parse_identifier(handler)?;

                        Some(GenericParameter::Lifetime(LifetimeParameter {
                            apostrophe,
                            identifier,
                        }))
                    }

                    Reading::Unit(Token::Keyword(const_keyword))
                        if const_keyword.kind == KeywordKind::Const =>
                    {
                        let identifier = parser.parse_identifier(handler)?;
                        let colon =
                            parser.parse_punctuation(':', true, handler)?;
                        let r#type = parser.parse_type(handler)?;

                        Some(GenericParameter::Constant(ConstantParameter {
                            const_keyword,
                            identifier,
                            colon,
                            r#type,
                            default: match parser.stop_at_significant() {
                                Reading::Unit(Token::Punctuation(equals))
                                    if equals.punctuation == '=' =>
                                {
                                    // eat equals
                                    parser.forward();

                                    let value =
                                        parser.parse_expression(handler)?;

                                    Some(DefaultGenericParameter {
                                        equals,
                                        value,
                                    })
                                }
                                _ => None,
                            },
                        }))
                    }

                    found => {
                        handler.receive(Error {
                            expected: SyntaxKind::GenericParameter,
                            alternatives: Vec::new(),
                            found: found.into_token(),
                        });
                        None
                    }
                }
            },
            handler,
        )?;

        Some(GenericParameters {
            left_bracket: tree.open,
            parameter_list: tree.list,
            right_bracket: tree.close,
        })
    }

    fn parse_lifetime_argument(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Lifetime> {
        let apostrophe = self.parse_punctuation('\'', true, handler)?;
        let identifier = match self.next_token() {
            Reading::Unit(Token::Identifier(identifier)) => {
                LifetimeIdentifier::Identifier(identifier)
            }
            Reading::Unit(Token::Keyword(static_keyword))
                if static_keyword.kind == KeywordKind::Static =>
            {
                LifetimeIdentifier::Static(static_keyword)
            }
            found => {
                handler.receive(Error {
                    expected: SyntaxKind::Identifier,
                    alternatives: vec![SyntaxKind::Keyword(
                        KeywordKind::Static,
                    )],
                    found: found.into_token(),
                });
                return None;
            }
        };

        Some(Lifetime { apostrophe, identifier })
    }

    fn parse_lifetime_bound_list(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<BoundList<Lifetime>> {
        let first = self.parse_lifetime_argument(handler)?;
        let mut rest = Vec::new();

        while let Some(plus) =
            self.try_parse(|parser| parser.parse_punctuation('+', true, &Dummy))
        {
            rest.push((plus, self.parse_lifetime_argument(handler)?));
        }

        Some(BoundList { first, rest })
    }

    fn parse_lifetime_predicate_operand(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<LifetimePredicateOperand> {
        match self.stop_at_significant() {
            Reading::Unit(Token::Punctuation(apostrophe))
                if apostrophe.punctuation == '\'' =>
            {
                self.forward();

                Some(LifetimePredicateOperand::LifetimeParameter(
                    LifetimeParameter {
                        apostrophe,
                        identifier: match self.next_token() {
                            Reading::Unit(Token::Identifier(ident)) => ident,
                            found => {
                                handler.receive(Error {
                                    expected: SyntaxKind::Identifier,
                                    alternatives: Vec::new(),
                                    found: found.into_token(),
                                });
                                return None;
                            }
                        },
                    },
                ))
            }

            _ => {
                Some(LifetimePredicateOperand::Type(self.parse_type(handler)?))
            }
        }
    }

    fn parse_bound_list<T>(
        &mut self,
        mut parser: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<BoundList<T>> {
        let first = parser(self)?;
        let mut rest = Vec::new();

        while let Some(plus) =
            self.try_parse(|parser| parser.parse_punctuation('+', true, &Dummy))
        {
            rest.push((plus, parser(self)?));
        }

        Some(BoundList { first, rest })
    }

    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn parse_predicate(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Predicate> {
        match self.stop_at_significant() {
            Reading::Unit(Token::Keyword(const_keyword))
                if const_keyword.kind == KeywordKind::Const =>
            {
                // eat const keyword
                self.forward();

                match self.stop_at_significant() {
                    Reading::Unit(Token::Keyword(trait_keyword))
                        if trait_keyword.kind == KeywordKind::Trait =>
                    {
                        // eat trait keyword
                        self.forward();

                        Some(Predicate::Trait(TraitPredicate {
                            higher_ranked_lifetime_parameters: None,
                            const_keyword: Some(const_keyword),
                            trait_keyword,
                            qualified_identifiers: self.parse_bound_list(
                                |parser| {
                                    parser.parse_qualified_identifier(handler)
                                },
                            )?,
                        }))
                    }

                    Reading::Unit(Token::Keyword(type_keyword))
                        if type_keyword.kind == KeywordKind::Type =>
                    {
                        // eat type keyword
                        self.forward();

                        Some(Predicate::ConstantType(ConstantTypePredicate {
                            const_keyword,
                            type_keyword,
                            types: self.parse_bound_list(|parser| {
                                parser.parse_type(handler)
                            })?,
                        }))
                    }

                    found => {
                        handler.receive(Error {
                            expected: SyntaxKind::Keyword(KeywordKind::Trait),
                            alternatives: vec![SyntaxKind::Keyword(
                                KeywordKind::Type,
                            )],
                            found: found.into_token(),
                        });
                        None
                    }
                }
            }

            Reading::Unit(Token::Keyword(for_keyword))
                if for_keyword.kind == KeywordKind::For =>
            {
                // eat for keyword
                self.forward();

                let lifetimes = self.parse_enclosed_list(
                    Delimiter::Bracket,
                    ',',
                    |parser| {
                        let apostrophe =
                            parser.parse_punctuation('\'', true, handler)?;
                        let identifier = parser.parse_identifier(handler)?;

                        Some(LifetimeParameter { apostrophe, identifier })
                    },
                    handler,
                )?;

                let higher_ranked_lifetime = HigherRankedLifetimeParameters {
                    for_keyword,
                    left_bracket: lifetimes.open,
                    lifetime_parameter_list: lifetimes.list,
                    right_bracket: lifetimes.close,
                };

                let const_keyword = self.try_parse(|parser| {
                    parser.parse_keyword(KeywordKind::Const, &Dummy)
                });

                let trait_keyword =
                    self.parse_keyword(KeywordKind::Trait, handler)?;

                Some(Predicate::Trait(TraitPredicate {
                    higher_ranked_lifetime_parameters: Some(
                        higher_ranked_lifetime,
                    ),
                    const_keyword,
                    trait_keyword,
                    qualified_identifiers: self.parse_bound_list(|parser| {
                        parser.parse_qualified_identifier(handler)
                    })?,
                }))
            }

            Reading::Unit(Token::Keyword(trait_keyword))
                if trait_keyword.kind == KeywordKind::Trait =>
            {
                // eat trait keyword
                self.forward();

                Some(Predicate::Trait(TraitPredicate {
                    higher_ranked_lifetime_parameters: None,
                    const_keyword: None,
                    trait_keyword,
                    qualified_identifiers: self.parse_bound_list(|parser| {
                        parser.parse_qualified_identifier(handler)
                    })?,
                }))
            }

            Reading::Unit(Token::Keyword(tuple_keyword))
                if tuple_keyword.kind == KeywordKind::Tuple =>
            {
                // eat token keyword
                self.forward();

                let types = self.parse_bound_list(|parser| {
                    match parser.stop_at_significant() {
                        Reading::IntoDelimited(Delimiter::Brace, _) => {
                            let expr = parser.step_into(
                                Delimiter::Brace,
                                |parser| parser.parse_expression(handler),
                                handler,
                            )?;

                            Some(TuplePredicateOperand::Constant(
                                ConstantArgument {
                                    left_brace: expr.open,
                                    expression: Box::new(expr.tree?),
                                    right_brace: expr.close,
                                },
                            ))
                        }
                        _ => parser
                            .parse_type(handler)
                            .map(TuplePredicateOperand::Type),
                    }
                })?;

                Some(Predicate::Tuple(TuplePredicate {
                    tuple_keyword,
                    operands: types,
                }))
            }

            Reading::Unit(Token::Punctuation(apostrophe))
                if apostrophe.punctuation == '\'' =>
            {
                let operands = self.parse_bound_list(|parser| {
                    parser.parse_lifetime_predicate_operand(handler)
                })?;
                let colon = self.parse_punctuation(':', true, handler)?;
                let bounds = self.parse_lifetime_bound_list(handler)?;

                Some(Predicate::Lifetime(LifetimePredicate {
                    operands,
                    colon,
                    bounds,
                }))
            }

            _ => {
                let first_ty = self.parse_type(handler)?;

                match (first_ty, self.stop_at_significant()) {
                    // parse member constraint
                    (
                        r#type::Type::QualifiedIdentifier(qualified_identifier),
                        Reading::Unit(Token::Punctuation(equals)),
                    ) if equals.punctuation == '=' => {
                        // eat equals
                        self.forward();

                        let bound = match self.stop_at_significant() {
                            Reading::IntoDelimited(Delimiter::Brace, _) => {
                                let expr = self.step_into(
                                    Delimiter::Brace,
                                    |parser| parser.parse_expression(handler),
                                    handler,
                                )?;

                                TraitMemberBound::Constant(ConstantArgument {
                                    left_brace: expr.open,
                                    expression: Box::new(expr.tree?),
                                    right_brace: expr.close,
                                })
                            }
                            _ => TraitMemberBound::Type(
                                self.parse_type(handler)?,
                            ),
                        };

                        Some(Predicate::TraitMember(TraitMemberPredicate {
                            qualified_identifier,
                            equals,
                            bound,
                        }))
                    }

                    (first_ty, _) => {
                        let mut rest = Vec::new();

                        while let Some(plus) = self.try_parse(|parser| {
                            parser.parse_punctuation('+', true, &Dummy)
                        }) {
                            rest.push((
                                plus,
                                self.parse_lifetime_predicate_operand(handler)?,
                            ));
                        }

                        let operands = BoundList {
                            first: LifetimePredicateOperand::Type(first_ty),
                            rest,
                        };
                        let colon =
                            self.parse_punctuation(':', true, handler)?;
                        let bounds = self.parse_lifetime_bound_list(handler)?;

                        Some(Predicate::Lifetime(LifetimePredicate {
                            operands,
                            colon,
                            bounds,
                        }))
                    }
                }
            }
        }
    }

    fn parse_where_clause(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<WhereClause> {
        let where_keyword = self.parse_keyword(KeywordKind::Where, handler)?;

        let first = self.parse_predicate(handler)?;
        let mut rest = Vec::new();
        let mut trailing_separator = None;

        while let Some(comma) =
            self.try_parse(|parser| parser.parse_punctuation(',', true, &Dummy))
        {
            if matches!(
                self.stop_at_significant(),
                Reading::Unit(Token::Punctuation(p)) if p.punctuation == ';'
            ) || matches!(
                self.stop_at_significant(),
                Reading::IntoDelimited(Delimiter::Brace, _)
            ) || matches!(
                self.stop_at_significant(),
                Reading::Unit(Token::Keyword(keyword)) if keyword.kind == KeywordKind::Delete
            ) {
                trailing_separator = Some(comma);
                break;
            }

            let constraint = self.parse_predicate(handler)?;
            rest.push((comma, constraint));
        }

        Some(WhereClause {
            where_keyword,
            predicate_list: ConnectedList { first, rest, trailing_separator },
        })
    }

    #[allow(clippy::option_option)]
    fn try_parse_where_clause(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Option<WhereClause>> {
        match self.stop_at_significant() {
            Reading::Unit(Token::Keyword(where_keyword))
                if where_keyword.kind == KeywordKind::Where =>
            {
                self.parse_where_clause(handler).map(Some)
            }
            _ => Some(None),
        }
    }

    fn parse_function_body(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<FunctionBody> {
        let delimited_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut statements = Vec::new();

                while !parser.is_exhausted() {
                    // parse statements
                    if let Some(statement) = parser.parse_statement(handler) {
                        statements.push(statement);
                        continue;
                    }

                    // try to stop at next statement
                    parser.stop_at(|token| {
                        matches!(
                            token,
                            Reading::Unit(Token::Punctuation(p)) if p.punctuation == ';'
                        )
                    });

                    // go after the semicolon
                    if matches!(
                        parser.stop_at_significant(),
                        Reading::Unit(Token::Punctuation(p)) if p.punctuation == ';'
                    ) {
                        parser.forward();
                    }
                }

                Some(statements)
            },
            handler,
        )?;

        Some(FunctionBody {
            left_brace: delimited_tree.open,
            statements: delimited_tree.tree?,
            right_brace: delimited_tree.close,
        })
    }

    fn parse_function_signature(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<FunctionSignature> {
        let function_keyword =
            self.parse_keyword(KeywordKind::Function, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;

        let parameters = self.parse_enclosed_list(
            Delimiter::Parenthesis,
            ',',
            |parser| {
                let irrefutable_pattern =
                    parser.parse_irrefutable_pattern(handler)?;
                let colon = parser.parse_punctuation(':', true, handler)?;
                let ty = parser.parse_type(handler)?;

                Some(Parameter { irrefutable_pattern, colon, ty })
            },
            handler,
        )?;

        let parameters = Parameters {
            left_paren: parameters.open,
            parameter_list: parameters.list,
            right_paren: parameters.close,
        };

        let return_type = match self.stop_at_significant() {
            Reading::Unit(Token::Punctuation(p)) if p.punctuation == ':' => {
                Some(ReturnType {
                    colon: self.parse_punctuation(':', true, handler)?,
                    ty: self.parse_type(handler)?,
                })
            }
            _ => None,
        };

        let where_clause = self.try_parse_where_clause(handler)?;

        Some(FunctionSignature {
            function_keyword,
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
        handler: &dyn Handler<Error>,
    ) -> Option<Option<GenericParameters>> {
        if matches!(
            self.stop_at_significant(),
            Reading::IntoDelimited(Delimiter::Bracket, _)
        ) {
            Some(Some(self.parse_generic_parameters(handler)?))
        } else {
            Some(None)
        }
    }

    fn parse_implements_member(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<ImplementationMember> {
        let access_modifier = self.parse_access_modifier(handler)?;

        match self.stop_at_significant() {
            Reading::Unit(Token::Keyword(type_keyword))
                if type_keyword.kind == KeywordKind::Type =>
            {
                let signature = self.parse_type_signature(handler)?;
                let definition = self.parse_type_definition(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(ImplementationMember::Type(Type {
                    access_modifier,
                    signature,
                    definition,
                    semicolon,
                }))
            }

            Reading::Unit(Token::Keyword(function_keyword))
                if function_keyword.kind == KeywordKind::Function =>
            {
                let signature = self.parse_function_signature(handler)?;
                let body = self.parse_function_body(handler)?;

                Some(ImplementationMember::Function(Function {
                    access_modifier,
                    const_keyword: None,
                    signature,
                    body,
                }))
            }

            Reading::Unit(Token::Keyword(const_keyword))
                if const_keyword.kind == KeywordKind::Const =>
            {
                // eat const Keyword
                self.forward();

                match self.stop_at_significant() {
                    Reading::Unit(Token::Keyword(function_keyword))
                        if function_keyword.kind == KeywordKind::Function =>
                    {
                        let signature =
                            self.parse_function_signature(handler)?;
                        let body = self.parse_function_body(handler)?;

                        Some(ImplementationMember::Function(Function {
                            access_modifier,
                            const_keyword: Some(const_keyword),
                            signature,
                            body,
                        }))
                    }

                    Reading::Unit(Token::Identifier(..)) => {
                        let signature = self.parse_const_signature(
                            Some(const_keyword),
                            handler,
                        )?;
                        let definition =
                            self.parse_const_definition(handler)?;

                        Some(ImplementationMember::Constant(Constant {
                            access_modifier,
                            signature,
                            definition,
                        }))
                    }

                    found => {
                        handler.receive(Error {
                            expected: SyntaxKind::Keyword(KeywordKind::Const),
                            alternatives: vec![SyntaxKind::Identifier],
                            found: found.into_token(),
                        });
                        None
                    }
                }
            }

            found => {
                handler.receive(Error {
                    expected: SyntaxKind::ImplementationMember,
                    alternatives: vec![],
                    found: found.into_token(),
                });
                None
            }
        }
    }

    fn parse_implements_body(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<ImplementationBody> {
        let delimited_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut implements_members = Vec::new();

                while !parser.is_exhausted() {
                    if let Some(member) =
                        parser.parse_implements_member(handler)
                    {
                        implements_members.push(member);
                        continue;
                    }

                    // try to stop at next function signature
                    parser.stop_at(|token| {
                        matches!(
                            token,
                            Reading::IntoDelimited(Delimiter::Brace, _)
                        )
                    });
                    parser.forward();
                }

                Some(implements_members)
            },
            handler,
        )?;

        Some(ImplementationBody {
            left_brace: delimited_tree.open,
            members: delimited_tree.tree?,
            right_brace: delimited_tree.close,
        })
    }

    fn parse_implements(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Implementation> {
        let implements_keyword =
            self.parse_keyword(KeywordKind::Implements, handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;
        let const_keyword = match self.stop_at_significant() {
            Reading::Unit(Token::Keyword(const_keyword))
                if const_keyword.kind == KeywordKind::Const =>
            {
                // eat const keyword
                self.forward();

                Some(const_keyword)
            }
            _ => None,
        };
        let qualified_identifier = self.parse_qualified_identifier(handler)?;
        let where_clause = self.try_parse_where_clause(handler)?;

        let kind = match self.stop_at_significant() {
            Reading::Unit(Token::Keyword(delete_keyword))
                if delete_keyword.kind == KeywordKind::Delete =>
            {
                // eat delete
                self.forward();

                let semicolon = self.parse_punctuation(';', true, handler)?;

                ImplementationKind::Negative(NegativeImplementation {
                    delete_keyword,
                    semicolon,
                })
            }
            _ => ImplementationKind::Positive(
                self.parse_implements_body(handler)?,
            ),
        };

        Some(Implementation {
            signature: ImplementationSignature {
                implements_keyword,
                generic_parameters,
                const_keyword,
                qualified_identifier,
                where_clause,
            },
            kind,
        })
    }

    fn parse_trait_signature(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<TraitSignature> {
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

    fn parse_trait_member(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<TraitMember> {
        let access_modifier = self.parse_access_modifier(handler)?;
        match self.stop_at_significant() {
            Reading::Unit(Token::Keyword(function_keyword))
                if function_keyword.kind == KeywordKind::Function =>
            {
                let function_signature =
                    self.parse_function_signature(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(TraitMember::Function(TraitFunction {
                    access_modifier,
                    signature: function_signature,
                    semicolon,
                }))
            }

            Reading::Unit(Token::Keyword(const_keyword))
                if const_keyword.kind == KeywordKind::Const =>
            {
                let const_keyword =
                    self.parse_keyword(KeywordKind::Const, handler)?;
                let identifier = self.parse_identifier(handler)?;
                let generic_parameters =
                    self.try_parse_generic_parameters(handler)?;
                let colon = self.parse_punctuation(':', true, handler)?;
                let ty = self.parse_type(handler)?;
                let where_clause = self.try_parse_where_clause(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(TraitMember::Constant(TraitConstant {
                    access_modifier,
                    signature: ConstantSignature {
                        const_keyword,
                        identifier,
                        generic_parameters,
                        colon,
                        ty,
                    },
                    where_clause,
                    semicolon,
                }))
            }

            Reading::Unit(Token::Keyword(type_keyword))
                if type_keyword.kind == KeywordKind::Type =>
            {
                let signature = self.parse_type_signature(handler)?;
                let where_clause = self.try_parse_where_clause(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(TraitMember::Type(TraitType {
                    access_modifier,
                    signature,
                    where_clause,
                    semicolon,
                }))
            }

            found => {
                self.forward();
                handler.receive(Error {
                    expected: SyntaxKind::TraitMember,
                    alternatives: Vec::new(),
                    found: found.into_token(),
                });
                None
            }
        }
    }

    fn parse_trait_body(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<TraitBody> {
        let delimited_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut trait_members = Vec::new();

                while !parser.is_exhausted() {
                    if let Some(trait_member) = parser.parse_trait_member(handler) {
                        trait_members.push(trait_member);
                        continue;
                    }

                    // try to stop at the next semicolon
                    parser.stop_at(|token| {
                        matches!(
                            token,
                            Reading::Unit(Token::Punctuation(p)) if p.punctuation == ';'
                        )
                    });

                    // eat semicolon
                    parser.forward();
                }
                Some(trait_members)
            },
            handler,
        )?;

        Some(TraitBody {
            left_brace: delimited_tree.open,
            members: delimited_tree.tree?,
            right_brace: delimited_tree.close,
        })
    }

    fn parse_type_signature(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<TypeSignature> {
        let type_keyword = self.parse_keyword(KeywordKind::Type, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;

        Some(TypeSignature { type_keyword, identifier, generic_parameters })
    }

    fn parse_type_definition(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<TypeDefinition> {
        let equals = self.parse_punctuation('=', true, handler)?;
        let ty = self.parse_type(handler)?;
        let where_clause = self.try_parse_where_clause(handler)?;

        Some(TypeDefinition { equals, ty, where_clause })
    }

    fn parse_struct_body(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<StructBody> {
        let enclosed_list = self.parse_enclosed_list(
            Delimiter::Brace,
            ',',
            |parser| {
                let access_modifier = parser.parse_access_modifier(handler)?;
                let identifier = parser.parse_identifier(handler)?;
                let colon = parser.parse_punctuation(':', true, handler)?;
                let ty = parser.parse_type(handler)?;

                Some(Field { access_modifier, identifier, colon, r#type: ty })
            },
            handler,
        )?;

        Some(StructBody {
            left_brace: enclosed_list.open,
            field_list: enclosed_list.list,
            right_brace: enclosed_list.close,
        })
    }

    fn parse_struct_signature(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<StructSignature> {
        let struct_keyword =
            self.parse_keyword(KeywordKind::Struct, handler)?;
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

    fn parse_enum_signature(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<EnumSignature> {
        let enum_keyword = self.parse_keyword(KeywordKind::Enum, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;
        let where_clause = self.try_parse_where_clause(handler)?;

        Some(EnumSignature {
            enum_keyword,
            identifier,
            generic_parameters,
            where_clause,
        })
    }

    fn parse_enum_body(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<EnumBody> {
        let body = self.parse_enclosed_list(
            Delimiter::Brace,
            ',',
            |parser| {
                let identifier = parser.parse_identifier(handler)?;

                // parse associated enum value
                let associated_value = if matches!(
                    parser.stop_at_significant(),
                    Reading::IntoDelimited(Delimiter::Parenthesis, _)
                ) {
                    let delimited_tree = parser.step_into(
                        Delimiter::Parenthesis,
                        |parser| parser.parse_type(handler),
                        handler,
                    )?;

                    Some(VariantAssociation {
                        left_paren: delimited_tree.open,
                        ty: delimited_tree.tree?,
                        right_paren: delimited_tree.close,
                    })
                } else {
                    None
                };

                Some(Variant { identifier, association: associated_value })
            },
            handler,
        )?;

        Some(EnumBody {
            left_brace: body.open,
            variant_list: body.list,
            right_brace: body.close,
        })
    }

    fn parse_module_signature(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<ModuleSignature> {
        let module_keyword =
            self.parse_keyword(KeywordKind::Module, handler)?;
        let identifier = self.parse_identifier(handler)?;

        Some(ModuleSignature { module_keyword, identifier })
    }

    /// Parses a [`ModulePath`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_module_path(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<ModulePath> {
        let first_identifier = self.parse_identifier(handler)?;
        let mut rest = Vec::new();

        while let Some(scope_separator) =
            self.try_parse(|this| this.parse_scope_separator(&Dummy))
        {
            let identifier = self.parse_identifier(handler)?;
            rest.push((scope_separator, identifier));
        }

        Some(ModulePath { first: first_identifier, rest })
    }

    fn parse_using(&mut self, handler: &dyn Handler<Error>) -> Option<Using> {
        let using_keyword = self.parse_keyword(KeywordKind::Using, handler)?;
        let module_path = self.parse_module_path(handler)?;
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Some(Using { using_keyword, module_path, semicolon })
    }

    /// Parses a [`ModuleContent`]
    pub fn parse_module_content(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> ModuleContent {
        let mut items = Vec::new();
        let mut usings = Vec::new();

        while !self.is_exhausted() {
            match (items.is_empty(), self.stop_at_significant()) {
                (true, Reading::Unit(Token::Keyword(using_keyword)))
                    if using_keyword.kind == KeywordKind::Using =>
                {
                    if let Some(using) = self.parse_using(handler) {
                        usings.push(using);
                        continue;
                    }
                }
                _ => {
                    if let Some(item) = self.parse_item(handler) {
                        items.push(item);
                        continue;
                    };
                }
            }

            // try to stop at the next access modifier or usings keyword
            self.stop_at(|token| {
                if let Reading::Unit(Token::Keyword(keyword)) = token {
                    (keyword.kind == KeywordKind::Public
                        || keyword.kind == KeywordKind::Private
                        || keyword.kind == KeywordKind::Internal)
                        || if items.is_empty() {
                            keyword.kind == KeywordKind::Using
                        } else {
                            false
                        }
                } else {
                    false
                }
            });
        }

        ModuleContent { usings, items }
    }

    fn parse_module_body(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<ModuleBody> {
        let delimited_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut items = Vec::new();
                let mut usings = Vec::new();

                while !parser.is_exhausted() {
                    match (items.is_empty(), parser.stop_at_significant()) {
                        (
                            true,
                            Reading::Unit(Token::Keyword(using_keyword)),
                        ) if using_keyword.kind == KeywordKind::Using => {
                            // eat using keyword
                            parser.forward();

                            let module_path =
                                parser.parse_module_path(handler)?;
                            let semicolon =
                                parser.parse_punctuation(';', true, handler)?;

                            usings.push(Using {
                                using_keyword,
                                module_path,
                                semicolon,
                            });
                            continue;
                        }
                        _ => {}
                    }

                    if let Some(item) = parser.parse_item(handler) {
                        items.push(item);
                        continue;
                    };

                    // try to stop at the next access modifier or usings keyword
                    parser.stop_at(|token| {
                        if let Reading::Unit(Token::Keyword(keyword)) = token {
                            (keyword.kind == KeywordKind::Public
                                || keyword.kind == KeywordKind::Private
                                || keyword.kind == KeywordKind::Internal)
                                || if items.is_empty() {
                                    keyword.kind == KeywordKind::Using
                                } else {
                                    false
                                }
                        } else {
                            false
                        }
                    });

                    // eat semicolon
                    parser.forward();
                }

                Some(ModuleContent { usings, items })
            },
            handler,
        )?;

        Some(ModuleBody {
            left_brace: delimited_tree.open,
            content: delimited_tree.tree?,
            right_brace: delimited_tree.close,
        })
    }

    fn parse_const_signature(
        &mut self,
        const_keyword: Option<Keyword>,
        handler: &dyn Handler<Error>,
    ) -> Option<ConstantSignature> {
        let const_keyword = if let Some(const_keyword) = const_keyword {
            const_keyword
        } else {
            self.parse_keyword(KeywordKind::Const, handler)?
        };
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;
        let colon = self.parse_punctuation(':', true, handler)?;
        let ty = self.parse_type(handler)?;

        Some(ConstantSignature {
            const_keyword,
            identifier,
            generic_parameters,
            colon,
            ty,
        })
    }

    fn parse_const_definition(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<ConstantDefinition> {
        let equals = self.parse_punctuation('=', true, handler)?;
        let expression = self.parse_expression(handler)?;
        let where_clause = self.try_parse_where_clause(handler)?;
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Some(ConstantDefinition { equals, expression, where_clause, semicolon })
    }

    #[allow(clippy::too_many_lines)]
    fn parse_item_with_access_modifier(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Item> {
        let access_modifier = self.parse_access_modifier(handler)?;

        match self.stop_at_significant() {
            // parse function
            Reading::Unit(Token::Keyword(k))
                if k.kind == KeywordKind::Function =>
            {
                let function_signature =
                    self.parse_function_signature(handler)?;
                let function_body = self.parse_function_body(handler)?;

                Some(Item::Function(Function {
                    access_modifier,
                    const_keyword: None,
                    signature: function_signature,
                    body: function_body,
                }))
            }

            Reading::Unit(Token::Keyword(k))
                if k.kind == KeywordKind::Const =>
            {
                // eat const keyword
                self.forward();

                match self.stop_at_significant() {
                    Reading::Unit(Token::Keyword(function_keyword))
                        if function_keyword.kind == KeywordKind::Function =>
                    {
                        let function_signature =
                            self.parse_function_signature(handler)?;
                        let function_body =
                            self.parse_function_body(handler)?;

                        Some(Item::Function(Function {
                            access_modifier,
                            const_keyword: Some(k),
                            signature: function_signature,
                            body: function_body,
                        }))
                    }

                    Reading::Unit(Token::Identifier(_)) => {
                        let const_signature =
                            self.parse_const_signature(Some(k), handler)?;
                        let const_definition =
                            self.parse_const_definition(handler)?;

                        Some(Item::Constant(Constant {
                            access_modifier,
                            signature: const_signature,
                            definition: const_definition,
                        }))
                    }

                    found => {
                        self.forward();
                        handler.receive(Error {
                            expected: SyntaxKind::Keyword(
                                KeywordKind::Function,
                            ),
                            alternatives: vec![SyntaxKind::Identifier],
                            found: found.into_token(),
                        });
                        None
                    }
                }
            }

            // parse module
            Reading::Unit(Token::Keyword(k))
                if k.kind == KeywordKind::Module =>
            {
                let signature = self.parse_module_signature(handler)?;

                let content = match self.stop_at_significant() {
                    Reading::Unit(Token::Punctuation(p))
                        if p.punctuation == ';' =>
                    {
                        // eat semi colon
                        self.forward();
                        ModuleKind::File(p)
                    }
                    _ => ModuleKind::Inline(self.parse_module_body(handler)?),
                };

                Some(Item::Module(Module {
                    access_modifier,
                    signature,
                    kind: content,
                }))
            }

            // parse trait
            Reading::Unit(Token::Keyword(k))
                if k.kind == KeywordKind::Trait =>
            {
                let trait_signature = self.parse_trait_signature(handler)?;
                let trait_body = self.parse_trait_body(handler)?;

                Some(Item::Trait(Trait {
                    access_modifier,
                    signature: trait_signature,
                    body: trait_body,
                }))
            }

            // parse struct
            Reading::Unit(Token::Keyword(k))
                if k.kind == KeywordKind::Struct =>
            {
                let struct_signature = self.parse_struct_signature(handler)?;
                let struct_body = self.parse_struct_body(handler)?;

                Some(Item::Struct(Struct {
                    access_modifier,
                    signature: struct_signature,
                    body: struct_body,
                }))
            }

            // parse type
            Reading::Unit(Token::Keyword(k)) if k.kind == KeywordKind::Type => {
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
            Reading::Unit(Token::Keyword(k)) if k.kind == KeywordKind::Enum => {
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
                handler.receive(Error {
                    expected: SyntaxKind::Item,
                    alternatives: Vec::new(),
                    found: found.into_token(),
                });
                None
            }
        }
    }

    /// Parses an [`Item`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_item(&mut self, handler: &dyn Handler<Error>) -> Option<Item> {
        match self.stop_at_significant() {
            // parses an item with an access modifier
            Reading::Unit(Token::Keyword(access_modifier))
                if matches!(
                    access_modifier.kind,
                    KeywordKind::Public
                        | KeywordKind::Private
                        | KeywordKind::Internal
                ) =>
            {
                self.parse_item_with_access_modifier(handler)
            }

            // parses an implements
            Reading::Unit(Token::Keyword(k))
                if k.kind == KeywordKind::Implements =>
            {
                self.parse_implements(handler).map(Item::Implementation)
            }

            found => {
                self.forward();
                handler.receive(Error {
                    expected: SyntaxKind::Item,
                    found: found.into_token(),
                    alternatives: Vec::new(),
                });
                None
            }
        }
    }
}

#[cfg(test)]
pub(super) mod tests;
