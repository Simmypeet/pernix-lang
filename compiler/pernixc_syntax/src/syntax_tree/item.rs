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
    expression::{Expression, Functional},
    pattern, r#type,
    statement::Statement,
    AccessModifier, ConnectedList, ConstantArgument, LifetimeArgument, QualifiedIdentifier,
    ScopeSeparator,
};
use crate::{
    error::{
        Error, GenericArgumentParameterListCannotBeEmpty, HigherRankedBoundParameterCannotBeEmpty,
        SyntaxKind, UnexpectedSyntax,
    },
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
    fn span(&self) -> Span { self.using_keyword.span.join(&self.semicolon.span).unwrap() }
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
        self.module_keyword
            .span
            .join(&self.identifier.span)
            .unwrap()
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

impl SourceElement for Module {
    fn span(&self) -> Span { self.access_modifier.span().join(&self.kind.span()).unwrap() }
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
    pub fn dissolve(self) -> (Vec<Using>, Vec<Item>) { (self.usings, self.items) }
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

impl SourceElement for ModuleBody {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
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
    fn span(&self) -> Span { self.apostrophe.span.join(&self.identifier.span).unwrap() }
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
    fn span(&self) -> Span { self.equals.span.join(&self.value.span()).unwrap() }
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
///     '=' Functional
///     ;
/// ```
type DefaultConstantParameter = DefaultGenericParameter<Functional>;

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

/// Syntax Synopsis:
/// ``` txt
/// ConstantParameter:
///     Identifier TypeAnnotation DefaultConstParameter?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantParameter {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    ty: r#type::Type,
    #[get = "pub"]
    default: Option<DefaultConstantParameter>,
}

impl SourceElement for ConstantParameter {
    fn span(&self) -> Span {
        self.identifier
            .span
            .join(
                &self
                    .default
                    .as_ref()
                    .map_or_else(|| self.ty.span(), SourceElement::span),
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
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
///     '<' GenericParameterList '>'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// LifetimeBound:
///     LifetimeBoundOperand ':' LifetimeArgument ('+' LifetimeArgument)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct LifetimeBound {
    #[get = "pub"]
    operand: LifetimeBoundOperand,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    arguments: BoundList<LifetimeArgument>,
}

impl SourceElement for LifetimeBound {
    fn span(&self) -> Span { self.operand.span().join(&self.arguments.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// LifetimeBoundOperand:
///     LifetimeParameter
///     | QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum LifetimeBoundOperand {
    LifetimeParameter(LifetimeParameter),
    QualifiedIdentifier(QualifiedIdentifier),
}

impl SourceElement for LifetimeBoundOperand {
    fn span(&self) -> Span {
        match self {
            Self::LifetimeParameter(lifetime_parameter) => lifetime_parameter.span(),
            Self::QualifiedIdentifier(qualified_identifier) => qualified_identifier.span(),
        }
    }
}

/// Similar to [`ConnectedList`] but specifically for list of constraints separated by plus sings
/// and has no trailing separator.
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
///     'const' 'type' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantTypeBound {
    #[get = "pub"]
    const_keyword: Keyword,
    #[get = "pub"]
    type_keyword: Keyword,
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for ConstantTypeBound {
    fn span(&self) -> Span {
        self.const_keyword
            .span
            .join(&self.qualified_identifier.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Predicate:
///     TraitBound
///     | LifetimeBound
///     | ConstantTypeBound
///     | TupleBound
///     | ForTupleBound
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum Predicate {
    TraitMember(TraitMemberBound),
    Trait(TraitBound),
    Lifetime(LifetimeBound),
    ConstantType(ConstantTypeBound),
}

impl SourceElement for Predicate {
    fn span(&self) -> Span {
        match self {
            Self::TraitMember(s) => s.span(),
            Self::Trait(s) => s.span(),
            Self::Lifetime(s) => s.span(),
            Self::ConstantType(s) => s.span(),
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
pub enum TraitMemberBoundArgument {
    Type(r#type::Type),
    Constant(ConstantArgument),
}

impl SourceElement for TraitMemberBoundArgument {
    fn span(&self) -> Span {
        match self {
            Self::Type(ty) => ty.span(),
            Self::Constant(c) => c.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitMemberBound:
///     QualifiedIdentifier '=' TraitMemberBoundArgument
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitMemberBound {
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    argument: TraitMemberBoundArgument,
}

impl SourceElement for TraitMemberBound {
    fn span(&self) -> Span {
        self.qualified_identifier
            .span()
            .join(&self.argument.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// HigherRankedLifetimeParameters:
///     'for' '<' LifetimeParameter (',' LifetimeParameter)* ','? '>'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct HigherRankedLifetimeParameters {
    #[get = "pub"]
    for_keyword: Keyword,
    #[get = "pub"]
    left_angle_bracket: Punctuation,
    #[get = "pub"]
    lifetime_parameter_list: ConnectedList<LifetimeParameter, Punctuation>,
    #[get = "pub"]
    right_angle_bracket: Punctuation,
}

impl SourceElement for HigherRankedLifetimeParameters {
    fn span(&self) -> Span {
        self.for_keyword
            .span
            .join(&self.right_angle_bracket.span)
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitBound:
///     HigherRankedLifetimeParameters? 'const'? QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitBound {
    #[get = "pub"]
    higher_ranked_lifetime_parameters: Option<HigherRankedLifetimeParameters>,
    #[get = "pub"]
    const_keyword: Option<Keyword>,
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for TraitBound {
    fn span(&self) -> Span {
        self.higher_ranked_lifetime_parameters.as_ref().map_or_else(
            || {
                self.const_keyword.as_ref().map_or_else(
                    || self.qualified_identifier.span(),
                    |const_keyword| {
                        const_keyword
                            .span
                            .join(&self.qualified_identifier.span())
                            .unwrap()
                    },
                )
            },
            |higher_ranked_lifetime_parameters| {
                higher_ranked_lifetime_parameters
                    .span()
                    .join(&self.qualified_identifier.span())
                    .unwrap()
            },
        )
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
///     'where' ':' PredicateList
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct WhereClause {
    #[get = "pub"]
    where_keyword: Keyword,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    constraint_list: PredicateList,
}

impl WhereClause {
    /// Dissolves the [`WhereClause`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, PredicateList) {
        (self.where_keyword, self.colon, self.constraint_list)
    }
}

impl SourceElement for WhereClause {
    fn span(&self) -> Span {
        self.where_keyword
            .span
            .join(&self.constraint_list.span())
            .unwrap()
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
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
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
    fn span(&self) -> Span { self.access_modifier.span().join(&self.body.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitFunction:
///     FunctionSignature ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// TraitType:
///     TypeSignature WhereClause? ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitType {
    #[get = "pub"]
    signature: TypeSignature,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for TraitType {
    fn span(&self) -> Span { self.signature.span().join(&self.semicolon.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitConstant:
///     ConstantSignature ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitConstant {
    #[get = "pub"]
    signature: ConstantSignature,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for TraitConstant {
    fn span(&self) -> Span { self.signature.span().join(&self.semicolon.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitMember:
///     TraitFunction
///     | TraitType
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
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
        self.irrefutable_pattern
            .span()
            .join(&self.ty.span())
            .unwrap()
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
    fn span(&self) -> Span { self.left_paren.span.join(&self.right_paren.span).unwrap() }
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
///     'function' 'const'? Identifier GenericParameters? Parameters ReturnType? WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
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

impl SourceElement for Type {
    fn span(&self) -> Span {
        self.access_modifier
            .span()
            .join(&self.semicolon.span)
            .unwrap()
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
    ) -> (
        Keyword,
        Identifier,
        Option<GenericParameters>,
        Option<WhereClause>,
    ) {
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
        self.struct_keyword
            .span
            .join(&self.identifier.span)
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// StructMemberList:
///     StructMember (',' StructMember)* ','?
///     ;
/// ```
pub type StructMemberList = ConnectedList<StructMember, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// StructDefinition:
///     '{' StructMember* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct StructBody {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    struct_member_list: Option<StructMemberList>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl StructBody {
    /// Dissolves the [`StructBody`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Option<StructMemberList>, Punctuation) {
        (self.left_brace, self.struct_member_list, self.right_brace)
    }
}

impl SourceElement for StructBody {
    fn span(&self) -> Span { self.left_brace.span.join(&self.right_brace.span).unwrap() }
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
    fn span(&self) -> Span { self.access_modifier.span().join(&self.body.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// StructField:
///     AccessModifier Identifier ':' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct StructField {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    ty: r#type::Type,
}

impl SourceElement for StructField {
    fn span(&self) -> Span { self.access_modifier.span().join(&self.ty.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// StructMember:
///     StructField
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
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

/// Syntax Synopsis:
/// ``` txt
/// ImplementsSignature:
///     'implements' GenericParameters? const? QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ImplementsSignature {
    #[get = "pub"]
    implements_keyword: Keyword,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
    #[get = "pub"]
    const_keyword: Option<Keyword>,
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for ImplementsSignature {
    fn span(&self) -> Span {
        self.implements_keyword
            .span
            .join(&self.qualified_identifier.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ImplementsFunction:
///     FunctionSignature FunctionBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// ImplementsType:
///     TypeSignature TypeDefinition ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// ImplementsConstant:
///     ConstantSignature ConstantDefinition
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ImplementsConstant {
    #[get = "pub"]
    signature: ConstantSignature,
    #[get = "pub"]
    definition: ConstantDefinition,
}

impl SourceElement for ImplementsConstant {
    fn span(&self) -> Span { self.signature.span().join(&self.definition.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// ImplementsMember:
///     ImplementsFunction
///     | ImplementsType
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum ImplementsMember {
    Type(ImplementsType),
    Function(ImplementsFunction),
    Constant(ImplementsConstant),
}

impl SourceElement for ImplementsMember {
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
/// NegativeImplements:
///     '=' 'delete' ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct NegativeImplements {
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    delete_keyword: Keyword,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for NegativeImplements {
    fn span(&self) -> Span { self.equals.span.join(&self.semicolon.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// ImplementsBody:
///     WhereClause? '{' ImplementsMember* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ImplementsBody {
    #[get = "pub"]
    where_clause: Option<WhereClause>,
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
    pub fn dissolve(
        self,
    ) -> (
        Option<WhereClause>,
        Punctuation,
        Vec<ImplementsMember>,
        Punctuation,
    ) {
        (
            self.where_clause,
            self.left_brace,
            self.members,
            self.right_brace,
        )
    }
}

impl SourceElement for ImplementsBody {
    fn span(&self) -> Span {
        let start = self
            .where_clause
            .as_ref()
            .map_or_else(|| self.left_brace.span.clone(), SourceElement::span);

        start.join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ImplementsKind:
///     NegativeImplements
///     | ImplementsBody
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ImplementsKind {
    Negative(NegativeImplements),
    Positive(ImplementsBody),
}

impl SourceElement for ImplementsKind {
    fn span(&self) -> Span {
        match self {
            Self::Negative(negative) => negative.span(),
            Self::Positive(positive) => positive.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Implements:
///     ImplementsSignature ImplementsKind
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Implements {
    #[get = "pub"]
    signature: ImplementsSignature,
    #[get = "pub"]
    kind: ImplementsKind,
}

impl Implements {
    /// Dissolves the [`Implements`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (ImplementsSignature, ImplementsKind) { (self.signature, self.kind) }
}

impl SourceElement for Implements {
    fn span(&self) -> Span { self.signature.span().join(&self.kind.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// EnumSignature:
///     'enum' Identifier GenericParameters?
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
}

impl SourceElement for EnumSignature {
    fn span(&self) -> Span { self.enum_keyword.span.join(&self.identifier.span).unwrap() }
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

impl SourceElement for VariantAssociation {
    fn span(&self) -> Span { self.left_paren.span.join(&self.right_paren.span).unwrap() }
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
    variant_association: Option<VariantAssociation>,
}

impl SourceElement for Variant {
    fn span(&self) -> Span {
        let end = self
            .variant_association
            .as_ref()
            .map_or_else(|| self.identifier.span.clone(), SourceElement::span);

        self.identifier.span.join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// EnumVariantList:
///     Identifier (',' Identifier)*
///     ;
/// ```
pub type EnumVariantList = ConnectedList<Variant, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// EnumBody:
///     '{' EnumVariantList? '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
    fn span(&self) -> Span { self.access_modifier.span().join(&self.body.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// ConstSignature:
///     'const' Identifier ':' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantSignature {
    #[get = "pub"]
    const_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    ty: r#type::Type,
}

impl SourceElement for ConstantSignature {
    fn span(&self) -> Span { self.const_keyword.span.join(&self.ty.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// ConstDefinition:
///     '=' Expression ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantDefinition {
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    expression: Expression,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for ConstantDefinition {
    fn span(&self) -> Span { self.equals.span.join(&self.semicolon.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` ebnf
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
        self.access_modifier
            .span()
            .join(&self.definition.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(clippy::large_enum_variant)]
pub enum Item {
    Trait(Trait),
    Function(Function),
    Type(Type),
    Struct(Struct),
    Implements(Implements),
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
            Self::Implements(i) => i.span(),
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
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Public => {
                Some(AccessModifier::Public(k))
            }
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Private => {
                Some(AccessModifier::Private(k))
            }
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Internal => {
                Some(AccessModifier::Internal(k))
            }
            found => {
                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::AccessModifier,
                    found: found.into_token(),
                }));
                None
            }
        }
    }

    fn parse_generic_parameters(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<GenericParameters> {
        let left_angle_bracket = self.parse_punctuation('<', true, handler)?;
        let (generic_parameter_list, right_angle_bracket) = self.parse_generic_list(
            |parser| match parser.stop_at_significant() {
                Reading::Atomic(Token::Punctuation(apostrophe))
                    if apostrophe.punctuation == '\'' =>
                {
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
                        Reading::Atomic(Token::Punctuation(colon)) if colon.punctuation == ':' => {
                            // eat colon
                            parser.forward();

                            let ty = parser.parse_type(handler)?;
                            let default = match parser.stop_at_significant() {
                                Reading::Atomic(Token::Punctuation(equals))
                                    if equals.punctuation == '=' =>
                                {
                                    // eat equals
                                    parser.forward();

                                    Some(DefaultConstantParameter {
                                        equals,
                                        value: parser.parse_primary_expression(handler)?,
                                    })
                                }
                                _ => None,
                            };

                            Some(GenericParameter::Constant(ConstantParameter {
                                identifier,
                                colon,
                                ty,
                                default,
                            }))
                        }

                        // type generic parameter
                        _ => {
                            let default = match parser.stop_at_significant() {
                                Reading::Atomic(Token::Punctuation(equals))
                                    if equals.punctuation == '=' =>
                                {
                                    // eat equals
                                    parser.forward();

                                    Some(DefaultTypeParameter {
                                        equals,
                                        value: parser.parse_type(handler)?,
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

    fn parse_lifetime_argument(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<LifetimeArgument> {
        let apostrophe = self.parse_punctuation('\'', true, handler)?;
        let identifier = self.parse_lifetime_argument_identifier(handler)?;

        Some(LifetimeArgument {
            apostrophe,
            identifier,
        })
    }

    fn parse_lifetime_bound_list(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<BoundList<LifetimeArgument>> {
        let first = self.parse_lifetime_argument(handler)?;
        let mut rest = Vec::new();

        while let Some(plus) = self.try_parse(|parser| parser.parse_punctuation('+', true, &Dummy))
        {
            rest.push((plus, self.parse_lifetime_argument(handler)?));
        }

        Some(BoundList { first, rest })
    }

    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn parse_constraint(&mut self, handler: &dyn Handler<Error>) -> Option<Predicate> {
        match self.stop_at_significant() {
            // parse for higher ranked bounds
            Reading::Atomic(Token::Keyword(for_keyword))
                if for_keyword.keyword == KeywordKind::For =>
            {
                // eat for keyword
                self.forward();

                let left_angle_bracket = self.parse_punctuation('<', true, handler)?;

                match self.stop_at_significant() {
                    // parse higher rankned lifetime bounds
                    Reading::Atomic(Token::Punctuation(apostrophe))
                        if apostrophe.punctuation == '\'' =>
                    {
                        let enclosed_tree = self.parse_generic_list(
                            |parser| {
                                let apostrophe = parser.parse_punctuation('\'', true, handler)?;
                                let identifier = parser.parse_identifier(handler)?;

                                Some(LifetimeParameter {
                                    apostrophe,
                                    identifier,
                                })
                            },
                            handler,
                        )?;

                        let const_keyword = match self.stop_at_significant() {
                            Reading::Atomic(Token::Keyword(const_keyword))
                                if const_keyword.keyword == KeywordKind::Const =>
                            {
                                // eat `const` keyword
                                self.forward();

                                Some(const_keyword)
                            }
                            _ => None,
                        };

                        let qualified_identifier =
                            self.parse_qualified_identifier(false, handler)?;

                        Some(Predicate::Trait(TraitBound {
                            higher_ranked_lifetime_parameters: Some(
                                HigherRankedLifetimeParameters {
                                    for_keyword,
                                    left_angle_bracket,
                                    lifetime_parameter_list: enclosed_tree.0.unwrap(),
                                    right_angle_bracket: enclosed_tree.1,
                                },
                            ),
                            const_keyword,
                            qualified_identifier,
                        }))
                    }

                    // empty higher ranked bounds
                    Reading::Atomic(Token::Punctuation(right_angle_bracket))
                        if right_angle_bracket.punctuation == '>' =>
                    {
                        // eat right angle bracket
                        self.forward();

                        // error
                        handler.receive(Error::HigherRankedBoundParameterCannotBeEmpty(
                            HigherRankedBoundParameterCannotBeEmpty {
                                span: for_keyword.span.join(&right_angle_bracket.span).unwrap(),
                            },
                        ));
                        None
                    }

                    found => {
                        // error
                        handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                            expected: SyntaxKind::HigherRankedBound,
                            found: found.into_token(),
                        }));
                        None
                    }
                }
            }

            // parse const keyword
            Reading::Atomic(Token::Keyword(const_keyword))
                if const_keyword.keyword == KeywordKind::Const =>
            {
                // eat const keyword
                self.forward();

                match self.stop_at_significant() {
                    Reading::Atomic(Token::Keyword(type_keyword))
                        if type_keyword.keyword == KeywordKind::Type =>
                    {
                        // eat type keyword
                        self.forward();

                        let qualified_identifier =
                            self.parse_qualified_identifier(false, handler)?;

                        Some(Predicate::ConstantType(ConstantTypeBound {
                            const_keyword,
                            type_keyword,
                            qualified_identifier,
                        }))
                    }
                    _ => {
                        let qualified_identifier =
                            self.parse_qualified_identifier(false, handler)?;

                        Some(Predicate::Trait(TraitBound {
                            higher_ranked_lifetime_parameters: None,
                            const_keyword: Some(const_keyword),
                            qualified_identifier,
                        }))
                    }
                }
            }

            // parses lifetime argument / bound
            Reading::Atomic(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                // eat apostrophe
                self.forward();

                let identifier = self.parse_identifier(handler)?;
                let colon = self.parse_punctuation(':', true, handler)?;

                let operand = LifetimeBoundOperand::LifetimeParameter(LifetimeParameter {
                    apostrophe,
                    identifier,
                });

                let arguments = self.parse_lifetime_bound_list(handler)?;

                Some(Predicate::Lifetime(LifetimeBound {
                    operand,
                    colon,
                    arguments,
                }))
            }
            _ => {
                let qualified_identifier = self.parse_qualified_identifier(false, handler)?;

                Some(match self.stop_at_significant() {
                    Reading::Atomic(Token::Punctuation(colon)) if colon.punctuation == ':' => {
                        // eat colon
                        self.forward();

                        let arguments = self.parse_lifetime_bound_list(handler)?;

                        Predicate::Lifetime(LifetimeBound {
                            operand: LifetimeBoundOperand::QualifiedIdentifier(
                                qualified_identifier,
                            ),
                            colon,
                            arguments,
                        })
                    }
                    Reading::Atomic(Token::Punctuation(equals)) if equals.punctuation == '=' => {
                        // eat equals
                        self.forward();

                        let argument = match self.stop_at_significant() {
                            Reading::IntoDelimited(p) if p.punctuation == '{' => {
                                let tree = self.step_into(
                                    Delimiter::Brace,
                                    |parser| parser.parse_expression(handler),
                                    handler,
                                )?;

                                TraitMemberBoundArgument::Constant(ConstantArgument {
                                    left_brace: tree.open,
                                    expression: Box::new(tree.tree?),
                                    right_brace: tree.close,
                                })
                            }
                            _ => TraitMemberBoundArgument::Type(self.parse_type(handler)?),
                        };

                        Predicate::TraitMember(TraitMemberBound {
                            qualified_identifier,
                            equals,
                            argument,
                        })
                    }
                    _ => Predicate::Trait(TraitBound {
                        higher_ranked_lifetime_parameters: None,
                        const_keyword: None,
                        qualified_identifier,
                    }),
                })
            }
        }
    }

    fn parse_where_clause(&mut self, handler: &dyn Handler<Error>) -> Option<WhereClause> {
        let where_keyword = self.parse_keyword(KeywordKind::Where, handler)?;
        let colon = self.parse_punctuation(':', true, handler)?;

        let first = self.parse_constraint(handler)?;
        let mut rest = Vec::new();
        let mut trailing_separator = None;

        while let Some(comma) = self.try_parse(|parser| parser.parse_punctuation(',', true, &Dummy))
        {
            if matches!(
                self.stop_at_significant(),
                Reading::Atomic(Token::Punctuation(p)) if p.punctuation == ';'
            ) || matches!(
                self.stop_at_significant(),
                Reading::IntoDelimited(p) if p.punctuation == '{'
            ) {
                trailing_separator = Some(comma);
                break;
            }

            let constraint = self.parse_constraint(handler)?;
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
        handler: &dyn Handler<Error>,
    ) -> Option<Option<WhereClause>> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(where_keyword))
                if where_keyword.keyword == KeywordKind::Where =>
            {
                self.parse_where_clause(handler).map(Some)
            }
            _ => Some(None),
        }
    }

    fn parse_function_body(&mut self, handler: &dyn Handler<Error>) -> Option<FunctionBody> {
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
                            Reading::Atomic(Token::Punctuation(p)) if p.punctuation == ';'
                        )
                    });

                    // go after the semicolon
                    if matches!(
                        parser.stop_at_significant(),
                        Reading::Atomic(Token::Punctuation(p)) if p.punctuation == ';'
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
        let function_keyword = self.parse_keyword(KeywordKind::Function, handler)?;
        let const_keyword = match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Const => {
                self.forward();
                Some(k)
            }
            _ => None,
        };

        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;

        let parameters = self.parse_enclosed_list(
            Delimiter::Parenthesis,
            ',',
            |parser| {
                let irrefutable_pattern = parser.parse_irrefutable_pattern(handler)?;
                let colon = parser.parse_punctuation(':', true, handler)?;
                let ty = parser.parse_type(handler)?;

                Some(Parameter {
                    irrefutable_pattern,
                    colon,
                    ty,
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
            Reading::Atomic(Token::Punctuation(p)) if p.punctuation == ':' => Some(ReturnType {
                colon: self.parse_punctuation(':', true, handler)?,
                ty: self.parse_type(handler)?,
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
        handler: &dyn Handler<Error>,
    ) -> Option<Option<GenericParameters>> {
        if matches!(
            self.stop_at_significant(),
            Reading::Atomic(Token::Punctuation(p)) if p.punctuation == '<'
        ) {
            Some(Some(self.parse_generic_parameters(handler)?))
        } else {
            Some(None)
        }
    }

    fn parse_implements_member(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<ImplementsMember> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(function_keyword))
                if function_keyword.keyword == KeywordKind::Function =>
            {
                let function_signature = self.parse_function_signature(handler)?;
                let function_body = self.parse_function_body(handler)?;

                Some(ImplementsMember::Function(ImplementsFunction {
                    signature: function_signature,
                    body: function_body,
                }))
            }

            Reading::Atomic(Token::Keyword(const_keyword))
                if const_keyword.keyword == KeywordKind::Const =>
            {
                let const_keyword = self.parse_keyword(KeywordKind::Const, handler)?;
                let identifier = self.parse_identifier(handler)?;
                let colon = self.parse_punctuation(':', true, handler)?;
                let ty = self.parse_type(handler)?;
                let equals = self.parse_punctuation('=', true, handler)?;
                let expression = self.parse_expression(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(ImplementsMember::Constant(ImplementsConstant {
                    signature: ConstantSignature {
                        const_keyword,
                        identifier,
                        colon,
                        ty,
                    },
                    definition: ConstantDefinition {
                        equals,
                        expression,
                        semicolon,
                    },
                }))
            }

            Reading::Atomic(Token::Keyword(type_keyword))
                if type_keyword.keyword == KeywordKind::Type =>
            {
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
                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    found: found.into_token(),
                    expected: SyntaxKind::ImplementsMember,
                }));
                None
            }
        }
    }

    fn parse_implements_body(&mut self, handler: &dyn Handler<Error>) -> Option<ImplementsBody> {
        let where_clause = self.try_parse_where_clause(handler)?;

        let delimited_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut implements_members = Vec::new();

                while !parser.is_exhausted() {
                    if let Some(member) = parser.parse_implements_member(handler) {
                        implements_members.push(member);
                        continue;
                    }

                    // try to stop at next function signature
                    parser.stop_at(|token| {
                        matches!(
                            token,
                            Reading::IntoDelimited(p) if p.punctuation == '{'
                        )
                    });
                    parser.forward();
                }

                Some(implements_members)
            },
            handler,
        )?;

        Some(ImplementsBody {
            where_clause,
            left_brace: delimited_tree.open,
            members: delimited_tree.tree?,
            right_brace: delimited_tree.close,
        })
    }

    fn parse_implements(&mut self, handler: &dyn Handler<Error>) -> Option<Implements> {
        let implements_keyword = self.parse_keyword(KeywordKind::Implements, handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;
        let const_keyword = match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(const_keyword))
                if const_keyword.keyword == KeywordKind::Const =>
            {
                // eat const keyword
                self.forward();

                Some(const_keyword)
            }
            _ => None,
        };
        let qualified_identifier = self.parse_qualified_identifier(false, handler)?;

        let kind = match self.stop_at_significant() {
            Reading::Atomic(Token::Punctuation(equals)) if equals.punctuation == '=' => {
                // eat equals
                self.forward();

                // eat delete keyword
                let delete_keyword = self.parse_keyword(KeywordKind::Delete, handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                ImplementsKind::Negative(NegativeImplements {
                    equals,
                    delete_keyword,
                    semicolon,
                })
            }
            _ => ImplementsKind::Positive(self.parse_implements_body(handler)?),
        };

        Some(Implements {
            signature: ImplementsSignature {
                implements_keyword,
                generic_parameters,
                const_keyword,
                qualified_identifier,
            },
            kind,
        })
    }

    fn parse_trait_signature(&mut self, handler: &dyn Handler<Error>) -> Option<TraitSignature> {
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

    fn parse_trait_member(&mut self, handler: &dyn Handler<Error>) -> Option<TraitMember> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(function_keyword))
                if function_keyword.keyword == KeywordKind::Function =>
            {
                let function_signature = self.parse_function_signature(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(TraitMember::Function(TraitFunction {
                    signature: function_signature,
                    semicolon,
                }))
            }

            Reading::Atomic(Token::Keyword(const_keyword))
                if const_keyword.keyword == KeywordKind::Const =>
            {
                let const_keyword = self.parse_keyword(KeywordKind::Const, handler)?;
                let identifier = self.parse_identifier(handler)?;
                let colon = self.parse_punctuation(':', true, handler)?;
                let ty = self.parse_type(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(TraitMember::Constant(TraitConstant {
                    signature: ConstantSignature {
                        const_keyword,
                        identifier,
                        colon,
                        ty,
                    },
                    semicolon,
                }))
            }

            Reading::Atomic(Token::Keyword(type_keyword))
                if type_keyword.keyword == KeywordKind::Type =>
            {
                let signature = self.parse_type_signature(handler)?;
                let where_clause = self.try_parse_where_clause(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(TraitMember::Type(TraitType {
                    signature,
                    where_clause,
                    semicolon,
                }))
            }

            found => {
                self.forward();
                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::TraitMember,
                    found: found.into_token(),
                }));
                None
            }
        }
    }

    fn parse_trait_body(&mut self, handler: &dyn Handler<Error>) -> Option<TraitBody> {
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
                            Reading::Atomic(Token::Punctuation(p)) if p.punctuation == ';'
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

    fn parse_type_signature(&mut self, handler: &dyn Handler<Error>) -> Option<TypeSignature> {
        let type_keyword = self.parse_keyword(KeywordKind::Type, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;

        Some(TypeSignature {
            type_keyword,
            identifier,
            generic_parameters,
        })
    }

    fn parse_type_definition(&mut self, handler: &dyn Handler<Error>) -> Option<TypeDefinition> {
        let equals = self.parse_punctuation('=', true, handler)?;
        let ty = self.parse_type(handler)?;
        let where_clause = self.try_parse_where_clause(handler)?;

        Some(TypeDefinition {
            equals,
            ty,
            where_clause,
        })
    }

    fn parse_struct_body(&mut self, handler: &dyn Handler<Error>) -> Option<StructBody> {
        let enclosed_list = self.parse_enclosed_list(
            Delimiter::Brace,
            ',',
            |parser| {
                let access_modifier = parser.parse_access_modifier(handler)?;
                let identifier = parser.parse_identifier(handler)?;
                let colon = parser.parse_punctuation(':', true, handler)?;
                let ty = parser.parse_type(handler)?;

                Some(StructMember::Field(StructField {
                    access_modifier,
                    identifier,
                    colon,
                    ty,
                }))
            },
            handler,
        )?;

        Some(StructBody {
            left_brace: enclosed_list.open,
            struct_member_list: enclosed_list.list,
            right_brace: enclosed_list.close,
        })
    }

    fn parse_struct_signature(&mut self, handler: &dyn Handler<Error>) -> Option<StructSignature> {
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

    fn parse_enum_signature(&mut self, handler: &dyn Handler<Error>) -> Option<EnumSignature> {
        let enum_keyword = self.parse_keyword(KeywordKind::Enum, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let generic_parameters = self.try_parse_generic_parameters(handler)?;

        Some(EnumSignature {
            enum_keyword,
            identifier,
            generic_parameters,
        })
    }

    fn parse_enum_body(&mut self, handler: &dyn Handler<Error>) -> Option<EnumBody> {
        let body = self.parse_enclosed_list(
            Delimiter::Brace,
            ',',
            |parser| {
                let identifier = parser.parse_identifier(handler)?;

                // parse associated enum value
                let associated_value = if matches!(
                    parser.stop_at_significant(),
                    Reading::IntoDelimited(p) if p.punctuation == '('
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

                Some(Variant {
                    identifier,
                    variant_association: associated_value,
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

    fn parse_module_signature(&mut self, handler: &dyn Handler<Error>) -> Option<ModuleSignature> {
        let module_keyword = self.parse_keyword(KeywordKind::Module, handler)?;
        let identifier = self.parse_identifier(handler)?;

        Some(ModuleSignature {
            module_keyword,
            identifier,
        })
    }

    /// Parses a [`ModulePath`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_module_path(&mut self, handler: &dyn Handler<Error>) -> Option<ModulePath> {
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

    fn parse_using(&mut self, handler: &dyn Handler<Error>) -> Option<Using> {
        let using_keyword = self.parse_keyword(KeywordKind::Using, handler)?;
        let module_path = self.parse_module_path(handler)?;
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Some(Using {
            using_keyword,
            module_path,
            semicolon,
        })
    }

    /// Parses a [`ModuleContent`]
    pub fn parse_module_content(&mut self, handler: &dyn Handler<Error>) -> ModuleContent {
        let mut items = Vec::new();
        let mut usings = Vec::new();

        while !self.is_exhausted() {
            match (items.is_empty(), self.stop_at_significant()) {
                (true, Reading::Atomic(Token::Keyword(using_keyword)))
                    if using_keyword.keyword == KeywordKind::Using =>
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
                if let Reading::Atomic(Token::Keyword(keyword)) = token {
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
        }

        ModuleContent { usings, items }
    }

    fn parse_module_body(&mut self, handler: &dyn Handler<Error>) -> Option<ModuleBody> {
        let delimited_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut items = Vec::new();
                let mut usings = Vec::new();

                while !parser.is_exhausted() {
                    match (items.is_empty(), parser.stop_at_significant()) {
                        (true, Reading::Atomic(Token::Keyword(using_keyword)))
                            if using_keyword.keyword == KeywordKind::Using =>
                        {
                            // eat using keyword
                            parser.forward();

                            let module_path = parser.parse_module_path(handler)?;
                            let semicolon = parser.parse_punctuation(';', true, handler)?;

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
                        if let Reading::Atomic(Token::Keyword(keyword)) = token {
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

    fn parse_const_signature(&mut self, handler: &dyn Handler<Error>) -> Option<ConstantSignature> {
        let const_keyword = self.parse_keyword(KeywordKind::Const, handler)?;
        let identifier = self.parse_identifier(handler)?;
        let colon = self.parse_punctuation(':', true, handler)?;
        let ty = self.parse_type(handler)?;

        Some(ConstantSignature {
            const_keyword,
            identifier,
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
        let semicolon = self.parse_punctuation(';', true, handler)?;

        Some(ConstantDefinition {
            equals,
            expression,
            semicolon,
        })
    }

    fn parse_item_with_access_modifier(&mut self, handler: &dyn Handler<Error>) -> Option<Item> {
        let access_modifier = self.parse_access_modifier(handler)?;

        match self.stop_at_significant() {
            // parse function
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Function => {
                let function_signature = self.parse_function_signature(handler)?;
                let function_body = self.parse_function_body(handler)?;

                Some(Item::Function(Function {
                    access_modifier,
                    signature: function_signature,
                    body: function_body,
                }))
            }

            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Const => {
                let signature = self.parse_const_signature(handler)?;
                let definition = self.parse_const_definition(handler)?;

                Some(Item::Constant(Constant {
                    access_modifier,
                    signature,
                    definition,
                }))
            }

            // parse module
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Module => {
                let signature = self.parse_module_signature(handler)?;

                let content = match self.stop_at_significant() {
                    Reading::Atomic(Token::Punctuation(p)) if p.punctuation == ';' => {
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
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Trait => {
                let trait_signature = self.parse_trait_signature(handler)?;
                let trait_body = self.parse_trait_body(handler)?;

                Some(Item::Trait(Trait {
                    access_modifier,
                    signature: trait_signature,
                    body: trait_body,
                }))
            }

            // parse struct
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Struct => {
                let struct_signature = self.parse_struct_signature(handler)?;
                let struct_body = self.parse_struct_body(handler)?;

                Some(Item::Struct(Struct {
                    access_modifier,
                    signature: struct_signature,
                    body: struct_body,
                }))
            }

            // parse type
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Type => {
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
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Enum => {
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
                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Item,
                    found: found.into_token(),
                }));
                None
            }
        }
    }

    /// Parses an [`Item`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_item(&mut self, handler: &dyn Handler<Error>) -> Option<Item> {
        match self.stop_at_significant() {
            // parses an item with an access modifier
            Reading::Atomic(Token::Keyword(access_modifier))
                if matches!(
                    access_modifier.keyword,
                    KeywordKind::Public | KeywordKind::Private | KeywordKind::Internal
                ) =>
            {
                self.parse_item_with_access_modifier(handler)
            }

            // parses an implements
            Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Implements => {
                self.parse_implements(handler).map(Item::Implements)
            }

            found => {
                self.forward();
                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Item,
                    found: found.into_token(),
                }));
                None
            }
        }
    }
}

#[cfg(test)]
pub(super) mod tests;
