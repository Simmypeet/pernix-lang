//! Contains all definition of item syntax trees.

#![allow(missing_docs)]

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{self, Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::Delimiter,
};

use super::{
    expression::Expression, pattern, predicate::Predicate, r#type,
    statement::Statements, AccessModifier, ConnectedList,
    EnclosedConnectedList, EnclosedTree, LifetimeParameter, ParseExt,
    QualifiedIdentifier, SimplePath, SyntaxTree,
};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, Parse},
        StateMachine,
    },
};

pub mod strategy;

/// Syntax Synopsis:
/// ``` txt
/// Alias:
///     'as' Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Alias {
    #[get = "pub"]
    as_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
}

impl SyntaxTree for Alias {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::As.to_owned(), expect::Identifier.to_owned())
            .map(|(as_keyword, identifier)| Self { as_keyword, identifier })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Alias {
    fn span(&self) -> Span {
        self.as_keyword.span.join(&self.identifier.span).unwrap()
    }
}

impl Alias {
    /// Dissolves the [`Alias`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Identifier) {
        (self.as_keyword, self.identifier)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Import:
///     Identifier Alias?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Import {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    alias: Option<Alias>,
}

impl SyntaxTree for Import {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (expect::Identifier.to_owned(), Alias::parse.or_none())
            .map(|(identifier, alias)| Self { identifier, alias })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Import {
    fn span(&self) -> Span {
        self.alias
            .as_ref()
            .map_or_else(|| self.identifier.span.clone(), SourceElement::span)
    }
}

impl Import {
    /// Dissolves the [`Import`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Identifier, Option<Alias>) {
        (self.identifier, self.alias)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// From:
///     'from' SimplePath
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct From {
    #[get = "pub"]
    from_keyword: Keyword,
    #[get = "pub"]
    simple_path: SimplePath,
}

impl SyntaxTree for From {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::From.to_owned(), SimplePath::parse)
            .map(|(from_keyword, simple_path)| Self {
                from_keyword,
                simple_path,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for From {
    fn span(&self) -> Span {
        self.from_keyword.span.join(&self.simple_path.span()).unwrap()
    }
}

impl From {
    /// Dissolves the [`From`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, SimplePath) {
        (self.from_keyword, self.simple_path)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// UsingOne:
///     SimplePath Alias?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UsingOne {
    #[get = "pub"]
    simple_path: SimplePath,
    #[get = "pub"]
    alias: Option<Alias>,
}

impl SyntaxTree for UsingOne {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (SimplePath::parse, Alias::parse.or_none())
            .map(|(simple_path, alias)| Self { simple_path, alias })
            .parse(state_machine, handler)
    }
}

impl SourceElement for UsingOne {
    fn span(&self) -> Span {
        self.alias
            .as_ref()
            .map_or_else(|| self.simple_path.span(), SourceElement::span)
    }
}

impl UsingOne {
    /// Dissolves the [`UsingOne`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (SimplePath, Option<Alias>) {
        (self.simple_path, self.alias)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// UsingFrom:
///     '{' (Import (',' Import)*)? '}' From
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UsingFrom {
    #[get = "pub"]
    imports: EnclosedConnectedList<Import, Punctuation>,
    #[get = "pub"]
    from: From,
}

impl SyntaxTree for UsingFrom {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Import::parse
                .enclosed_connected_list(','.to_owned(), Delimiter::Brace),
            From::parse,
        )
            .map(|(imports, from)| Self { imports, from })
            .parse(state_machine, handler)
    }
}

impl SourceElement for UsingFrom {
    fn span(&self) -> Span {
        self.from.span().join(&self.imports.span()).unwrap()
    }
}

impl UsingFrom {
    /// Dissolves the [`UsingFrom`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (EnclosedConnectedList<Import, Punctuation>, From) {
        (self.imports, self.from)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// UsingKind:
///     UsingOne
///     | UsingFrom
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum UsingKind {
    One(UsingOne),
    From(UsingFrom),
}

impl SyntaxTree for UsingKind {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (UsingOne::parse.map(Self::One), UsingFrom::parse.map(Self::From))
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for UsingKind {
    fn span(&self) -> Span {
        match self {
            Self::One(one) => one.span(),
            Self::From(from) => from.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Using:
///     'using' UsingKind ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Using {
    #[get = "pub"]
    using_keyword: Keyword,
    #[get = "pub"]
    kind: UsingKind,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SyntaxTree for Using {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Using.to_owned(), UsingKind::parse, ';'.to_owned())
            .map(|(using_keyword, kind, semicolon)| Self {
                using_keyword,
                kind,
                semicolon,
            })
            .parse(state_machine, handler)
    }
}

impl Using {
    /// Dissolves the [`Using`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, UsingKind, Punctuation) {
        (self.using_keyword, self.kind, self.semicolon)
    }
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

impl SyntaxTree for ModuleSignature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Module.to_owned(), expect::Identifier.to_owned())
            .map(|(module_keyword, identifier)| Self {
                module_keyword,
                identifier,
            })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for Module {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse, ModuleSignature::parse, ModuleKind::parse)
            .map(|(access_modifier, signature, kind)| Self {
                access_modifier,
                signature,
                kind,
            })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for ModuleContent {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Using::parse.keep_take(), Item::parse.keep_take_all())
            .map(|(usings, items)| Self { usings, items })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for ModuleKind {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (';'.to_owned().map(Self::File), ModuleBody::parse.map(Self::Inline))
            .branch()
            .parse(state_machine, handler)
    }
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
pub type ModuleBody = EnclosedTree<ModuleContent>;

impl SyntaxTree for ModuleBody {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ModuleContent::parse
            .enclosed_tree(Delimiter::Brace)
            .parse(state_machine, handler)
    }
}

/// Represents a syntax tree node for a default generic parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct DefaultGenericParameter<Value> {
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    value: Value,
}

impl<Value: SyntaxTree + 'static> SyntaxTree
    for DefaultGenericParameter<Value>
{
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('='.to_owned(), Value::parse)
            .map(|(equals, value)| Self { equals, value })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for TypeParameter {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (expect::Identifier.to_owned(), DefaultTypeParameter::parse.or_none())
            .map(|(identifier, default)| Self { identifier, default })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for ConstantParameter {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Const.to_owned(),
            expect::Identifier.to_owned(),
            ':'.to_owned(),
            r#type::Type::parse,
            DefaultConstantParameter::parse.or_none(),
        )
            .map(|(const_keyword, identifier, colon, r#type, default)| Self {
                const_keyword,
                identifier,
                colon,
                r#type,
                default,
            })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for GenericParameter {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            LifetimeParameter::parse.map(GenericParameter::Lifetime),
            TypeParameter::parse.map(GenericParameter::Type),
            ConstantParameter::parse.map(GenericParameter::Constant),
        )
            .branch()
            .parse(state_machine, handler)
    }
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
/// GenericParameters:
///     '[' GenericParameterList? ']'
///     ;
/// ```
pub type GenericParameters =
    EnclosedConnectedList<GenericParameter, Punctuation>;

impl SyntaxTree for GenericParameters {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        GenericParameter::parse
            .enclosed_connected_list(','.to_owned(), Delimiter::Bracket)
            .parse(state_machine, handler)
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

impl SyntaxTree for WhereClause {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Where.to_owned(),
            Predicate::parse.connected_list(','.to_owned()),
        )
            .map(|(where_keyword, predicate_list)| Self {
                where_keyword,
                predicate_list,
            })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for TraitSignature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Trait.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
            WhereClause::parse.or_none(),
        )
            .map(
                |(
                    trait_keyword,
                    identifier,
                    generic_parameters,
                    where_clause,
                )| {
                    Self {
                        trait_keyword,
                        identifier,
                        generic_parameters,
                        where_clause,
                    }
                },
            )
            .parse(state_machine, handler)
    }
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
pub type TraitBody = EnclosedTree<Vec<TraitMember>>;

impl SyntaxTree for TraitBody {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        TraitMember::parse
            .keep_take_all()
            .enclosed_tree(Delimiter::Brace)
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
/// ```txt
/// MarkerSignature:
///     'marker' MarkerKind Identifier GenericParameters? WhereClause?
///     '
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct MarkerSignature {
    #[get = "pub"]
    marker_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    generic_parameters: Option<GenericParameters>,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
}

impl SyntaxTree for MarkerSignature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Marker.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
            WhereClause::parse.or_none(),
        )
            .map(
                |(
                    marker_keyword,
                    identifier,
                    generic_parameters,
                    where_clause,
                )| {
                    Self {
                        marker_keyword,
                        identifier,
                        generic_parameters,
                        where_clause,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

impl MarkerSignature {
    /// Dissolves the [`MarkerSignature`] into a tuple of its fields.
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn dissolve(
        self,
    ) -> (Keyword, Identifier, Option<GenericParameters>, Option<WhereClause>)
    {
        (
            self.marker_keyword,
            self.identifier,
            self.generic_parameters,
            self.where_clause,
        )
    }
}

impl SourceElement for MarkerSignature {
    fn span(&self) -> Span {
        let start = &self.marker_keyword.span;
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
/// ```txt
/// Marker:
///     AccessModifier MarkerSignature ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Marker {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: MarkerSignature,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SyntaxTree for Marker {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse, MarkerSignature::parse, ';'.to_owned())
            .map(|(access_modifier, signature, semicolon)| Self {
                access_modifier,
                signature,
                semicolon,
            })
            .parse(state_machine, handler)
    }
}

impl Marker {
    /// Dissolves the [`Marker`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (AccessModifier, MarkerSignature, Punctuation) {
        (self.access_modifier, self.signature, self.semicolon)
    }
}

impl SourceElement for Marker {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.semicolon.span).unwrap()
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

impl SyntaxTree for Trait {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse, TraitSignature::parse, TraitBody::parse)
            .map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for TraitFunction {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse, FunctionSignature::parse, ';'.to_owned())
            .map(|(access_modifier, signature, semicolon)| Self {
                access_modifier,
                signature,
                semicolon,
            })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for TraitType {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            TypeSignature::parse,
            WhereClause::parse.or_none(),
            ';'.to_owned(),
        )
            .map(|(access_modifier, signature, where_clause, semicolon)| Self {
                access_modifier,
                signature,
                where_clause,
                semicolon,
            })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for TraitConstant {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            ConstantSignature::parse,
            WhereClause::parse.or_none(),
            ';'.to_owned(),
        )
            .map(|(access_modifier, signature, where_clause, semicolon)| Self {
                access_modifier,
                signature,
                where_clause,
                semicolon,
            })
            .parse(state_machine, handler)
    }
}

impl TraitConstant {
    /// Dissolves the [`TraitConstant`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (AccessModifier, ConstantSignature, Option<WhereClause>, Punctuation)
    {
        (
            self.access_modifier,
            self.signature,
            self.where_clause,
            self.semicolon,
        )
    }
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

impl SyntaxTree for TraitMember {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            TraitFunction::parse.map(TraitMember::Function),
            TraitType::parse.map(TraitMember::Type),
            TraitConstant::parse.map(TraitMember::Constant),
        )
            .branch()
            .parse(state_machine, handler)
    }
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
    r#type: r#type::Type,
}

impl SyntaxTree for Parameter {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (pattern::Irrefutable::parse, ':'.to_owned(), r#type::Type::parse)
            .map(|(irrefutable_pattern, colon, r#type)| Self {
                irrefutable_pattern,
                colon,
                r#type,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Parameter {
    fn span(&self) -> Span {
        self.irrefutable_pattern.span().join(&self.r#type.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Parameters:
///     '(' ParameterList? ')'
///     ;
/// ```
pub type Parameters = EnclosedConnectedList<Parameter, Punctuation>;

impl SyntaxTree for Parameters {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Parameter::parse
            .enclosed_connected_list(','.to_owned(), Delimiter::Parenthesis)
            .parse(state_machine, handler)
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
    r#type: r#type::Type,
}

impl SyntaxTree for ReturnType {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (':'.to_owned(), r#type::Type::parse)
            .map(|(colon, r#type)| Self { colon, r#type })
            .parse(state_machine, handler)
    }
}

impl SourceElement for ReturnType {
    fn span(&self) -> Span {
        self.colon.span.join(&self.r#type.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// FunctionSignature:
///     'function' Identifier GenericParameters? Parameters ReturnType?
///     WhereClause?
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

impl SyntaxTree for FunctionSignature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Function.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
            Parameters::parse,
            ReturnType::parse.or_none(),
            WhereClause::parse.or_none(),
        )
            .map(
                |(
                    function_keyword,
                    identifier,
                    generic_parameters,
                    parameters,
                    return_type,
                    where_clause,
                )| {
                    Self {
                        function_keyword,
                        identifier,
                        generic_parameters,
                        parameters,
                        return_type,
                        where_clause,
                    }
                },
            )
            .parse(state_machine, handler)
    }
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
    statements: Statements,
}

impl SyntaxTree for Function {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            KeywordKind::Const.to_owned().or_none(),
            FunctionSignature::parse,
            Statements::parse,
        )
            .map(|(access_modifier, const_keyword, signature, statements)| {
                Self { access_modifier, const_keyword, signature, statements }
            })
            .parse(state_machine, handler)
    }
}

impl Function {
    /// Dissolves the [`Function`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (AccessModifier, Option<Keyword>, FunctionSignature, Statements) {
        (
            self.access_modifier,
            self.const_keyword,
            self.signature,
            self.statements,
        )
    }
}

impl SourceElement for Function {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.statements.span()).unwrap()
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

impl SyntaxTree for TypeSignature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Type.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
        )
            .map(|(type_keyword, identifier, generic_parameters)| Self {
                type_keyword,
                identifier,
                generic_parameters,
            })
            .parse(state_machine, handler)
    }
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
    r#type: r#type::Type,
    #[get = "pub"]
    where_clause: Option<WhereClause>,
}

impl SyntaxTree for TypeDefinition {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('='.to_owned(), r#type::Type::parse, WhereClause::parse.or_none())
            .map(|(equals, r#type, where_clause)| Self {
                equals,
                r#type,
                where_clause,
            })
            .parse(state_machine, handler)
    }
}

impl TypeDefinition {
    /// Dissolves the [`TypeDefinition`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, r#type::Type, Option<WhereClause>) {
        (self.equals, self.r#type, self.where_clause)
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
                    .map_or_else(|| self.r#type.span(), SourceElement::span),
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

impl SyntaxTree for Type {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            TypeSignature::parse,
            TypeDefinition::parse,
            ';'.to_owned(),
        )
            .map(|(access_modifier, signature, definition, semicolon)| Self {
                access_modifier,
                signature,
                definition,
                semicolon,
            })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for StructSignature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Struct.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
            WhereClause::parse.or_none(),
        )
            .map(
                |(
                    struct_keyword,
                    identifier,
                    generic_parameters,
                    where_clause,
                )| {
                    Self {
                        struct_keyword,
                        identifier,
                        generic_parameters,
                        where_clause,
                    }
                },
            )
            .parse(state_machine, handler)
    }
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
/// StructDefinition:
///     '{' FieldList* '}'
///     ;
/// ```
pub type StructBody = EnclosedConnectedList<Field, Punctuation>;

impl SyntaxTree for StructBody {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Field::parse
            .enclosed_connected_list(','.to_owned(), Delimiter::Brace)
            .parse(state_machine, handler)
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

impl SyntaxTree for Struct {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse, StructSignature::parse, StructBody::parse)
            .map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for Field {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            expect::Identifier.to_owned(),
            ':'.to_owned(),
            r#type::Type::parse,
        )
            .map(|(access_modifier, identifier, colon, r#type)| Self {
                access_modifier,
                identifier,
                colon,
                r#type,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Field {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.r#type.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ImplementationSignature:
///     'final'?
///     'implements'
///     GenericParameters?
///     const?
///     QualifiedIdentifier
///     WhereClause?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ImplementationSignature {
    #[get = "pub"]
    final_keyword: Option<Keyword>,
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

impl SyntaxTree for ImplementationSignature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Final.to_owned().or_none(),
            KeywordKind::Implements.to_owned(),
            GenericParameters::parse.or_none(),
            KeywordKind::Const.to_owned().or_none(),
            QualifiedIdentifier::parse,
            WhereClause::parse.or_none(),
        )
            .map(
                |(
                    final_keyword,
                    implements_keyword,
                    generic_parameters,
                    const_keyword,
                    qualified_identifier,
                    where_clause,
                )| {
                    Self {
                        final_keyword,
                        implements_keyword,
                        generic_parameters,
                        const_keyword,
                        qualified_identifier,
                        where_clause,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

impl ImplementationSignature {
    /// Dissolves the [`ImplementationSignature`] into a tuple of its fields.
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn dissolve(
        self,
    ) -> (
        Option<Keyword>,
        Keyword,
        Option<GenericParameters>,
        Option<Keyword>,
        QualifiedIdentifier,
        Option<WhereClause>,
    ) {
        (
            self.final_keyword,
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
        let start = self.final_keyword.as_ref().map_or_else(
            || self.implements_keyword.span.clone(),
            SourceElement::span,
        );

        let end = self.where_clause.as_ref().map_or_else(
            || self.qualified_identifier.span(),
            SourceElement::span,
        );

        start.join(&end).unwrap()
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

impl SyntaxTree for ImplementationMember {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Type::parse.map(ImplementationMember::Type),
            Function::parse.map(ImplementationMember::Function),
            Constant::parse.map(ImplementationMember::Constant),
        )
            .branch()
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for NegativeImplementation {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Delete.to_owned(), ';'.to_owned())
            .map(|(delete_keyword, semicolon)| Self {
                delete_keyword,
                semicolon,
            })
            .parse(state_machine, handler)
    }
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
pub type ImplementationBody = EnclosedTree<Vec<ImplementationMember>>;

impl SyntaxTree for ImplementationBody {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ImplementationMember::parse
            .keep_take_all()
            .enclosed_tree(Delimiter::Brace)
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ImplementationKind:
///     NegativeImplementation
///     | ImplementationBody
///     | ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ImplementationKind {
    Negative(NegativeImplementation),
    Positive(ImplementationBody),
    Empty(Punctuation),
}

impl SyntaxTree for ImplementationKind {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            NegativeImplementation::parse.map(ImplementationKind::Negative),
            ImplementationBody::parse.map(ImplementationKind::Positive),
            ';'.to_owned().map(ImplementationKind::Empty),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for ImplementationKind {
    fn span(&self) -> Span {
        match self {
            Self::Negative(negative) => negative.span(),
            Self::Positive(positive) => positive.span(),
            Self::Empty(empty) => empty.span.clone(),
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

impl SyntaxTree for Implementation {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (ImplementationSignature::parse, ImplementationKind::parse)
            .map(|(signature, kind)| Self { signature, kind })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for EnumSignature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Enum.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
            WhereClause::parse.or_none(),
        )
            .map(
                |(
                    enum_keyword,
                    identifier,
                    generic_parameters,
                    where_clause,
                )| Self {
                    enum_keyword,
                    identifier,
                    generic_parameters,
                    where_clause,
                },
            )
            .parse(state_machine, handler)
    }
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
///     '(' Type ')'
///     ;
/// ```
pub type VariantAssociation = EnclosedTree<r#type::Type>;

impl SyntaxTree for VariantAssociation {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        r#type::Type::parse
            .enclosed_tree(Delimiter::Parenthesis)
            .parse(state_machine, handler)
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

impl SyntaxTree for Variant {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (expect::Identifier.to_owned(), VariantAssociation::parse.or_none())
            .map(|(identifier, association)| Self { identifier, association })
            .parse(state_machine, handler)
    }
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
/// EnumBody:
///     '{' (Variant (',' Variant)+ ','?)? '}'
///     ;
/// ```
pub type EnumBody = EnclosedConnectedList<Variant, Punctuation>;

impl SyntaxTree for EnumBody {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Variant::parse
            .enclosed_connected_list(','.to_owned(), Delimiter::Brace)
            .parse(state_machine, handler)
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

impl SyntaxTree for Enum {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse, EnumSignature::parse, EnumBody::parse)
            .map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .parse(state_machine, handler)
    }
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
    r#type: r#type::Type,
}

impl ConstantSignature {
    /// Dissolves the [`ConstantSignature`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Keyword,
        Identifier,
        Option<GenericParameters>,
        Punctuation,
        r#type::Type,
    ) {
        (
            self.const_keyword,
            self.identifier,
            self.generic_parameters,
            self.colon,
            self.r#type,
        )
    }
}

impl SyntaxTree for ConstantSignature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Const.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
            ':'.to_owned(),
            r#type::Type::parse,
        )
            .map(
                |(
                    const_keyword,
                    identifier,
                    generic_parameters,
                    colon,
                    r#type,
                )| {
                    Self {
                        const_keyword,
                        identifier,
                        generic_parameters,
                        colon,
                        r#type,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for ConstantSignature {
    fn span(&self) -> Span {
        self.const_keyword.span.join(&self.r#type.span()).unwrap()
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

impl ConstantDefinition {
    /// Dissolves the [`ConstantDefinition`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Punctuation, Expression, Option<WhereClause>, Punctuation) {
        (self.equals, self.expression, self.where_clause, self.semicolon)
    }
}

impl SyntaxTree for ConstantDefinition {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            '='.to_owned(),
            Expression::parse,
            WhereClause::parse.or_none(),
            ';'.to_owned(),
        )
            .map(|(equals, expression, where_clause, semicolon)| Self {
                equals,
                expression,
                where_clause,
                semicolon,
            })
            .parse(state_machine, handler)
    }
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

impl SyntaxTree for Constant {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            ConstantSignature::parse,
            ConstantDefinition::parse,
        )
            .map(|(access_modifier, signature, definition)| Self {
                access_modifier,
                signature,
                definition,
            })
            .parse(state_machine, handler)
    }
}

impl Constant {
    /// Dissolves the [`Constant`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (AccessModifier, ConstantSignature, ConstantDefinition) {
        (self.access_modifier, self.signature, self.definition)
    }
}

impl SourceElement for Constant {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.definition.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ```txt
/// ExternFunction:
///     AccessModifier FunctionSignature ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ExternFunction {
    #[get = "pub"]
    access_modifier: AccessModifier,
    #[get = "pub"]
    signature: FunctionSignature,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SyntaxTree for ExternFunction {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse, FunctionSignature::parse, ';'.to_owned())
            .map(|(access_modifier, signature, semicolon)| Self {
                access_modifier,
                signature,
                semicolon,
            })
            .parse(state_machine, handler)
    }
}

impl ExternFunction {
    /// Dissolves the [`ExternFunction`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (AccessModifier, FunctionSignature, Punctuation) {
        (self.access_modifier, self.signature, self.semicolon)
    }
}

impl SourceElement for ExternFunction {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.semicolon.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ```txt
/// ExternBody:
///     '{' ExternFunction* '}'
///     ;
/// ```
pub type ExternBody = EnclosedTree<Vec<ExternFunction>>;

impl SyntaxTree for ExternBody {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ExternFunction::parse
            .keep_take_all()
            .enclosed_tree(Delimiter::Brace)
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
/// ```txt
/// Extern:
///     'extern' String ExternBody
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Extern {
    #[get = "pub"]
    extern_keyword: Keyword,
    #[get = "pub"]
    convention: token::String,
    #[get = "pub"]
    extern_body: ExternBody,
}

impl SyntaxTree for Extern {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Extern.to_owned(),
            expect::String.to_owned(),
            ExternBody::parse,
        )
            .map(|(extern_keyword, convention, extern_body)| Self {
                extern_keyword,
                convention,
                extern_body,
            })
            .parse(state_machine, handler)
    }
}

impl Extern {
    /// Dissolves the [`Extern`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Keyword, token::String, EnclosedTree<Vec<ExternFunction>>) {
        (self.extern_keyword, self.convention, self.extern_body)
    }
}

impl SourceElement for Extern {
    fn span(&self) -> Span {
        self.extern_keyword.span.join(&self.extern_body.span()).unwrap()
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
///     | Extern
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
    Marker(Marker),
    Extern(Extern),
}

impl SyntaxTree for Item {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Trait::parse.map(Item::Trait),
            Function::parse.map(Item::Function),
            Type::parse.map(Item::Type),
            Struct::parse.map(Item::Struct),
            Implementation::parse.map(Item::Implementation),
            Enum::parse.map(Item::Enum),
            Module::parse.map(Item::Module),
            Constant::parse.map(Item::Constant),
            Marker::parse.map(Item::Marker),
            Extern::parse.map(Item::Extern),
        )
            .branch()
            .parse(state_machine, handler)
    }
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
            Self::Marker(m) => m.span(),
            Self::Extern(e) => e.span(),
        }
    }
}

#[cfg(test)]
mod test;
