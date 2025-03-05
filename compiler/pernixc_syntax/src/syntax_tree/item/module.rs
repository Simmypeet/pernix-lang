use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, Span};

use super::{
    constant::Constant, function::Function, implements::Implements,
    marker::Marker, r#enum::Enum, r#extern::Extern, r#struct::Struct,
    r#trait::Trait, r#type::Type,
};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, Parse, Passable},
        StateMachine,
    },
    syntax_tree::{AccessModifier, ConnectedList, ParseExt, SyntaxTree},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub marker_keyword: Keyword,
    pub identifier: Identifier,
}

impl SourceElement for Signature {
    fn span(&self) -> Span {
        self.marker_keyword.span().join(&self.identifier.span())
    }
}

impl SyntaxTree for Signature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Marker.to_owned(), expect::Identifier.to_owned())
            .map(|(marker_keyword, identifier)| Self {
                marker_keyword,
                identifier,
            })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct From {
    pub from: Keyword,
    pub identifier: Identifier,
}

impl SourceElement for From {
    fn span(&self) -> Span { self.from.span().join(&self.identifier.span()) }
}

impl SyntaxTree for From {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::From.to_owned(), expect::Identifier.to_owned())
            .map(|(from, identifier)| Self { from, identifier })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Alias {
    pub as_keyword: Keyword,
    pub identifier: Identifier,
}

impl SourceElement for Alias {
    fn span(&self) -> Span {
        self.as_keyword.span().join(&self.identifier.span())
    }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImportItem {
    pub identifier: Identifier,
    pub alias: Option<Alias>,
}

impl SourceElement for ImportItem {
    fn span(&self) -> Span {
        let identifier = self.identifier.span();

        identifier.join(
            &self
                .alias
                .as_ref()
                .map_or_else(|| identifier.clone(), SourceElement::span),
        )
    }
}

impl SyntaxTree for ImportItem {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (expect::Identifier.to_owned(), Alias::parse.or_none())
            .map(|(identifier, alias)| Self { identifier, alias })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Import {
    pub from: Option<From>,
    pub import_keyword: Keyword,
    pub parenthesis_delimited: Option<(Punctuation, Punctuation)>,
    pub items: Option<ConnectedList<ImportItem, Punctuation>>,
}

impl SourceElement for Import {
    fn span(&self) -> Span {
        let begin = self
            .from
            .as_ref()
            .map_or_else(|| self.import_keyword.span(), SourceElement::span);

        let end = self.parenthesis_delimited.as_ref().map_or_else(
            || {
                self.items.as_ref().map_or_else(
                    || self.import_keyword.span.clone(),
                    SourceElement::span,
                )
            },
            |x| x.1.span(),
        );

        begin.join(&end)
    }
}

impl SyntaxTree for Import {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            From::parse.or_none(),
            KeywordKind::Import.to_owned(),
            (
                ImportItem::parse
                    .enclosed_connected_list(
                        ','.to_owned(),
                        DelimiterKind::Parenthesis,
                    )
                    .map(|x| (Some((x.open, x.close)), x.connected_list)),
                ImportItem::parse
                    .connected_list(','.to_owned())
                    .map(|x| (None, Some(x))),
            )
                .branch(),
        )
            .map(|(from, import_keyword, (parenthesis_delimited, items))| {
                Self { from, import_keyword, parenthesis_delimited, items }
            })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body {
    pub members: Vec<Passable<Member>>,
}

impl SyntaxTree for Body {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Member::parse
            .indentation_item()
            .keep_take_all()
            .map(|members| Self { members })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InlineBody {
    pub colon: Punctuation,
    pub body: Body,
}

impl SourceElement for InlineBody {
    fn span(&self) -> Span {
        let begin = self.colon.span();

        begin.join(
            &self
                .body
                .members
                .last()
                .map_or_else(|| self.colon.span(), SourceElement::span),
        )
    }
}

impl SyntaxTree for InlineBody {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Body::parse
            .step_into_indentation()
            .map(|(colon, body)| Self { colon: colon.clone(), body })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Module {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub inline_body: Option<InlineBody>,
}

impl SourceElement for Module {
    fn span(&self) -> Span {
        let begin = self.access_modifier.span();

        begin.join(
            &self
                .inline_body
                .as_ref()
                .map_or_else(|| self.signature.span(), SourceElement::span),
        )
    }
}

impl SyntaxTree for Module {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse, Signature::parse, InlineBody::parse.or_none())
            .map(|(access_modifier, signature, inline_body)| Self {
                access_modifier,
                signature,
                inline_body,
            })
            .parse(state_machine, handler)
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
pub enum Member {
    Module(Module),
    Import(Import),
    Trait(Trait),
    Function(Function),
    Type(Type),
    Struct(Struct),
    Implements(Implements),
    Enum(Enum),
    Constant(Constant),
    Marker(Marker),
    Extern(Extern),
}

impl SyntaxTree for Member {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Module::parse.map(Self::Module),
            Import::parse.map(Self::Import),
            Trait::parse.map(Self::Trait),
            Function::parse.map(Self::Function),
            Type::parse.map(Self::Type),
            Struct::parse.map(Self::Struct),
            Implements::parse.map(Self::Implements),
            Enum::parse.map(Self::Enum),
            Constant::parse.map(Self::Constant),
            Marker::parse.map(Self::Marker),
            Extern::parse.map(Self::Extern),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Member {
    fn span(&self) -> Span {
        match self {
            Self::Module(module) => module.span(),
            Self::Import(import) => import.span(),
            Self::Trait(trait_) => trait_.span(),
            Self::Function(function) => function.span(),
            Self::Type(type_) => type_.span(),
            Self::Struct(struct_) => struct_.span(),
            Self::Implements(implements) => implements.span(),
            Self::Enum(r#enum) => r#enum.span(),
            Self::Constant(constant) => constant.span(),
            Self::Marker(marker) => marker.span(),
            Self::Extern(extern_) => extern_.span(),
        }
    }
}
