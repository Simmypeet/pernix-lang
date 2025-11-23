use enum_as_inner::EnumAsInner;
use pernixc_lexical::kind::arbitrary::Identifier;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy as _},
    prop_oneof,
};

use crate::{
    arbitrary::{
        AccessModifier, IndentDisplay, IntoSeparated, Passable, SimplePath,
        write_indent_line_for_indent_display,
    },
    item::{
        constant::arbitrary::Constant, effect::arbitrary::Effect,
        r#enum::arbitrary::Enum, r#extern::arbitrary::Extern,
        function::arbitrary::Function, implements::arbitrary::Implements,
        marker::arbitrary::Marker, r#struct::arbitrary::Struct,
        r#trait::arbitrary::Trait, r#type::arbitrary::Type,
    },
    reference,
};

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(" as {identifier}")]
    pub struct Alias for super::Alias {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
    }
}

impl Arbitrary for Alias {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{simple_path}{}",
        alias.as_ref().map(|x| format!(" {x}")).unwrap_or_default()
    )]
    pub struct ImportItem for super::ImportItem {
        pub simple_path (SimplePath),
        pub alias (Option<Alias>),
    }
}

impl Arbitrary for ImportItem {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (SimplePath::arbitrary(), proptest::option::of(Alias::arbitrary()))
            .prop_map(|(simple_path, alias)| Self { simple_path, alias })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{}",
        items.iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    )]
    pub struct ImportItems for super::ImportItems {
        pub items (Vec<ImportItem>),
    }
}

impl Arbitrary for ImportItems {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(ImportItem::arbitrary(), 1..=6)
            .prop_map(|items| Self { items })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("({})", self.items.into_separated(", "))]
    pub struct ParenthesizedImportItems for super::ParenthesizedImportItems {
        pub items (Vec<ImportItem>),
    }
}

impl Arbitrary for ParenthesizedImportItems {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(ImportItem::arbitrary(), 0..=3)
            .prop_map(|items| Self { items })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    pub enum ImportItemsKind for super::ImportItemsKind {
        Regular(ImportItems),
        Parenthesized(ParenthesizedImportItems),
    }
}

impl Arbitrary for ImportItemsKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            6 => ImportItems::arbitrary().prop_map(ImportItemsKind::Regular),
            1 => ParenthesizedImportItems::arbitrary()
                .prop_map(ImportItemsKind::Parenthesized),
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("from {simple_path} ")]
    pub struct From for super::From {
        pub simple_path (SimplePath),
    }
}

impl Arbitrary for From {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        SimplePath::arbitrary()
            .prop_map(|simple_path| Self { simple_path })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{}import {items}",
        from.as_ref()
            .map(ToString::to_string)
            .unwrap_or_default()
    )]
    pub struct Import for super::Import {
        pub from (Option<From>),
        pub items (ImportItemsKind),
    }
}

impl Arbitrary for Import {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(SimplePath::arbitrary()),
            ImportItemsKind::arbitrary(),
        )
            .prop_map(|(from, items)| Self {
                from: from.map(|simple_path| From { simple_path }),
                items,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("module {identifier}")]
    pub struct Signature for super::Signature {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier)
    }
}

impl Arbitrary for Signature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Content for super::Content {
        pub members (Vec<Passable<Member>>),
    }
}

impl Arbitrary for Content {
    type Parameters = Option<BoxedStrategy<Module>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(
            prop_oneof![
                1 => Just(Passable::Pass),
                10 => Member::arbitrary_with(arg).prop_map(Passable::Line)
            ],
            1..=10,
        )
        .prop_map(|members| Self { members })
        .boxed()
    }
}

impl IndentDisplay for Content {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        for member in &self.members {
            write_indent_line_for_indent_display(f, member, indent)?;
        }
        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct InlineBody for super::InlineBody {
        pub content (Content),
    }
}

impl Arbitrary for InlineBody {
    type Parameters = Option<BoxedStrategy<Module>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        Content::arbitrary_with(arg)
            .prop_map(|content| Self { content })
            .boxed()
    }
}

impl IndentDisplay for InlineBody {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        writeln!(f, ":")?;
        self.content.indent_fmt(f, indent + 1)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Module for super::Module {
        pub access_modifier (AccessModifier),
        pub signature (Signature),
        pub inline_body (Option<InlineBody>),
    }
}

impl Arbitrary for Module {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = (AccessModifier::arbitrary(), Signature::arbitrary())
            .prop_map(|(access_modifier, signature)| Self {
                access_modifier,
                signature,
                inline_body: None,
            });

        leaf.prop_recursive(4, 24, 6, move |inner| {
            (
                AccessModifier::arbitrary(),
                Signature::arbitrary(),
                proptest::option::of(InlineBody::arbitrary_with(Some(inner))),
            )
                .prop_map(
                    |(access_modifier, signature, inline_body)| Self {
                        access_modifier,
                        signature,
                        inline_body,
                    },
                )
        })
        .boxed()
    }
}

impl IndentDisplay for Module {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{} {}", self.access_modifier, self.signature)?;
        if let Some(inline_body) = &self.inline_body {
            inline_body.indent_fmt(f, indent)?;
        }
        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone, EnumAsInner)]
    pub enum Member for super::Member {
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
        Effect(Effect),
    }
}

impl Arbitrary for Member {
    type Parameters = Option<BoxedStrategy<Module>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            arg.unwrap_or_else(Module::arbitrary).prop_map(Member::Module),
            Import::arbitrary().prop_map(Member::Import),
            Trait::arbitrary().prop_map(Member::Trait),
            Function::arbitrary().prop_map(Member::Function),
            Type::arbitrary().prop_map(Member::Type),
            Struct::arbitrary().prop_map(Member::Struct),
            Implements::arbitrary().prop_map(Member::Implements),
            Enum::arbitrary().prop_map(Member::Enum),
            Constant::arbitrary().prop_map(Member::Constant),
            Marker::arbitrary().prop_map(Member::Marker),
            Extern::arbitrary().prop_map(Member::Extern),
            Effect::arbitrary().prop_map(Member::Effect),
        ]
        .boxed()
    }
}

impl IndentDisplay for Member {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Module(module) => module.indent_fmt(f, indent),
            Self::Import(import) => write!(f, "{import}"),
            Self::Trait(trait_) => trait_.indent_fmt(f, indent),
            Self::Function(function) => function.indent_fmt(f, indent),
            Self::Type(type_) => type_.indent_fmt(f, indent),
            Self::Struct(struct_) => struct_.indent_fmt(f, indent),
            Self::Implements(implements) => implements.indent_fmt(f, indent),
            Self::Enum(r#enum) => r#enum.indent_fmt(f, indent),
            Self::Constant(constant) => constant.indent_fmt(f, indent),
            Self::Marker(marker) => marker.indent_fmt(f, indent),
            Self::Extern(extern_) => extern_.indent_fmt(f, indent),
            Self::Effect(effect) => effect.indent_fmt(f, indent),
        }
    }
}
