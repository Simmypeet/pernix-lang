use enum_as_inner::EnumAsInner;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_assert_eq, prop_oneof,
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    item::{
        constant::strategy::Constant, function::strategy::Function,
        implements::strategy::Implements, marker::strategy::Marker,
        r#enum::strategy::Enum, r#extern::strategy::Extern,
        r#struct::strategy::Struct, r#trait::strategy::Trait,
        r#type::strategy::Type,
    },
    strategy::{
        write_indent_line_for_indent_display, AccessModifier, ConnectedList,
        ConstantPunctuation, Identifier, IndentDisplay, Passable, SimplePath,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImportItem {
    pub identifier: Identifier,
    pub alias: Option<Identifier>,
}

impl Arbitrary for ImportItem {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Identifier::arbitrary(), proptest::option::of(Identifier::arbitrary()))
            .prop_map(|(identifier, alias)| Self { identifier, alias })
            .boxed()
    }
}

impl IndentDisplay for ImportItem {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{}", self.identifier)?;
        if let Some(alias) = &self.alias {
            write!(f, " as {alias}")?;
        }
        Ok(())
    }
}

impl Input<&super::ImportItem> for &ImportItem {
    fn assert(self, output: &super::ImportItem) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.alias.as_ref().assert(output.alias.as_ref().map(|x| &x.identifier))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Import {
    pub from: Option<SimplePath>,
    pub is_parenthesis_delimited: bool,
    pub items: Option<ConnectedList<ImportItem, ConstantPunctuation<','>>>,
}

impl Arbitrary for Import {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(SimplePath::arbitrary()),
            proptest::bool::ANY,
            proptest::option::of(ConnectedList::arbitrary_with(
                ImportItem::arbitrary(),
                ConstantPunctuation::arbitrary(),
            )),
        )
            .prop_map(|(from, is_parenthesis_delimited, items)| Self {
                from,
                is_parenthesis_delimited,
                items,
            })
            .prop_filter("filter out empty import items", |x| {
                if !x.is_parenthesis_delimited {
                    return x.items.is_some();
                }

                true
            })
            .boxed()
    }
}

impl IndentDisplay for Import {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(from) = &self.from {
            write!(f, "from {from} ")?;
        }
        write!(f, "import ")?;
        if self.is_parenthesis_delimited {
            write!(f, "(")?;
        }
        if let Some(items) = &self.items {
            items.indent_fmt(f, indent)?;
        }
        if self.is_parenthesis_delimited {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl Input<&super::Import> for &Import {
    fn assert(self, output: &super::Import) -> TestCaseResult {
        self.from
            .as_ref()
            .assert(output.from.as_ref().map(|x| &x.simple_path))?;

        prop_assert_eq!(
            self.is_parenthesis_delimited,
            output.parenthesis_delimited.is_some()
        );

        self.items.as_ref().assert(output.items.as_ref())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub identifier: Identifier,
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

impl IndentDisplay for Signature {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        write!(f, "module {}", self.identifier)
    }
}

impl Input<&super::Signature> for &Signature {
    fn assert(self, output: &super::Signature) -> TestCaseResult {
        self.identifier.assert(&output.identifier)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body {
    pub members: Vec<Passable<Member>>,
}

impl Arbitrary for Body {
    type Parameters = Option<BoxedStrategy<Module>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(
            prop_oneof![
                1 => Just(Passable::Pass),
                10 => Member::arbitrary_with(arg).prop_map(Passable::SyntaxTree)
            ],
            1..=10,
        )
        .prop_map(|members| Self { members })
        .boxed()
    }
}

impl IndentDisplay for Body {
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

impl Input<&super::Body> for &Body {
    fn assert(self, output: &super::Body) -> TestCaseResult {
        self.members.assert(&output.members)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Module {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub inline_body: Option<Body>,
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

        leaf.prop_recursive(4, 64, 16, move |inner| {
            (
                AccessModifier::arbitrary(),
                Signature::arbitrary(),
                proptest::option::of(Body::arbitrary_with(Some(inner))),
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
        write!(f, "{} ", self.access_modifier)?;
        self.signature.indent_fmt(f, indent)?;
        if let Some(inline_body) = &self.inline_body {
            writeln!(f, ":")?;
            inline_body.indent_fmt(f, indent + 1)?;
        }
        Ok(())
    }
}

impl Input<&super::Module> for &Module {
    fn assert(self, output: &super::Module) -> TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        self.signature.assert(&output.signature)?;
        self.inline_body.as_ref().map(|x| x.members.as_slice()).assert(
            output.inline_body.as_ref().map(|x| x.body.members.as_slice()),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
            Self::Import(import) => import.indent_fmt(f, indent),
            Self::Trait(trait_) => trait_.indent_fmt(f, indent),
            Self::Function(function) => function.indent_fmt(f, indent),
            Self::Type(type_) => type_.indent_fmt(f, indent),
            Self::Struct(struct_) => struct_.indent_fmt(f, indent),
            Self::Implements(implements) => implements.indent_fmt(f, indent),
            Self::Enum(r#enum) => r#enum.indent_fmt(f, indent),
            Self::Constant(constant) => constant.indent_fmt(f, indent),
            Self::Marker(marker) => marker.indent_fmt(f, indent),
            Self::Extern(extern_) => extern_.indent_fmt(f, indent),
        }
    }
}

impl Input<&super::Member> for &Member {
    fn assert(self, output: &super::Member) -> TestCaseResult {
        match (self, output) {
            (Member::Module(a), super::Member::Module(b)) => a.assert(b),
            (Member::Import(a), super::Member::Import(b)) => a.assert(b),
            (Member::Trait(a), super::Member::Trait(b)) => a.assert(b),
            (Member::Function(a), super::Member::Function(b)) => a.assert(b),
            (Member::Type(a), super::Member::Type(b)) => a.assert(b),
            (Member::Struct(a), super::Member::Struct(b)) => a.assert(b),
            (Member::Implements(a), super::Member::Implements(b)) => {
                a.assert(b)
            }
            (Member::Enum(a), super::Member::Enum(b)) => a.assert(b),
            (Member::Constant(a), super::Member::Constant(b)) => a.assert(b),
            (Member::Marker(a), super::Member::Marker(b)) => a.assert(b),
            (Member::Extern(a), super::Member::Extern(b)) => a.assert(b),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}
