use std::fmt::Write;

use enum_as_inner::EnumAsInner;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy},
    prop_oneof,
};

use crate::{
    arbitrary::{
        write_indent_line_for_indent_display, IndentDisplay, Label,
        QualifiedIdentifier,
    },
    expression::{
        arbitrary::Expression, binary::arbitrary::Binary,
        terminator::arbitrary::Terminator,
    },
    r#type::arbitrary::Type,
    reference,
    statement::arbitrary::{Statement, Statements},
};

reference! {
    #[derive(Debug, Clone)]
    pub enum Block for super::Block {
        Scope(Scope),
        IfElse(IfElse),
    }
}

impl Arbitrary for Block {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Scope::arbitrary_with(args.clone()).prop_map(Self::Scope),
            IfElse::arbitrary_with(args).prop_map(Self::IfElse),
        ]
        .boxed()
    }
}

impl IndentDisplay for Block {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Scope(scope) => scope.indent_fmt(f, indent),
            Self::IfElse(if_else) => if_else.indent_fmt(f, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Scope for super::Scope {
        pub unsafe_keyword (bool),
        pub label (Option<Label>),
        pub statements (Statements)
    }
}

impl Arbitrary for Scope {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            proptest::option::of(Label::arbitrary()),
            Statements::arbitrary_with(args),
        )
            .prop_map(|(unsafe_keyword, label, statements)| Self {
                unsafe_keyword,
                label,
                statements,
            })
            .boxed()
    }
}

impl IndentDisplay for Scope {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.unsafe_keyword {
            f.write_str("unsafe ")?;
        }

        f.write_str("scope")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        self.statements.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct IndentedGroup for super::IndentedGroup {
        pub unsafe_keyword (bool),
        pub label (Option<Label>),
        pub statements (Statements),
    }
}

impl IndentDisplay for IndentedGroup {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.unsafe_keyword {
            f.write_str("unsafe")?;
        }

        if let Some(label) = &self.label {
            if self.unsafe_keyword {
                f.write_char(' ')?;
            }

            write!(f, "{label}")?;
        }

        self.statements.indent_fmt(f, indent)
    }
}

impl Arbitrary for IndentedGroup {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            proptest::option::of(Label::arbitrary()),
            Statements::arbitrary_with(args),
        )
            .prop_map(|(unsafe_keyword, label, statements)| Self {
                unsafe_keyword,
                label,
                statements,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct InlineExpression for super::InlineExpression {
        #{map_input_assert(&**expression, expression)}
        pub expression (Box<Expression>),
    }
}

impl Arbitrary for InlineExpression {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        args.0
            .unwrap_or_else(|| {
                Expression::arbitrary_with((args.1, args.2, args.3))
            })
            .prop_filter("filter block appear last", |x| !match x {
                Expression::Binary(binary) => {
                    let node = binary
                        .chain
                        .last()
                        .map_or_else(|| &binary.first, |x| &x.node);

                    node.is_block()
                }
                Expression::Terminator(terminator) => {
                    let binary = match terminator {
                        Terminator::Return(a) => a.binary.as_ref(),
                        Terminator::Continue(_) => None,
                        Terminator::Express(a) => a.binary.as_ref(),
                        Terminator::Break(a) => a.binary.as_ref(),
                    };

                    let Some(binary) = binary else {
                        return true;
                    };

                    let node = binary
                        .chain
                        .last()
                        .map_or_else(|| &binary.first, |x| &x.node);

                    node.is_block()
                }
            })
            .prop_map(|expression| Self { expression: Box::new(expression) })
            .boxed()
    }
}

impl IndentDisplay for InlineExpression {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str(": ")?;
        self.expression.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone, EnumAsInner)]
    pub enum Group for super::Group {
        Indented(IndentedGroup),
        Inline(InlineExpression),
    }
}

impl Arbitrary for Group {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            IndentedGroup::arbitrary_with(args.clone())
                .prop_map(Group::Indented),
            InlineExpression::arbitrary_with(args).prop_map(Group::Inline),
        ]
        .boxed()
    }
}

impl IndentDisplay for Group {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Indented(indented_group) => {
                indented_group.indent_fmt(f, indent)
            }
            Self::Inline(inline_expression) => {
                inline_expression.indent_fmt(f, indent)
            }
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum GroupOrIfElse for super::GroupOrIfElse {
        Group(Group),
        IfElse(IfElse),
    }
}

impl IndentDisplay for GroupOrIfElse {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Group(group) => group.indent_fmt(f, indent),
            Self::IfElse(if_else) => if_else.indent_fmt(f, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct IfElse for super::IfElse {
        #{map_input_assert(&**binary, binary)}
        pub binary (Box<Binary>),
        pub then (Group),
        pub r#else (Option<Else>),
    }
}

impl Arbitrary for IfElse {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let binary = args
            .0
            .clone()
            .unwrap_or_else(|| {
                Expression::arbitrary_with((
                    args.1.clone(),
                    args.2.clone(),
                    args.3.clone(),
                ))
            })
            .prop_filter_map("need binary and must not contain block", |x| {
                x.into_binary().ok().and_then(|x| {
                    (!x.chain
                        .last()
                        .map_or_else(|| &x.first, |x| &x.node)
                        .is_block())
                    .then_some(x)
                })
            });

        let leaf = (
            binary.clone().prop_map(Box::new),
            Group::arbitrary_with(args.clone()),
        )
            .prop_map(|(binary, then)| Self {
                binary,
                then,
                r#else: None,
            });

        leaf.prop_recursive(4, 24, 6, move |inner| {
            (
                binary.clone().prop_map(Box::new),
                Group::arbitrary_with(args.clone()),
                proptest::option::of(prop_oneof![
                    Group::arbitrary_with(args.clone()).prop_map(|x| {
                        Else {
                            group_or_if_else: Box::new(GroupOrIfElse::Group(x)),
                        }
                    }),
                    inner.prop_map(|x| {
                        Else {
                            group_or_if_else: Box::new(GroupOrIfElse::IfElse(
                                x,
                            )),
                        }
                    })
                ]),
            )
                .prop_map(|(binary, then, r#else)| Self {
                    binary,
                    then,
                    r#else,
                })
        })
        .boxed()
    }
}

impl IndentDisplay for IfElse {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "if ")?;
        self.binary.indent_fmt(f, indent)?;
        if self
            .then
            .as_indented()
            .is_some_and(|x| x.unsafe_keyword || x.label.is_some())
        {
            f.write_char(' ')?;
        }
        self.then.indent_fmt(f, indent)?;

        if let Some(else_expression) = &self.r#else {
            if self.then.is_inline() {
                f.write_char(' ')?;
            }

            if self.then.is_indented() {
                write_indent_line_for_indent_display(
                    f,
                    else_expression,
                    indent,
                )?;
            } else {
                else_expression.indent_fmt(f, indent)?;
            }
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Else for super::Else {
        #{map_input_assert(&**group_or_if_else, group_or_if_else)}
        pub group_or_if_else (Box<GroupOrIfElse>),
    }
}

impl IndentDisplay for Else {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("else")?;
        let should_space = match &*self.group_or_if_else {
            GroupOrIfElse::Group(group) => {
                group.is_inline()
                    || group
                        .as_indented()
                        .is_some_and(|x| x.unsafe_keyword || x.label.is_some())
            }
            GroupOrIfElse::IfElse(_) => true,
        };

        if should_space {
            f.write_char(' ')?;
        }

        self.group_or_if_else.indent_fmt(f, indent)
    }
}
