use std::fmt::{Display, Write};

use enum_as_inner::EnumAsInner;
use pernixc_lexical::kind::arbitrary::Identifier;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy},
    prop_oneof,
};

use crate::{
    arbitrary::{
        write_indent_line, write_indent_line_for_indent_display, IndentDisplay,
        IntoSeparated, Label, Passable, QualifiedIdentifier,
    },
    expression::{
        arbitrary::Expression, binary::arbitrary::Binary,
        terminator::arbitrary::Terminator,
    },
    pattern::arbitrary::{Irrefutable, Refutable},
    r#type::arbitrary::Type,
    reference,
    statement::arbitrary::{Statement, Statements},
};

reference! {
    #[derive(Debug, Clone)]
    pub enum Block for super::Block {
        Scope(Scope),
        IfElse(IfElse),
        Loop(Loop),
        Match(Match),
        While(While),
        Do(Do),
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
            IfElse::arbitrary_with(args.clone()).prop_map(Self::IfElse),
            Loop::arbitrary_with(args.clone()).prop_map(Self::Loop),
            Match::arbitrary_with(args.clone()).prop_map(Self::Match),
            While::arbitrary_with(args.clone()).prop_map(Self::While),
            Do::arbitrary_with(args).prop_map(Self::Do),
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
            Self::Loop(loop_) => loop_.indent_fmt(f, indent),
            Self::Match(match_) => match_.indent_fmt(f, indent),
            Self::While(while_) => while_.indent_fmt(f, indent),
            Self::Do(do_) => do_.indent_fmt(f, indent),
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

reference! {
    #[derive(Debug, Clone)]
    pub struct Loop for super::Loop {
        pub group (IndentedGroup)
    }
}

impl Arbitrary for Loop {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        IndentedGroup::arbitrary_with(args)
            .prop_map(|group| Self { group })
            .boxed()
    }
}

impl IndentDisplay for Loop {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("loop")?;

        if self.group.unsafe_keyword || self.group.label.is_some() {
            f.write_char(' ')?;
        }

        self.group.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct While for super::While {
        #{map_input_assert(&**binary, binary)}
        pub binary (Box<Binary>),
        pub group (IndentedGroup),
    }
}

impl Arbitrary for While {
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

        (binary.prop_map(Box::new), IndentedGroup::arbitrary_with(args))
            .prop_map(|(binary, group)| Self { binary, group })
            .boxed()
    }
}

impl IndentDisplay for While {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "while ")?;
        self.binary.indent_fmt(f, indent)?;

        if self.group.unsafe_keyword || self.group.label.is_some() {
            f.write_char(' ')?;
        }

        self.group.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct MatchArm for super::MatchArm {
        pub refutable_pattern (Refutable),
        pub group (Group),
    }
}

impl Arbitrary for MatchArm {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (Refutable::arbitrary(), Group::arbitrary_with(args))
            .prop_map(|(refutable_pattern, group)| Self {
                refutable_pattern,
                group,
            })
            .boxed()
    }
}

impl IndentDisplay for MatchArm {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{}", self.refutable_pattern)?;

        if self
            .group
            .as_indented()
            .is_some_and(|x| x.unsafe_keyword || x.label.is_some())
        {
            f.write_char(' ')?;
        }

        self.group.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct MatchBody for super::MatchBody {
        pub arms (Vec<Passable<MatchArm>>),
    }
}

impl Arbitrary for MatchBody {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(
            prop_oneof![
                8 => MatchArm::arbitrary_with(args).prop_map(Passable::Line),
                1 => Just(Passable::Pass)
            ],
            1..5,
        )
        .prop_map(|arms| Self { arms })
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Match for super::Match {
        #{map_input_assert(&**binary, binary)}
        pub binary (Box<Binary>),
        pub body (MatchBody),
    }
}

impl Arbitrary for Match {
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

        (binary.prop_map(Box::new), MatchBody::arbitrary_with(args))
            .prop_map(|(binary, body)| Self { binary, body })
            .boxed()
    }
}

impl IndentDisplay for Match {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "match ")?;
        self.binary.indent_fmt(f, indent)?;
        write_indent_line(f, &":", indent)?;

        for arm in &self.body.arms {
            write_indent_line_for_indent_display(f, arm, indent + 1)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct HandlerArguments for super::HandlerArguments {
        pub irrefutable_patterns (Vec<Irrefutable>)
    }
}

impl Arbitrary for HandlerArguments {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(Irrefutable::arbitrary(), 0..3)
            .prop_map(|irrefutable_patterns| Self { irrefutable_patterns })
            .boxed()
    }
}

impl IndentDisplay for HandlerArguments {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        f.write_char('(')?;

        self.irrefutable_patterns.into_separated(',').fmt(f)?;

        f.write_char(')')
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Handler for super::Handler {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (Identifier),
        pub arguments (HandlerArguments),
        pub statements (Statements),
    }
}

impl Arbitrary for Handler {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            HandlerArguments::arbitrary(),
            Statements::arbitrary_with(args),
        )
            .prop_map(|(identifier, arguments, statements)| Self {
                identifier,
                arguments,
                statements,
            })
            .boxed()
    }
}

impl IndentDisplay for Handler {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{} ", self.identifier)?;
        self.arguments.indent_fmt(f, indent)?;
        self.statements.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct WithBody for super::WithBody {
        pub handlers (Vec<Passable<Handler>>)
    }
}

impl Arbitrary for WithBody {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(
            prop_oneof![
                8 => Handler::arbitrary_with(args)
                    .prop_map(Passable::Line),
                1 => Just(Passable::Pass)
            ],
            1..5,
        )
        .prop_map(|handlers| Self { handlers })
        .boxed()
    }
}

impl IndentDisplay for WithBody {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        writeln!(f, ":",)?;
        for handler in &self.handlers {
            write_indent_line_for_indent_display(f, handler, indent + 1)?;
        }

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct With for super::With {
        pub effect (QualifiedIdentifier),
        pub body (WithBody),
    }
}

impl Arbitrary for With {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            QualifiedIdentifier::arbitrary_with((
                args.1.clone(),
                args.0.clone(),
            )),
            WithBody::arbitrary_with(args),
        )
            .prop_map(|(effect, body)| Self { effect, body })
            .boxed()
    }
}

impl IndentDisplay for With {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("with ")?;

        self.effect.indent_fmt(f, indent)?;
        self.body.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Do for super::Do {
        pub label (Option<Label>),
        pub statements (Statements),
        pub with (With),
    }
}

impl Arbitrary for Do {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(Label::arbitrary()),
            Statements::arbitrary_with(args.clone()),
            With::arbitrary_with(args),
        )
            .prop_map(|(label, statements, with)| Self {
                label,
                statements,
                with,
            })
            .boxed()
    }
}

impl IndentDisplay for Do {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("do")?;

        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        self.statements.indent_fmt(f, indent)?;

        write_indent_line_for_indent_display(f, &self.with, indent)
    }
}
