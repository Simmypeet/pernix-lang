use std::fmt::{Display, Write};

use enum_as_inner::EnumAsInner;
use pernixc_test_input::Input;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_assert_eq, prop_oneof,
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{
    expression::{
        binary::strategy::Binary, strategy::Expression,
        terminator::strategy::Terminator,
    },
    pattern::strategy::Refutable,
    r#type::strategy::Type,
    statement::strategy::{Statement, Statements},
    strategy::{
        write_indent_line, write_indent_line_for_indent_display, IndentDisplay,
        Label, Passable, QualifiedIdentifier,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Block {
    Scope(Scope),
    Loop(Loop),
    IfElse(IfElse),
    Match(Match),
    While(While),
}

impl Input<&super::Block> for &Block {
    fn assert(self, output: &super::Block) -> TestCaseResult {
        match (self, output) {
            (Block::Scope(input), super::Block::Scope(output)) => {
                input.assert(output)
            }
            (Block::Loop(input), super::Block::Loop(output)) => {
                input.assert(output)
            }
            (Block::IfElse(input), super::Block::IfElse(output)) => {
                input.assert(output)
            }
            (Block::Match(input), super::Block::Match(output)) => {
                input.assert(output)
            }
            (Block::While(input), super::Block::While(output)) => {
                input.assert(output)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
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
            Scope::arbitrary_with(args.clone()).prop_map(Block::Scope),
            Loop::arbitrary_with(args.clone()).prop_map(Block::Loop),
            IfElse::arbitrary_with(args.clone()).prop_map(Block::IfElse),
            While::arbitrary_with(args.clone()).prop_map(Block::While),
            Match::arbitrary_with(args).prop_map(Block::Match),
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
            Self::Scope(block) => block.indent_fmt(f, indent),
            Self::Loop(loop_) => loop_.indent_fmt(f, indent),
            Self::IfElse(if_else) => if_else.indent_fmt(f, indent),
            Self::Match(match_) => match_.indent_fmt(f, indent),
            Self::While(while_) => while_.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope {
    pub is_unsafe: bool,
    pub label_specifier: Option<Label>,
    pub statements: Statements,
}

impl Input<&super::Scope> for &Scope {
    fn assert(self, output: &super::Scope) -> TestCaseResult {
        prop_assert_eq!(self.is_unsafe, output.unsafe_keyword.is_some());
        self.label_specifier.as_ref().assert(output.label.as_ref())?;
        self.statements.assert(&output.statements)
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
            .prop_map(|(is_unsafe, label_specifier, statements)| Self {
                is_unsafe,
                label_specifier,
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
        if self.is_unsafe {
            f.write_str("unsafe ")?;
        }

        f.write_str("scope")?;

        if let Some(label_specifier) = &self.label_specifier {
            write!(f, " {label_specifier}")?;
        }

        self.statements.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Group {
    Indented(IndentedGroup),
    Inline(Box<Expression>),
}

impl Input<&super::Group> for &Group {
    fn assert(self, output: &super::Group) -> TestCaseResult {
        match (self, output) {
            (Group::Indented(input), super::Group::Indented(output)) => {
                input.assert(output)
            }
            (Group::Inline(input), super::Group::Inline(output)) => {
                input.assert(&output.expression)
            }

            (input, output) => Err(TestCaseError::fail(format!(
                "expected {input:?}, got {output:?}",
            ))),
        }
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
                .prop_map(Self::Indented),
            args.0
                .unwrap_or_else(|| {
                    Expression::arbitrary_with((args.1, args.2, args.3))
                })
                .prop_filter("filter block appear last", |x| {
                    !match x {
                        Expression::Binary(binary) => {
                            let node = binary
                                .chain
                                .last()
                                .map_or_else(|| &binary.first, |x| &x.1);

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
                                .map_or_else(|| &binary.first, |x| &x.1);

                            node.is_block()
                        }
                    }
                })
                .prop_map(|expression| Self::Inline(Box::new(expression))),
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
            Self::Indented(group) => group.indent_fmt(f, indent),
            Self::Inline(expression) => {
                f.write_str(": ")?;
                expression.indent_fmt(f, indent)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IndentedGroup {
    pub is_unsafe: bool,
    pub label: Option<Label>,
    pub statements: Statements,
}

impl Input<&super::IndentedGroup> for &IndentedGroup {
    fn assert(self, output: &super::IndentedGroup) -> TestCaseResult {
        prop_assert_eq!(self.is_unsafe, output.unsafe_keyword.is_some());
        self.label.as_ref().assert(output.label.as_ref())?;
        self.statements.assert(&output.statements)
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
            .prop_map(|(is_unsafe, label, statements)| Self {
                is_unsafe,
                label,
                statements,
            })
            .boxed()
    }
}

impl IndentDisplay for IndentedGroup {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.is_unsafe {
            f.write_str(" unsafe ")?;
        }

        if let Some(label) = &self.label {
            write!(f, " {label} ")?;
        }

        self.statements.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfElse {
    pub binary: Box<Binary>,
    pub then_expression: Group,
    pub else_expression: Option<Else>,
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
                        .map_or_else(|| &x.first, |x| &x.1)
                        .is_block())
                    .then_some(x)
                })
            });

        let leaf = (
            binary.clone().prop_map(Box::new),
            Group::arbitrary_with(args.clone()),
        )
            .prop_map(|(binary, then_expression)| Self {
                binary,
                then_expression,
                else_expression: None,
            });

        leaf.prop_recursive(4, 24, 6, move |inner| {
            (
                binary.clone().prop_map(Box::new),
                Group::arbitrary_with(args.clone()),
                proptest::option::of(prop_oneof![
                    Group::arbitrary_with(args.clone()).prop_map(|x| {
                        Else { expression: Box::new(GroupOrIfElse::Group(x)) }
                    }),
                    inner.prop_map(|x| {
                        Else { expression: Box::new(GroupOrIfElse::IfElse(x)) }
                    })
                ]),
            )
                .prop_map(
                    |(binary, then_expression, else_expression)| Self {
                        binary,
                        then_expression,
                        else_expression,
                    },
                )
        })
        .boxed()
    }
}

impl Input<&super::IfElse> for &IfElse {
    fn assert(self, output: &super::IfElse) -> TestCaseResult {
        self.binary.assert(&output.binary)?;
        self.then_expression.assert(&output.then_expression)?;
        self.else_expression.as_ref().assert(output.else_expression.as_ref())
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
        self.then_expression.indent_fmt(f, indent)?;

        if let Some(else_expression) = &self.else_expression {
            if self.then_expression.is_inline() {
                f.write_char(' ')?;
            }

            if self.then_expression.is_indented() {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum GroupOrIfElse {
    Group(Group),
    IfElse(IfElse),
}

impl Input<&super::GroupOrIfElse> for &GroupOrIfElse {
    fn assert(self, output: &super::GroupOrIfElse) -> TestCaseResult {
        match (self, output) {
            (GroupOrIfElse::Group(i), super::GroupOrIfElse::Group(o)) => {
                i.assert(o)
            }
            (GroupOrIfElse::IfElse(i), super::GroupOrIfElse::IfElse(o)) => {
                i.assert(o)
            }
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} to be {output:?}"
            ))),
        }
    }
}

impl IndentDisplay for GroupOrIfElse {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Group(block) => block.indent_fmt(f, indent),
            Self::IfElse(if_else) => if_else.indent_fmt(f, indent),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Else {
    expression: Box<GroupOrIfElse>,
}

impl Input<&super::Else> for &Else {
    fn assert(self, output: &super::Else) -> TestCaseResult {
        self.expression.assert(&output.expression)
    }
}

impl IndentDisplay for Else {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("else")?;
        let should_space = match &*self.expression {
            GroupOrIfElse::Group(group) => group.is_inline(),
            GroupOrIfElse::IfElse(_) => true,
        };

        if should_space {
            f.write_char(' ')?;
        }

        self.expression.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loop {
    pub group: IndentedGroup,
}

impl Input<&super::Loop> for &Loop {
    fn assert(self, output: &super::Loop) -> TestCaseResult {
        self.group.assert(&output.group)
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
        self.group.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct While {
    pub binary: Box<Binary>,
    pub group: IndentedGroup,
}

impl Input<&super::While> for &While {
    fn assert(self, output: &super::While) -> TestCaseResult {
        self.binary.assert(&output.binary)?;
        self.group.assert(&output.group)
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
                        .map_or_else(|| &x.first, |x| &x.1)
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
        self.group.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MatchArm {
    pub refutable_pattern: Refutable,
    pub group: Group,
}

impl Input<&super::MatchArm> for &MatchArm {
    fn assert(self, output: &super::MatchArm) -> TestCaseResult {
        self.refutable_pattern.assert(&output.refutable_pattern)?;
        self.group.assert(&output.group)
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
        self.refutable_pattern.fmt(f)?;
        self.group.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Match {
    pub binary: Box<Binary>,
    pub arms: Vec<Passable<MatchArm>>,
}

impl Input<&super::Match> for &Match {
    fn assert(self, output: &super::Match) -> TestCaseResult {
        self.binary.assert(&output.binary)?;
        self.arms.assert(&output.body.arms)
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
                        .map_or_else(|| &x.first, |x| &x.1)
                        .is_block())
                    .then_some(x)
                })
            });

        (
            binary.prop_map(Box::new),
            proptest::collection::vec(
                prop_oneof![
                    10 => MatchArm::arbitrary_with(args)
                        .prop_map(Passable::SyntaxTree),
                    1 => Just(Passable::Pass),
                ],
                1..10,
            ),
        )
            .prop_map(|(binary, arms)| Self { binary, arms })
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

        for arm in &self.arms {
            write_indent_line_for_indent_display(f, arm, indent + 1)?;
        }

        Ok(())
    }
}
