use std::fmt::{Display, Write};

use enum_as_inner::EnumAsInner;
use pernixc_tests::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    self,
    expression::tests::{Expression, Functional},
    pattern,
    statement::tests::Statement,
    tests::{
        AccessModifier, ConnectedList, ConstantPunctuation, Identifier, LifetimeArgument,
        QualifiedIdentifier,
    },
    ty,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Module {
    pub access_modifier: AccessModifier,
    pub signature: ModuleSignature,
    pub kind: ModuleKind,
}

impl Input for Module {
    type Output = super::Module;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.kind.assert(output.kind())
    }
}

impl Arbitrary for Module {
    type Parameters = Option<BoxedStrategy<Item>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            ModuleSignature::arbitrary(),
            ModuleKind::arbitrary_with(args),
        )
            .prop_map(|(access_modifier, signature, content)| Self {
                access_modifier,
                signature,
                kind: content,
            })
            .boxed()
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}{}",
            self.access_modifier, self.signature, self.kind
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModulePath {
    pub first: Identifier,
    pub rest: Vec<Identifier>,
}

impl Input for ModulePath {
    type Output = super::ModulePath;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.first.assert(output.first())?;
        prop_assert_eq!(self.rest.len(), output.rest().len());

        for (input, (_, output)) in self.rest.iter().zip(output.rest().iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl Arbitrary for ModulePath {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::collection::vec(Identifier::arbitrary(), 0..=6),
        )
            .prop_map(|(first, rest)| Self { first, rest })
            .boxed()
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.first, f)?;

        for identifier in &self.rest {
            write!(f, "::{identifier}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Using {
    module_path: ModulePath,
}

impl Input for Using {
    type Output = super::Using;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.module_path.assert(output.module_path())
    }
}

impl Arbitrary for Using {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        ModulePath::arbitrary()
            .prop_map(|module_path| Self { module_path })
            .boxed()
    }
}

impl Display for Using {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "using {};", self.module_path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleSignature {
    identifier: Identifier,
}

impl Input for ModuleSignature {
    type Output = super::ModuleSignature;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())
    }
}

impl Arbitrary for ModuleSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Display for ModuleSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "module {}", self.identifier)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleContent {
    pub usings: Vec<Using>,
    pub items: Vec<Item>,
}

impl Input for ModuleContent {
    type Output = super::ModuleContent;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.usings.len(), output.usings().len());
        prop_assert_eq!(self.items.len(), output.items().len());

        for (input, output) in self.usings.iter().zip(output.usings().iter()) {
            input.assert(output)?;
        }

        for (input, output) in self.items.iter().zip(output.items().iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl Arbitrary for ModuleContent {
    type Parameters = Option<BoxedStrategy<Item>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let item_strategy = args.unwrap_or_else(Item::arbitrary);
        (
            proptest::collection::vec(Using::arbitrary(), 0..=6),
            proptest::collection::vec(item_strategy, 0..=6),
        )
            .prop_map(|(usings, items)| Self { usings, items })
            .boxed()
    }
}

impl Display for ModuleContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for using in &self.usings {
            write!(f, "{using}")?;
        }

        for item in &self.items {
            write!(f, "{item}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleKind {
    File,
    Inline(ModuleContent),
}

impl Input for ModuleKind {
    type Output = super::ModuleKind;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::File, super::ModuleKind::File(_)) => Ok(()),
            (Self::Inline(input), super::ModuleKind::Inline(output)) => {
                input.assert(output.content())
            }
            _ => Err(TestCaseError::fail(format!(
                "expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for ModuleKind {
    type Parameters = Option<BoxedStrategy<Item>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let item_strategy = args.unwrap_or_else(Item::arbitrary);

        prop_oneof![
            Just(Self::File),
            (ModuleContent::arbitrary_with(Some(item_strategy))).prop_map(Self::Inline),
        ]
        .boxed()
    }
}

impl Display for ModuleKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::File => write!(f, ";"),
            Self::Inline(module_body) => write!(f, "{{ {module_body} }}",),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameter {
    pub identifier: Identifier,
}

impl Input for LifetimeParameter {
    type Output = super::LifetimeParameter;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())
    }
}

impl Arbitrary for LifetimeParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Display for LifetimeParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.identifier)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstParameter {
    pub identifier: Identifier,
    pub ty: ty::tests::Type,
    pub default: Option<Functional>,
}

impl Input for ConstParameter {
    type Output = super::ConstParameter;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.ty.assert(output.ty())?;
        match (self.default.as_ref(), output.default().as_ref()) {
            (None, None) => Ok(()),
            (Some(expected), Some(output)) => expected.assert(output.value()),
            (expected, output) => Err(TestCaseError::fail(format!(
                "expected {expected:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for ConstParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            ty::tests::Type::arbitrary(),
            proptest::option::of(Expression::arbitrary().prop_filter_map(
                "allows only non-binary functional variant",
                |x| match x {
                    Expression::Functional(Functional::Binary(_)) => None,
                    Expression::Functional(functional) => Some(functional),
                    _ => None,
                },
            )),
        )
            .prop_map(|(identifier, ty, default)| Self {
                identifier,
                ty,
                default,
            })
            .boxed()
    }
}

impl Display for ConstParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.ty)?;

        if let Some(default) = self.default.as_ref() {
            write!(f, " = {default}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameter {
    pub identifier: Identifier,
    pub default: Option<ty::tests::Type>,
}

impl Input for TypeParameter {
    type Output = super::TypeParameter;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        match (self.default.as_ref(), output.default().as_ref()) {
            (None, None) => Ok(()),
            (Some(expected), Some(output)) => expected.assert(output.value()),
            (expected, output) => Err(TestCaseError::fail(format!(
                "expected {expected:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for TypeParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(ty::tests::Type::arbitrary()),
        )
            .prop_map(|(identifier, ty)| Self {
                identifier,
                default: ty,
            })
            .boxed()
    }
}

impl Display for TypeParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.identifier, f)?;

        if let Some(type_parameter) = self.default.as_ref() {
            write!(f, " = {type_parameter}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericParameter {
    Lifetime(LifetimeParameter),
    Type(TypeParameter),
    Const(ConstParameter),
}

impl Input for GenericParameter {
    type Output = super::GenericParameter;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Lifetime(i), super::GenericParameter::Lifetime(o)) => i.assert(o),
            (Self::Type(i), super::GenericParameter::Type(o)) => i.assert(o),
            (Self::Const(i), super::GenericParameter::Const(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, found {output:?}"
            ))),
        }
    }
}

impl Arbitrary for GenericParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::prop_oneof![
            LifetimeParameter::arbitrary().prop_map(Self::Lifetime),
            TypeParameter::arbitrary().prop_map(Self::Type),
            ConstParameter::arbitrary().prop_map(Self::Const),
        ]
        .boxed()
    }
}

impl Display for GenericParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lifetime(l) => Display::fmt(l, f),
            Self::Type(t) => Display::fmt(t, f),
            Self::Const(c) => Display::fmt(c, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParameters {
    pub parameter_list: ConnectedList<GenericParameter, ConstantPunctuation<','>>,
}

impl Input for GenericParameters {
    type Output = super::GenericParameters;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.parameter_list.assert(output.parameter_list())
    }
}

impl Arbitrary for GenericParameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        ConnectedList::arbitrary_with(
            GenericParameter::arbitrary(),
            ConstantPunctuation::arbitrary(),
        )
        .prop_map(|parameter_list| Self { parameter_list })
        .boxed()
    }
}

impl Display for GenericParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}>", self.parameter_list)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetimeParameters {
    pub lifetime_parameter_list: ConnectedList<LifetimeParameter, ConstantPunctuation<','>>,
}

impl Input for HigherRankedLifetimeParameters {
    type Output = super::HigherRankedLifetimeParameters;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.lifetime_parameter_list
            .assert(output.lifetime_parameter_list())
    }
}

impl Arbitrary for HigherRankedLifetimeParameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        ConnectedList::arbitrary_with(
            LifetimeParameter::arbitrary(),
            ConstantPunctuation::arbitrary(),
        )
        .prop_map(|lifetime_parameter_list| Self {
            lifetime_parameter_list,
        })
        .boxed()
    }
}

impl Display for HigherRankedLifetimeParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "for<{}>", self.lifetime_parameter_list)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBound {
    pub higher_ranked_lifetime_parameters: Option<HigherRankedLifetimeParameters>,
    pub is_const: bool,
    pub qualified_identifier: QualifiedIdentifier,
}

impl Input for TraitBound {
    type Output = super::TraitBound;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.higher_ranked_lifetime_parameters
            .assert(output.higher_ranked_lifetime_parameters())?;
        prop_assert_eq!(self.is_const, output.const_keyword.is_some());
        self.qualified_identifier
            .assert(output.qualified_identifier())
    }
}

impl Arbitrary for TraitBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimeParameters::arbitrary()),
            QualifiedIdentifier::arbitrary_with((false, None)),
            proptest::bool::ANY,
        )
            .prop_map(
                |(higher_ranked_lifetime_parameters, qualified_identifier, is_const)| Self {
                    higher_ranked_lifetime_parameters,
                    is_const,
                    qualified_identifier,
                },
            )
            .boxed()
    }
}

impl Display for TraitBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(higher_ranked_lifetime_parameters) = &self.higher_ranked_lifetime_parameters {
            write!(f, "{higher_ranked_lifetime_parameters} ")?;
        }

        if self.is_const {
            write!(f, "const ")?;
        }

        Display::fmt(&self.qualified_identifier, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundList<T> {
    pub first: T,
    pub rest: Vec<T>,
}

impl<T: Input> Input for BoundList<T> {
    type Output = super::BoundList<T::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.first.assert(output.first())?;

        prop_assert_eq!(self.rest.len(), output.rest().len());

        for (input, (_, output)) in self.rest.iter().zip(output.rest().iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl<T: Display> Display for BoundList<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.first, f)?;

        for bound in &self.rest {
            write!(f, " + {bound}")?;
        }

        Ok(())
    }
}

impl<T: std::fmt::Debug> BoundList<T> {
    fn arbitrary_with(args: impl Strategy<Value = T> + Clone + 'static) -> BoxedStrategy<Self> {
        (args.clone(), proptest::collection::vec(args, 0..=6))
            .prop_map(|(first, rest)| Self { first, rest })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum LifetimeBoundOperand {
    LifetimeParameter(LifetimeParameter),
    Type(ty::tests::Type),
}

impl Input for LifetimeBoundOperand {
    type Output = super::LifetimeBoundOperand;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::LifetimeParameter(i), super::LifetimeBoundOperand::LifetimeParameter(o)) => {
                i.assert(o)
            }
            (Self::Type(i), super::LifetimeBoundOperand::Type(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for LifetimeBoundOperand {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            LifetimeParameter::arbitrary().prop_map(Self::LifetimeParameter),
            ty::tests::Type::arbitrary().prop_map(Self::Type),
        ]
        .boxed()
    }
}

impl Display for LifetimeBoundOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LifetimeParameter(lifetime_parameter) => Display::fmt(lifetime_parameter, f),
            Self::Type(ty) => Display::fmt(ty, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeBound {
    pub operand: LifetimeBoundOperand,
    pub parameters: BoundList<LifetimeArgument>,
}

impl Input for LifetimeBound {
    type Output = super::LifetimeBound;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operand.assert(output.operand())?;
        self.parameters.assert(output.arguments())?;

        Ok(())
    }
}

impl Arbitrary for LifetimeBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            LifetimeBoundOperand::arbitrary(),
            BoundList::arbitrary_with(LifetimeArgument::arbitrary()),
        )
            .prop_map(|(operand, parameters)| Self {
                operand,
                parameters,
            })
            .boxed()
    }
}

impl Display for LifetimeBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.operand, self.parameters)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleBound {
    pub identifier: Identifier,
}

impl Input for TupleBound {
    type Output = super::TupleBound;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())
    }
}

impl Arbitrary for TupleBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Display for TupleBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.identifier)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TupleEach {
    Packed(ConnectedList<Identifier, ConstantPunctuation<','>>),
    Identifier(Identifier),
}

impl Input for TupleEach {
    type Output = super::TupleEach;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Packed(input), super::TupleEach::Packed(output)) => {
                input.assert(output.identifier_list())
            }
            (Self::Identifier(input), super::TupleEach::Identifier(output)) => input.assert(output),
            (input, output) => Err(TestCaseError::fail(format!(
                "Expected {input:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for TupleEach {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            ConnectedList::arbitrary_with(
                Identifier::arbitrary(),
                ConstantPunctuation::arbitrary()
            )
            .prop_map(Self::Packed),
            Identifier::arbitrary().prop_map(Self::Identifier),
        ]
        .boxed()
    }
}

impl Display for TupleEach {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Packed(input) => write!(f, "({input})"),
            Self::Identifier(input) => Display::fmt(input, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ForTupleBound {
    pub tuple_each_pattern: TupleEach,
    pub tuple_each_argument: TupleEach,
    pub constraint_list: Option<ConnectedList<Box<Constraint>, ConstantPunctuation<','>>>,
}

impl Input for ForTupleBound {
    type Output = super::ForTupleBound;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.tuple_each_pattern
            .assert(output.tuple_each_pattern())?;
        self.tuple_each_argument
            .assert(output.tuple_each_argument())?;
        self.constraint_list.assert(output.constraint_list())
    }
}

impl Arbitrary for ForTupleBound {
    type Parameters = Option<BoxedStrategy<Constraint>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        let constraint_strategy = arg.unwrap_or_else(Constraint::arbitrary);

        (
            TupleEach::arbitrary(),
            TupleEach::arbitrary(),
            proptest::option::of(ConnectedList::arbitrary_with(
                constraint_strategy.prop_map(Box::new),
                ConstantPunctuation::arbitrary(),
            )),
        )
            .prop_map(
                |(tuple_each_pattern, tuple_each_argument, constraint_list)| Self {
                    tuple_each_pattern,
                    tuple_each_argument,
                    constraint_list,
                },
            )
            .boxed()
    }
}

impl Display for ForTupleBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "for<{}: {}>",
            self.tuple_each_pattern, self.tuple_each_argument,
        )?;

        write!(f, "{{")?;
        if let Some(constraint_list) = self.constraint_list.as_ref() {
            write!(f, " {constraint_list} ")?;
        }
        write!(f, "}}")?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitAssociationBoundArgument {
    Const(Expression),
    Type(ty::tests::Type),
}

impl Input for TraitAssociationBoundArgument {
    type Output = super::TraitAssociationBoundArgument;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Const(i), super::TraitAssociationBoundArgument::Const(o)) => {
                i.assert(o.expression())
            }
            (Self::Type(i), super::TraitAssociationBoundArgument::Type(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for TraitAssociationBoundArgument {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Expression::arbitrary().prop_map(Self::Const),
            ty::tests::Type::arbitrary().prop_map(Self::Type),
        ]
        .boxed()
    }
}

impl Display for TraitAssociationBoundArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(expression) => {
                write!(f, "{{{expression}}}")
            }
            Self::Type(ty) => Display::fmt(ty, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociationBound {
    pub qualified_identifier: QualifiedIdentifier,
    pub argument: TraitAssociationBoundArgument,
}

impl Input for TraitAssociationBound {
    type Output = super::TraitAssociationBound;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.qualified_identifier
            .assert(output.qualified_identifier())?;
        self.argument.assert(output.argument())
    }
}

impl Arbitrary for TraitAssociationBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            QualifiedIdentifier::arbitrary_with((false, None)),
            TraitAssociationBoundArgument::arbitrary(),
        )
            .prop_map(|(qualified_identifier, argument)| Self {
                qualified_identifier,
                argument,
            })
            .boxed()
    }
}

impl Display for TraitAssociationBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.qualified_identifier, self.argument)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constraint {
    Trait(TraitBound),
    Lifetime(LifetimeBound),
    Tuple(TupleBound),
    ForTuple(ForTupleBound),
    TraitAssociation(TraitAssociationBound),
}

impl Input for Constraint {
    type Output = super::Constraint;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Trait(i), super::Constraint::Trait(o)) => i.assert(o),
            (Self::Lifetime(i), super::Constraint::Lifetime(o)) => i.assert(o),
            (Self::Tuple(i), super::Constraint::Tuple(o)) => i.assert(o),
            (Self::ForTuple(i), super::Constraint::ForTuple(o)) => i.assert(o),
            (Self::TraitAssociation(i), super::Constraint::TraitAssociation(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for Constraint {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            TraitBound::arbitrary().prop_map(Self::Trait),
            LifetimeBound::arbitrary().prop_map(Self::Lifetime),
            TupleBound::arbitrary().prop_map(Self::Tuple),
            TraitAssociationBound::arbitrary().prop_map(Self::TraitAssociation),
        ];

        leaf.prop_recursive(2, 12, 6, |inner| {
            ForTupleBound::arbitrary_with(Some(inner)).prop_map(Self::ForTuple)
        })
        .boxed()
    }
}

impl Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Trait(i) => Display::fmt(i, f),
            Self::Lifetime(i) => Display::fmt(i, f),
            Self::Tuple(i) => Display::fmt(i, f),
            Self::ForTuple(i) => Display::fmt(i, f),
            Self::TraitAssociation(i) => Display::fmt(i, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhereClause {
    pub constraint_list: ConnectedList<Constraint, ConstantPunctuation<','>>,
}

impl Input for WhereClause {
    type Output = super::WhereClause;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.constraint_list.assert(output.constraint_list())
    }
}

impl Arbitrary for WhereClause {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        ConnectedList::arbitrary_with(Constraint::arbitrary(), ConstantPunctuation::arbitrary())
            .prop_map(|constraint_list| Self { constraint_list })
            .boxed()
    }
}

impl Display for WhereClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "where: {}", self.constraint_list)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub irrefutable_pattern: pattern::tests::Irrefutable,
    pub ty: ty::tests::Type,
}

impl Input for Parameter {
    type Output = super::Parameter;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.irrefutable_pattern
            .assert(output.irrefutable_pattern())?;
        self.ty.assert(output.ty())
    }
}

impl Arbitrary for Parameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            pattern::tests::Irrefutable::arbitrary(),
            ty::tests::Type::arbitrary(),
        )
            .prop_map(|(irrefutable_pattern, ty)| Self {
                irrefutable_pattern,
                ty,
            })
            .boxed()
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.irrefutable_pattern, self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameters {
    pub parameter_list: Option<ConnectedList<Parameter, ConstantPunctuation<','>>>,
}

impl Input for Parameters {
    type Output = super::Parameters;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.parameter_list.assert(output.parameter_list())
    }
}

impl Arbitrary for Parameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            Parameter::arbitrary(),
            ConstantPunctuation::arbitrary(),
        ))
        .prop_map(|parameter_list| Self { parameter_list })
        .boxed()
    }
}

impl Display for Parameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('(')?;
        if let Some(parameter_list) = &self.parameter_list {
            Display::fmt(parameter_list, f)?;
        }
        f.write_char(')')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnType {
    pub ty: ty::tests::Type,
}

impl Input for ReturnType {
    type Output = super::ReturnType;

    fn assert(&self, output: &Self::Output) -> TestCaseResult { self.ty.assert(output.ty()) }
}

impl Arbitrary for ReturnType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        ty::tests::Type::arbitrary()
            .prop_map(|ty| Self { ty })
            .boxed()
    }
}

impl Display for ReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { Display::fmt(&self.ty, f) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionSignature {
    is_const: bool,
    identifier: Identifier,
    generic_parameters: Option<GenericParameters>,
    parameters: Parameters,
    return_type: Option<ReturnType>,
    where_clause: Option<WhereClause>,
}

impl Input for FunctionSignature {
    type Output = super::FunctionSignature;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.is_const, output.const_keyword.is_some());
        self.identifier.assert(output.identifier())?;
        self.generic_parameters
            .assert(output.generic_parameters())?;
        self.parameters.assert(output.parameters())?;
        self.return_type.assert(output.return_type())?;
        self.where_clause.assert(output.where_clause())
    }
}

impl Arbitrary for FunctionSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            Parameters::arbitrary(),
            proptest::option::of(ReturnType::arbitrary()),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(
                |(
                    is_const,
                    identifier,
                    generic_parameters,
                    parameters,
                    return_type,
                    where_clause,
                )| Self {
                    is_const,
                    identifier,
                    generic_parameters,
                    parameters,
                    return_type,
                    where_clause,
                },
            )
            .boxed()
    }
}

impl Display for FunctionSignature {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("function ")?;

        if self.is_const {
            formatter.write_str("const ")?;
        }

        Display::fmt(&self.identifier, formatter)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            Display::fmt(generic_parameters, formatter)?;
        }

        Display::fmt(&self.parameters, formatter)?;

        if let Some(return_type) = &self.return_type {
            write!(formatter, ": {return_type}")?;
        }

        if let Some(where_clause) = &self.where_clause {
            write!(formatter, " {where_clause}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionBody {
    pub statements: Vec<Statement>,
}

impl Input for FunctionBody {
    type Output = super::FunctionBody;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.statements.len(), output.statements().len());

        for (left, right) in self.statements.iter().zip(output.statements().iter()) {
            left.assert(right)?;
        }

        Ok(())
    }
}

impl Arbitrary for FunctionBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(Statement::arbitrary(), 0..=6)
            .prop_map(|statements| Self { statements })
            .boxed()
    }
}

impl Display for FunctionBody {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_char('{')?;

        for statement in &self.statements {
            Display::fmt(statement, formatter)?;
        }

        formatter.write_char('}')?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub access_modifier: AccessModifier,
    pub signature: FunctionSignature,
    pub body: FunctionBody,
}

impl Input for Function {
    type Output = super::Function;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for Function {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            FunctionSignature::arbitrary(),
            FunctionBody::arbitrary(),
        )
            .prop_map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .boxed()
    }
}

impl Display for Function {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "{} {} {}",
            self.access_modifier, self.signature, self.body
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstSignature {
    pub identifier: Identifier,
    pub ty: ty::tests::Type,
}

impl Input for ConstSignature {
    type Output = super::ConstSignature;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.ty.assert(output.ty())
    }
}

impl Arbitrary for ConstSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (Identifier::arbitrary(), ty::tests::Type::arbitrary())
            .prop_map(|(identifier, ty)| Self { identifier, ty })
            .boxed()
    }
}

impl Display for ConstSignature {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "const {}: {}", self.identifier, self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstDefinition {
    expression: Expression,
}

impl Input for ConstDefinition {
    type Output = super::ConstDefinition;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.expression.assert(output.expression())
    }
}

impl Arbitrary for ConstDefinition {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        Expression::arbitrary()
            .prop_map(|expression| Self { expression })
            .boxed()
    }
}

impl Display for ConstDefinition {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, " = {};", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Const {
    access_modifier: AccessModifier,
    signature: ConstSignature,
    definition: ConstDefinition,
}

impl Input for Const {
    type Output = super::Const;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.definition.assert(output.definition())
    }
}

impl Arbitrary for Const {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            ConstSignature::arbitrary(),
            ConstDefinition::arbitrary(),
        )
            .prop_map(|(access_modifier, signature, definition)| Self {
                access_modifier,
                signature,
                definition,
            })
            .boxed()
    }
}

impl Display for Const {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "{} {}{}",
            self.access_modifier, self.signature, self.definition
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Item {
    Function(Function),
    Trait(Trait),
    Type(Type),
    Struct(Struct),
    Module(Module),
    Enum(Enum),
    Implements(Implements),
    Const(Const),
}

impl Input for Item {
    type Output = super::Item;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Function(i), super::Item::Function(o)) => i.assert(o),
            (Self::Trait(i), super::Item::Trait(o)) => i.assert(o),
            (Self::Type(i), super::Item::Type(o)) => i.assert(o),
            (Self::Struct(i), super::Item::Struct(o)) => i.assert(o),
            (Self::Enum(i), super::Item::Enum(o)) => i.assert(o),
            (Self::Implements(i), super::Item::Implements(o)) => i.assert(o),
            (Self::Module(i), super::Item::Module(o)) => i.assert(o),
            (Self::Const(i), super::Item::Const(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Item {
    type Parameters = bool;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(avoid_module: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Function::arbitrary().prop_map(Self::Function),
            Trait::arbitrary().prop_map(Self::Trait),
            Type::arbitrary().prop_map(Self::Type),
            Struct::arbitrary().prop_map(Self::Struct),
            Enum::arbitrary().prop_map(Self::Enum),
            Implements::arbitrary().prop_map(Self::Implements),
            Const::arbitrary().prop_map(Self::Const),
        ];

        if avoid_module {
            leaf.boxed()
        } else {
            leaf.prop_recursive(4, 24, 6, move |inner| {
                Module::arbitrary_with(Some(inner)).prop_map(Self::Module)
            })
            .boxed()
        }
    }
}

impl Display for Item {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(f) => Display::fmt(f, formatter),
            Self::Trait(t) => Display::fmt(t, formatter),
            Self::Type(t) => Display::fmt(t, formatter),
            Self::Struct(s) => Display::fmt(s, formatter),
            Self::Enum(e) => Display::fmt(e, formatter),
            Self::Implements(i) => Display::fmt(i, formatter),
            Self::Module(m) => Display::fmt(m, formatter),
            Self::Const(c) => Display::fmt(c, formatter),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitSignature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub where_clause: Option<WhereClause>,
}

impl Input for TraitSignature {
    type Output = super::TraitSignature;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters
            .assert(output.generic_parameters())?;
        self.where_clause.assert(output.where_clause())
    }
}

impl Arbitrary for TraitSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(identifier, generic_parameters, where_clause)| Self {
                identifier,
                generic_parameters,
                where_clause,
            })
            .boxed()
    }
}

impl Display for TraitSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "trait {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            Display::fmt(generic_parameters, f)?;
        }

        if let Some(where_clause) = &self.where_clause {
            write!(f, " {where_clause}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeSignature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
}

impl Input for TypeSignature {
    type Output = super::TypeSignature;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters.assert(output.generic_parameters())
    }
}

impl Arbitrary for TypeSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
        )
            .prop_map(|(identifier, generic_parameters)| Self {
                identifier,
                generic_parameters,
            })
            .boxed()
    }
}

impl Display for TypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "type {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            Display::fmt(generic_parameters, f)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitFunction {
    pub function_signature: FunctionSignature,
}

impl Input for TraitFunction {
    type Output = super::TraitFunction;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.function_signature.assert(output.signature())
    }
}

impl Arbitrary for TraitFunction {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        FunctionSignature::arbitrary()
            .prop_map(|function_signature| Self { function_signature })
            .boxed()
    }
}

impl Display for TraitFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{};", self.function_signature)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitType {
    pub type_signature: TypeSignature,
}

impl Input for TraitType {
    type Output = super::TraitType;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.type_signature.assert(output.signature())
    }
}

impl Arbitrary for TraitType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        TypeSignature::arbitrary()
            .prop_map(|type_signature| Self { type_signature })
            .boxed()
    }
}

impl Display for TraitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{};", self.type_signature)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitMember {
    Function(TraitFunction),
    Type(TraitType),
}

impl Input for TraitMember {
    type Output = super::TraitMember;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Function(f), super::TraitMember::Function(g)) => f.assert(g),
            (Self::Type(f), super::TraitMember::Type(g)) => f.assert(g),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for TraitMember {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            TraitFunction::arbitrary().prop_map(Self::Function),
            TraitType::arbitrary().prop_map(Self::Type),
        ]
        .boxed()
    }
}

impl Display for TraitMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(function) => Display::fmt(function, f),
            Self::Type(type_) => Display::fmt(type_, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBody {
    pub members: Vec<TraitMember>,
}

impl Input for TraitBody {
    type Output = super::TraitBody;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.members.len(), output.members().len());

        for (left, right) in self.members.iter().zip(output.members().iter()) {
            left.assert(right)?;
        }

        Ok(())
    }
}

impl Arbitrary for TraitBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(TraitMember::arbitrary(), 0..=6)
            .prop_map(|members| Self { members })
            .boxed()
    }
}

impl Display for TraitBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;

        for member in &self.members {
            Display::fmt(member, f)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait {
    pub access_modifier: AccessModifier,
    pub signature: TraitSignature,
    pub body: TraitBody,
}

impl Input for Trait {
    type Output = super::Trait;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for Trait {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            TraitSignature::arbitrary(),
            TraitBody::arbitrary(),
        )
            .prop_map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .boxed()
    }
}

impl Display for Trait {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.access_modifier, self.signature, self.body
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefinition {
    pub ty: ty::tests::Type,
}

impl Input for TypeDefinition {
    type Output = super::TypeDefinition;

    fn assert(&self, output: &Self::Output) -> TestCaseResult { self.ty.assert(output.ty()) }
}

impl Arbitrary for TypeDefinition {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        ty::tests::Type::arbitrary()
            .prop_map(|ty| Self { ty })
            .boxed()
    }
}

impl Display for TypeDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "= {}", self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub access_modifier: AccessModifier,
    pub signature: TypeSignature,
    pub definition: TypeDefinition,
}

impl Input for Type {
    type Output = super::Type;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.definition.assert(output.definition())
    }
}

impl Arbitrary for Type {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            TypeSignature::arbitrary(),
            TypeDefinition::arbitrary(),
        )
            .prop_map(|(access_modifier, signature, definition)| Self {
                access_modifier,
                signature,
                definition,
            })
            .boxed()
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {};",
            self.access_modifier, self.signature, self.definition
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructField {
    pub access_modifier: AccessModifier,
    pub identifier: Identifier,
    pub ty: ty::tests::Type,
}

impl Input for StructField {
    type Output = super::StructField;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.identifier.assert(output.identifier())?;
        self.ty.assert(output.ty())
    }
}

impl Arbitrary for StructField {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            Identifier::arbitrary(),
            ty::tests::Type::arbitrary(),
        )
            .prop_map(|(access_modifier, identifier, ty)| Self {
                access_modifier,
                identifier,
                ty,
            })
            .boxed()
    }
}

impl Display for StructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}: {}",
            self.access_modifier, self.identifier, self.ty
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StructMember {
    Field(StructField),
}

impl Input for StructMember {
    type Output = super::StructMember;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Field(i), super::StructMember::Field(o)) => i.assert(o),
        }
    }
}

impl Arbitrary for StructMember {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![StructField::arbitrary().prop_map(Self::Field),].boxed()
    }
}

impl Display for StructMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Field(field) => Display::fmt(field, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructBody {
    struct_member_list: Option<ConnectedList<StructMember, ConstantPunctuation<','>>>,
}

impl Input for StructBody {
    type Output = super::StructBody;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.struct_member_list
            .assert(output.struct_member_list())?;

        Ok(())
    }
}

impl Arbitrary for StructBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            StructMember::arbitrary(),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(|struct_member_list| Self { struct_member_list })
        .boxed()
    }
}

impl Display for StructBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{')?;

        if let Some(struct_member_list) = &self.struct_member_list {
            Display::fmt(&struct_member_list, f)?;
        }

        f.write_char('}')?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructSignature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub where_clause: Option<WhereClause>,
}

impl Input for StructSignature {
    type Output = super::StructSignature;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters
            .assert(output.generic_parameters())?;
        self.where_clause.assert(output.where_clause())
    }
}

impl Arbitrary for StructSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(identifier, generic_parameters, where_clause)| Self {
                identifier,
                generic_parameters,
                where_clause,
            })
            .boxed()
    }
}

impl Display for StructSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            Display::fmt(generic_parameters, f)?;
        }

        if let Some(where_clause) = &self.where_clause {
            write!(f, " {where_clause}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub access_modifier: AccessModifier,
    pub signature: StructSignature,
    pub body: StructBody,
}

impl Input for Struct {
    type Output = super::Struct;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for Struct {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            StructSignature::arbitrary(),
            StructBody::arbitrary(),
        )
            .prop_map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .boxed()
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.access_modifier, self.signature, self.body
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumSignature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
}

impl Input for EnumSignature {
    type Output = super::EnumSignature;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters.assert(output.generic_parameters())
    }
}

impl Arbitrary for EnumSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
        )
            .prop_map(|(identifier, generic_parameters)| Self {
                identifier,
                generic_parameters,
            })
            .boxed()
    }
}

impl Display for EnumSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "enum {}", self.identifier)?;

        if let Some(generic_parameters) = self.generic_parameters.as_ref() {
            Display::fmt(generic_parameters, f)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumVariant {
    pub identifier: Identifier,
    pub variant_association: Option<ty::tests::Type>,
}

impl Input for EnumVariant {
    type Output = super::EnumVariant;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        match (
            self.variant_association.as_ref(),
            output.variant_association().as_ref(),
        ) {
            (None, None) => Ok(()),
            (Some(expected), Some(output)) => expected.assert(&output.ty),
            (expected, output) => Err(TestCaseError::fail(format!(
                "expected associated value {expected:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for EnumVariant {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(ty::tests::Type::arbitrary()),
        )
            .prop_map(|(identifier, variant_association)| Self {
                identifier,
                variant_association,
            })
            .boxed()
    }
}

impl Display for EnumVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)?;

        if let Some(variant_association) = self.variant_association.as_ref() {
            write!(f, "({variant_association})")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumBody {
    pub variant_list: Option<ConnectedList<EnumVariant, ConstantPunctuation<','>>>,
}

impl Input for EnumBody {
    type Output = super::EnumBody;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.variant_list.assert(output.variant_list())
    }
}

impl Arbitrary for EnumBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            EnumVariant::arbitrary(),
            ConstantPunctuation::arbitrary(),
        ))
        .prop_map(|variant_list| Self { variant_list })
        .boxed()
    }
}

impl Display for EnumBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{')?;
        if let Some(variant_list) = &self.variant_list {
            variant_list.fmt(f)?;
        }
        f.write_char('}')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    pub access_modifier: AccessModifier,
    pub signature: EnumSignature,
    pub body: EnumBody,
}

impl Input for Enum {
    type Output = super::Enum;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for Enum {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            EnumSignature::arbitrary(),
            EnumBody::arbitrary(),
        )
            .prop_map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .boxed()
    }
}

impl Display for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.access_modifier, self.signature, self.body
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementsType {
    pub signature: TypeSignature,
    pub definition: TypeDefinition,
}

impl Input for ImplementsType {
    type Output = super::ImplementsType;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.signature.assert(output.signature())?;
        self.definition.assert(output.definition())
    }
}

impl Arbitrary for ImplementsType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (TypeSignature::arbitrary(), TypeDefinition::arbitrary())
            .prop_map(|(signature, definition)| Self {
                signature,
                definition,
            })
            .boxed()
    }
}

impl Display for ImplementsType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.signature, self.definition)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementsFunction {
    pub signature: FunctionSignature,
    pub body: FunctionBody,
}

impl Input for ImplementsFunction {
    type Output = super::ImplementsFunction;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for ImplementsFunction {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (FunctionSignature::arbitrary(), FunctionBody::arbitrary())
            .prop_map(|(signature, body)| Self { signature, body })
            .boxed()
    }
}

impl Display for ImplementsFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.signature, self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplementsMember {
    Type(ImplementsType),
    Function(ImplementsFunction),
}

impl Input for ImplementsMember {
    type Output = super::ImplementsMember;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Type(input), super::ImplementsMember::Type(output)) => input.assert(output),
            (Self::Function(input), super::ImplementsMember::Function(output)) => {
                input.assert(output)
            }
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for ImplementsMember {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            ImplementsType::arbitrary().prop_map(Self::Type),
            ImplementsFunction::arbitrary().prop_map(Self::Function),
        ]
        .boxed()
    }
}

impl Display for ImplementsMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type(t) => Display::fmt(t, f),
            Self::Function(t) => Display::fmt(t, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementsBody {
    pub members: Vec<ImplementsMember>,
}

impl Input for ImplementsBody {
    type Output = super::ImplementsBody;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.members.len(), output.members().len());

        for (input, output) in self.members.iter().zip(output.members().iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl Arbitrary for ImplementsBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (proptest::collection::vec(ImplementsMember::arbitrary(), 0..=6))
            .prop_map(|members| Self { members })
            .boxed()
    }
}

impl Display for ImplementsBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{')?;
        for member in &self.members {
            Display::fmt(member, f)?;
        }
        f.write_char('}')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementsSignature {
    pub generic_parameters: Option<GenericParameters>,
    pub is_const: bool,
    pub qualified_identifier: QualifiedIdentifier,
    pub where_clause: Option<WhereClause>,
}

impl Input for ImplementsSignature {
    type Output = super::ImplementsSignature;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.generic_parameters
            .assert(output.generic_parameters())?;
        self.qualified_identifier
            .assert(output.qualified_identifier())?;
        prop_assert_eq!(self.is_const, output.const_keyword.is_some());
        self.where_clause.assert(output.where_clause())?;

        Ok(())
    }
}

impl Arbitrary for ImplementsSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(GenericParameters::arbitrary()),
            QualifiedIdentifier::arbitrary_with((false, None)),
            proptest::bool::ANY,
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(
                |(generic_parameters, qualified_identifier, is_const, where_clause)| Self {
                    generic_parameters,
                    is_const,
                    qualified_identifier,
                    where_clause,
                },
            )
            .boxed()
    }
}

impl Display for ImplementsSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "implements")?;

        if let Some(generic_parameters) = &self.generic_parameters {
            Display::fmt(generic_parameters, f)?;
        }

        if self.is_const {
            write!(f, " const")?;
        }

        write!(f, " {}", self.qualified_identifier)?;

        if let Some(where_clause) = &self.where_clause {
            write!(f, " {where_clause}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Implements {
    pub signature: ImplementsSignature,
    pub body: ImplementsBody,
}

impl Input for Implements {
    type Output = super::Implements;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for Implements {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            ImplementsSignature::arbitrary(),
            ImplementsBody::arbitrary(),
        )
            .prop_map(|(signature, body)| Self { signature, body })
            .boxed()
    }
}

impl Display for Implements {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.signature, self.body)
    }
}

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn item_test(
        item_input in Item::arbitrary()
    ) {
        let source = item_input.to_string();

        let item = syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_item(handler)
        )?;

        item_input.assert(&item)?;
    }
}
