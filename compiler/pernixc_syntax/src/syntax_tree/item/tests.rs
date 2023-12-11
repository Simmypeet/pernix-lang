use std::fmt::{Debug, Display, Write};

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
    expression::{self, tests::Expression},
    pattern::tests::Irrefutable,
    r#type,
    statement::tests::Statement,
    tests::{
        AccessModifier, ConnectedList, ConstantPunctuation, Identifier, Lifetime,
        QualifiedIdentifier,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Module {
    pub access_modifier: AccessModifier,
    pub signature: ModuleSignature,
    pub kind: ModuleKind,
}

impl Input<&super::Module> for &Module {
    fn assert(self, output: &super::Module) -> TestCaseResult {
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

impl Input<&super::ModulePath> for &ModulePath {
    fn assert(self, output: &super::ModulePath) -> TestCaseResult {
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

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Input<&super::Using> for &Using {
    fn assert(self, output: &super::Using) -> TestCaseResult {
        self.module_path.assert(output.module_path())
    }
}

impl Arbitrary for Using {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Input<&super::ModuleSignature> for &ModuleSignature {
    fn assert(self, output: &super::ModuleSignature) -> TestCaseResult {
        self.identifier.assert(output.identifier())
    }
}

impl Arbitrary for ModuleSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Input<&super::ModuleContent> for &ModuleContent {
    fn assert(self, output: &super::ModuleContent) -> TestCaseResult {
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

impl Input<&super::ModuleKind> for &ModuleKind {
    fn assert(self, output: &super::ModuleKind) -> TestCaseResult {
        match (self, output) {
            (ModuleKind::File, super::ModuleKind::File(_)) => Ok(()),
            (ModuleKind::Inline(input), super::ModuleKind::Inline(output)) => {
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

impl Input<&super::LifetimeParameter> for &LifetimeParameter {
    fn assert(self, output: &super::LifetimeParameter) -> TestCaseResult {
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
pub struct ConstantParameter {
    pub identifier: Identifier,
    pub ty: r#type::tests::Type,
    pub default: Option<Expression>,
}

impl Input<&super::ConstantParameter> for &ConstantParameter {
    fn assert(self, output: &super::ConstantParameter) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.ty.assert(output.r#type())?;
        self.default
            .as_ref()
            .assert(output.default().as_ref().map(|x| &x.value))
    }
}

impl Arbitrary for ConstantParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            r#type::tests::Type::arbitrary(),
            proptest::option::of(Expression::arbitrary()),
        )
            .prop_map(|(identifier, ty, default)| Self {
                identifier,
                ty,
                default,
            })
            .boxed()
    }
}

impl Display for ConstantParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "const {}: {}", self.identifier, self.ty)?;

        if let Some(default) = self.default.as_ref() {
            write!(f, " = {default}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameter {
    pub identifier: Identifier,
    pub default: Option<r#type::tests::Type>,
}

impl Input<&super::TypeParameter> for &TypeParameter {
    fn assert(self, output: &super::TypeParameter) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.default
            .as_ref()
            .assert(output.default.as_ref().map(|x| &x.value))
    }
}

impl Arbitrary for TypeParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(r#type::tests::Type::arbitrary()),
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
    Const(ConstantParameter),
}

impl Input<&super::GenericParameter> for &GenericParameter {
    fn assert(self, output: &super::GenericParameter) -> TestCaseResult {
        match (self, output) {
            (GenericParameter::Lifetime(i), super::GenericParameter::Lifetime(o)) => i.assert(o),
            (GenericParameter::Type(i), super::GenericParameter::Type(o)) => i.assert(o),
            (GenericParameter::Const(i), super::GenericParameter::Constant(o)) => i.assert(o),
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
            ConstantParameter::arbitrary().prop_map(Self::Const),
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
    pub parameter_list: Option<ConnectedList<GenericParameter, ConstantPunctuation<','>>>,
}

impl Input<&super::GenericParameters> for &GenericParameters {
    fn assert(self, output: &super::GenericParameters) -> TestCaseResult {
        self.parameter_list
            .as_ref()
            .assert(output.parameter_list().as_ref())
    }
}

impl Arbitrary for GenericParameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            GenericParameter::arbitrary(),
            ConstantPunctuation::arbitrary(),
        ))
        .prop_map(|parameter_list| Self { parameter_list })
        .boxed()
    }
}

impl Display for GenericParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;

        if let Some(parameter_list) = &self.parameter_list {
            Display::fmt(parameter_list, f)?;
        }

        f.write_char(']')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetimeParameters {
    pub lifetime_parameter_list: Option<ConnectedList<LifetimeParameter, ConstantPunctuation<','>>>,
}

impl Input<&super::HigherRankedLifetimeParameters> for &HigherRankedLifetimeParameters {
    fn assert(self, output: &super::HigherRankedLifetimeParameters) -> TestCaseResult {
        self.lifetime_parameter_list
            .as_ref()
            .assert(output.lifetime_parameter_list().as_ref())
    }
}

impl Arbitrary for HigherRankedLifetimeParameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            LifetimeParameter::arbitrary(),
            ConstantPunctuation::arbitrary(),
        ))
        .prop_map(|lifetime_parameter_list| Self {
            lifetime_parameter_list,
        })
        .boxed()
    }
}

impl Display for HigherRankedLifetimeParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("for[")?;

        if let Some(lifetime_parameter_list) = &self.lifetime_parameter_list {
            Display::fmt(lifetime_parameter_list, f)?;
        }

        f.write_char(']')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitPredicate {
    pub higher_ranked_lifetime_parameters: Option<HigherRankedLifetimeParameters>,
    pub is_const: bool,
    pub qualified_identifiers: BoundList<QualifiedIdentifier>,
}

impl Input<&super::TraitPredicate> for &TraitPredicate {
    fn assert(self, output: &super::TraitPredicate) -> TestCaseResult {
        self.higher_ranked_lifetime_parameters
            .as_ref()
            .assert(output.higher_ranked_lifetime_parameters().as_ref())?;
        prop_assert_eq!(self.is_const, output.const_keyword.is_some());
        self.qualified_identifiers
            .assert(output.qualified_identifiers())
    }
}

impl Arbitrary for TraitPredicate {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimeParameters::arbitrary()),
            BoundList::arbitrary_with(QualifiedIdentifier::arbitrary()),
            proptest::bool::ANY,
        )
            .prop_map(
                |(higher_ranked_lifetime_parameters, qualified_identifiers, is_const)| Self {
                    higher_ranked_lifetime_parameters,
                    is_const,
                    qualified_identifiers,
                },
            )
            .boxed()
    }
}

impl Display for TraitPredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(higher_ranked_lifetime_parameters) = &self.higher_ranked_lifetime_parameters {
            write!(f, "{higher_ranked_lifetime_parameters} ")?;
        }

        if self.is_const {
            write!(f, "const ")?;
        }

        write!(f, "trait ")?;

        Display::fmt(&self.qualified_identifiers, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundList<T> {
    pub first: T,
    pub rest: Vec<T>,
}

impl<I: Debug, O: Debug> Input<&super::BoundList<O>> for &BoundList<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::BoundList<O>) -> TestCaseResult {
        self.first.assert(&output.first)?;

        prop_assert_eq!(self.rest.len(), output.rest.len());

        for (input, output) in self.rest.iter().zip(output.rest.iter()) {
            input.assert(&output.1)?;
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
pub enum LifetimePredicateOperand {
    LifetimeParameter(LifetimeParameter),
    Type(r#type::tests::Type),
}

impl Input<&super::LifetimePredicateOperand> for &LifetimePredicateOperand {
    fn assert(self, output: &super::LifetimePredicateOperand) -> TestCaseResult {
        match (self, output) {
            (
                LifetimePredicateOperand::LifetimeParameter(i),
                super::LifetimePredicateOperand::LifetimeParameter(o),
            ) => i.assert(o),
            (LifetimePredicateOperand::Type(i), super::LifetimePredicateOperand::Type(o)) => {
                i.assert(o)
            }
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for LifetimePredicateOperand {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            LifetimeParameter::arbitrary().prop_map(Self::LifetimeParameter),
            r#type::tests::Type::arbitrary().prop_map(Self::Type),
        ]
        .boxed()
    }
}

impl Display for LifetimePredicateOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LifetimeParameter(lifetime_parameter) => Display::fmt(lifetime_parameter, f),
            Self::Type(ty) => Display::fmt(ty, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimePredicate {
    pub operands: BoundList<LifetimePredicateOperand>,
    pub bounds: BoundList<Lifetime>,
}

impl Input<&super::LifetimePredicate> for &LifetimePredicate {
    fn assert(self, output: &super::LifetimePredicate) -> TestCaseResult {
        self.operands.assert(output.operands())?;
        self.bounds.assert(output.bounds())?;

        Ok(())
    }
}

impl Arbitrary for LifetimePredicate {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            BoundList::arbitrary_with(LifetimePredicateOperand::arbitrary()),
            BoundList::arbitrary_with(Lifetime::arbitrary()),
        )
            .prop_map(|(operands, bounds)| Self { operands, bounds })
            .boxed()
    }
}

impl Display for LifetimePredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.operands, self.bounds)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitMemberBound {
    Constant(Expression),
    Type(r#type::tests::Type),
}

impl Input<&super::TraitMemberBound> for &TraitMemberBound {
    fn assert(self, output: &super::TraitMemberBound) -> TestCaseResult {
        match (self, output) {
            (TraitMemberBound::Constant(i), super::TraitMemberBound::Constant(o)) => {
                i.assert(o.expression())
            }
            (TraitMemberBound::Type(i), super::TraitMemberBound::Type(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for TraitMemberBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Expression::arbitrary().prop_map(Self::Constant),
            r#type::tests::Type::arbitrary().prop_map(Self::Type),
        ]
        .boxed()
    }
}

impl Display for TraitMemberBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant(expression) => {
                write!(f, "{{{expression}}}")
            }
            Self::Type(ty) => Display::fmt(ty, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitMemberPredicate {
    pub qualified_identifier: QualifiedIdentifier,
    pub bound: TraitMemberBound,
}

impl Input<&super::TraitMemberPredicate> for &TraitMemberPredicate {
    fn assert(self, output: &super::TraitMemberPredicate) -> TestCaseResult {
        self.qualified_identifier
            .assert(output.qualified_identifier())?;
        self.bound.assert(output.bound())
    }
}

impl Arbitrary for TraitMemberPredicate {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            QualifiedIdentifier::arbitrary(),
            TraitMemberBound::arbitrary(),
        )
            .prop_map(|(qualified_identifier, argument)| Self {
                qualified_identifier,
                bound: argument,
            })
            .boxed()
    }
}

impl Display for TraitMemberPredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.qualified_identifier, self.bound)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Predicate {
    Trait(TraitPredicate),
    Lifetime(LifetimePredicate),
    TraitAssociation(TraitMemberPredicate),
}

impl Input<&super::Predicate> for &Predicate {
    fn assert(self, output: &super::Predicate) -> TestCaseResult {
        match (self, output) {
            (Predicate::Trait(i), super::Predicate::Trait(o)) => i.assert(o),
            (Predicate::Lifetime(i), super::Predicate::Lifetime(o)) => i.assert(o),
            (Predicate::TraitAssociation(i), super::Predicate::TraitMember(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for Predicate {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            TraitPredicate::arbitrary().prop_map(Self::Trait),
            LifetimePredicate::arbitrary().prop_map(Self::Lifetime),
            TraitMemberPredicate::arbitrary().prop_map(Self::TraitAssociation),
        ]
        .boxed()
    }
}

impl Display for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Trait(i) => Display::fmt(i, f),
            Self::Lifetime(i) => Display::fmt(i, f),
            Self::TraitAssociation(i) => Display::fmt(i, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhereClause {
    pub predicate_list: ConnectedList<Predicate, ConstantPunctuation<','>>,
}

impl Input<&super::WhereClause> for &WhereClause {
    fn assert(self, output: &super::WhereClause) -> TestCaseResult {
        self.predicate_list.assert(output.predicate_list())
    }
}

impl Arbitrary for WhereClause {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        ConnectedList::arbitrary_with(Predicate::arbitrary(), ConstantPunctuation::arbitrary())
            .prop_map(|constraint_list| Self {
                predicate_list: constraint_list,
            })
            .boxed()
    }
}

impl Display for WhereClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "where {}", self.predicate_list)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub irrefutable_pattern: Irrefutable,
    pub ty: r#type::tests::Type,
}

impl Input<&super::Parameter> for &Parameter {
    fn assert(self, output: &super::Parameter) -> TestCaseResult {
        self.irrefutable_pattern
            .assert(output.irrefutable_pattern())?;
        self.ty.assert(output.ty())
    }
}

impl Arbitrary for Parameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Irrefutable::arbitrary(), r#type::tests::Type::arbitrary())
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

impl Input<&super::Parameters> for &Parameters {
    fn assert(self, output: &super::Parameters) -> TestCaseResult {
        self.parameter_list
            .as_ref()
            .assert(output.parameter_list().as_ref())
    }
}

impl Arbitrary for Parameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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
    pub ty: r#type::tests::Type,
}

impl Input<&super::ReturnType> for &ReturnType {
    fn assert(self, output: &super::ReturnType) -> TestCaseResult { self.ty.assert(output.ty()) }
}

impl Arbitrary for ReturnType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        r#type::tests::Type::arbitrary()
            .prop_map(|ty| Self { ty })
            .boxed()
    }
}

impl Display for ReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { Display::fmt(&self.ty, f) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionSignature {
    identifier: Identifier,
    generic_parameters: Option<GenericParameters>,
    parameters: Parameters,
    return_type: Option<ReturnType>,
    where_clause: Option<WhereClause>,
}

impl Input<&super::FunctionSignature> for &FunctionSignature {
    fn assert(self, output: &super::FunctionSignature) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters().as_ref())?;
        self.parameters.assert(output.parameters())?;
        self.return_type
            .as_ref()
            .assert(output.return_type().as_ref())?;
        self.where_clause
            .as_ref()
            .assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for FunctionSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            Parameters::arbitrary(),
            proptest::option::of(ReturnType::arbitrary()),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(
                |(identifier, generic_parameters, parameters, return_type, where_clause)| Self {
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

impl Input<&super::FunctionBody> for &FunctionBody {
    fn assert(self, output: &super::FunctionBody) -> TestCaseResult {
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

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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
    pub is_const: bool,
    pub signature: FunctionSignature,
    pub body: FunctionBody,
}

impl Input<&super::Function> for &Function {
    fn assert(self, output: &super::Function) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        prop_assert_eq!(self.is_const, output.const_keyword().is_some());
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for Function {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            proptest::bool::ANY,
            FunctionSignature::arbitrary(),
            FunctionBody::arbitrary(),
        )
            .prop_map(|(access_modifier, is_const, signature, body)| Self {
                access_modifier,
                is_const,
                signature,
                body,
            })
            .boxed()
    }
}

impl Display for Function {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{} ", self.access_modifier)?;

        if self.is_const {
            write!(formatter, "const ")?;
        }

        write!(formatter, "{} {}", self.signature, self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantSignature {
    pub identifier: Identifier,
    pub ty: r#type::tests::Type,
}

impl Input<&super::ConstantSignature> for &ConstantSignature {
    fn assert(self, output: &super::ConstantSignature) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.ty.assert(output.ty())
    }
}

impl Arbitrary for ConstantSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Identifier::arbitrary(), r#type::tests::Type::arbitrary())
            .prop_map(|(identifier, ty)| Self { identifier, ty })
            .boxed()
    }
}

impl Display for ConstantSignature {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "const {}: {}", self.identifier, self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantDefinition {
    expression: Expression,
}

impl Input<&super::ConstantDefinition> for &ConstantDefinition {
    fn assert(self, output: &super::ConstantDefinition) -> TestCaseResult {
        self.expression.assert(output.expression())
    }
}

impl Arbitrary for ConstantDefinition {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Expression::arbitrary()
            .prop_map(|expression| Self { expression })
            .boxed()
    }
}

impl Display for ConstantDefinition {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, " = {};", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant {
    access_modifier: AccessModifier,
    signature: ConstantSignature,
    definition: ConstantDefinition,
}

impl Input<&super::Constant> for &Constant {
    fn assert(self, output: &super::Constant) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.definition.assert(output.definition())
    }
}

impl Arbitrary for Constant {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            ConstantSignature::arbitrary(),
            ConstantDefinition::arbitrary(),
        )
            .prop_map(|(access_modifier, signature, definition)| Self {
                access_modifier,
                signature,
                definition,
            })
            .boxed()
    }
}

impl Display for Constant {
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
    Implementation(Implementation),
    Constant(Constant),
}

impl Input<&super::Item> for &Item {
    fn assert(self, output: &super::Item) -> TestCaseResult {
        match (self, output) {
            (Item::Function(i), super::Item::Function(o)) => i.assert(o),
            (Item::Trait(i), super::Item::Trait(o)) => i.assert(o),
            (Item::Type(i), super::Item::Type(o)) => i.assert(o),
            (Item::Struct(i), super::Item::Struct(o)) => i.assert(o),
            (Item::Enum(i), super::Item::Enum(o)) => i.assert(o),
            (Item::Implementation(i), super::Item::Implementation(o)) => i.assert(o),
            (Item::Module(i), super::Item::Module(o)) => i.assert(o),
            (Item::Constant(i), super::Item::Constant(o)) => i.assert(o),
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
            Implementation::arbitrary().prop_map(Self::Implementation),
            Constant::arbitrary().prop_map(Self::Constant),
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
            Self::Implementation(i) => Display::fmt(i, formatter),
            Self::Module(m) => Display::fmt(m, formatter),
            Self::Constant(c) => Display::fmt(c, formatter),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitSignature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub where_clause: Option<WhereClause>,
}

impl Input<&super::TraitSignature> for &TraitSignature {
    fn assert(self, output: &super::TraitSignature) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters().as_ref())?;
        self.where_clause
            .as_ref()
            .assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for TraitSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Input<&super::TypeSignature> for &TypeSignature {
    fn assert(self, output: &super::TypeSignature) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters().as_ref())
    }
}

impl Arbitrary for TypeSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Input<&super::TraitFunction> for &TraitFunction {
    fn assert(self, output: &super::TraitFunction) -> TestCaseResult {
        self.function_signature.assert(output.signature())
    }
}

impl Arbitrary for TraitFunction {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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
    pub where_clause: Option<WhereClause>,
}

impl Input<&super::TraitType> for &TraitType {
    fn assert(self, output: &super::TraitType) -> TestCaseResult {
        self.type_signature.assert(output.signature())?;
        self.where_clause
            .as_ref()
            .assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for TraitType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            TypeSignature::arbitrary(),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(type_signature, where_clause)| Self {
                type_signature,
                where_clause,
            })
            .boxed()
    }
}

impl Display for TraitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_signature)?;

        if let Some(where_clause) = self.where_clause.as_ref() {
            write!(f, " {where_clause}")?;
        }

        write!(f, ";")?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitConstant {
    pub identifier: Identifier,
    pub ty: r#type::tests::Type,
}

impl Input<&super::TraitConstant> for &TraitConstant {
    fn assert(self, output: &super::TraitConstant) -> TestCaseResult {
        self.identifier.assert(output.signature().identifier())?;
        self.ty.assert(output.signature().ty())
    }
}

impl Arbitrary for TraitConstant {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Identifier::arbitrary(), r#type::tests::Type::arbitrary())
            .prop_map(|(identifier, ty)| Self { identifier, ty })
            .boxed()
    }
}

impl Display for TraitConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "const {}: {};", self.identifier, self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitMember {
    Function(TraitFunction),
    Type(TraitType),
    Constant(TraitConstant),
}

impl Input<&super::TraitMember> for &TraitMember {
    fn assert(self, output: &super::TraitMember) -> TestCaseResult {
        match (self, output) {
            (TraitMember::Function(f), super::TraitMember::Function(g)) => f.assert(g),
            (TraitMember::Type(f), super::TraitMember::Type(g)) => f.assert(g),
            (TraitMember::Constant(f), super::TraitMember::Constant(g)) => f.assert(g),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for TraitMember {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            TraitFunction::arbitrary().prop_map(Self::Function),
            TraitType::arbitrary().prop_map(Self::Type),
            TraitConstant::arbitrary().prop_map(Self::Constant),
        ]
        .boxed()
    }
}

impl Display for TraitMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(function) => Display::fmt(function, f),
            Self::Type(type_) => Display::fmt(type_, f),
            Self::Constant(constant) => Display::fmt(constant, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBody {
    pub members: Vec<TraitMember>,
}

impl Input<&super::TraitBody> for &TraitBody {
    fn assert(self, output: &super::TraitBody) -> TestCaseResult {
        self.members.assert(output.members())
    }
}

impl Arbitrary for TraitBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Input<&super::Trait> for &Trait {
    fn assert(self, output: &super::Trait) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for Trait {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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
    pub ty: r#type::tests::Type,
    pub where_clause: Option<WhereClause>,
}

impl Input<&super::TypeDefinition> for &TypeDefinition {
    fn assert(self, output: &super::TypeDefinition) -> TestCaseResult {
        self.ty.assert(output.ty())?;
        self.where_clause
            .as_ref()
            .assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for TypeDefinition {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            r#type::tests::Type::arbitrary(),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(ty, where_clause)| Self { ty, where_clause })
            .boxed()
    }
}

impl Display for TypeDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "= {}", self.ty)?;

        if let Some(where_clause) = &self.where_clause {
            write!(f, " {where_clause}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub access_modifier: AccessModifier,
    pub signature: TypeSignature,
    pub definition: TypeDefinition,
}

impl Input<&super::Type> for &Type {
    fn assert(self, output: &super::Type) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.definition.assert(output.definition())
    }
}

impl Arbitrary for Type {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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
    pub ty: r#type::tests::Type,
}

impl Input<&super::StructField> for &StructField {
    fn assert(self, output: &super::StructField) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.identifier.assert(output.identifier())?;
        self.ty.assert(output.ty())
    }
}

impl Arbitrary for StructField {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            Identifier::arbitrary(),
            r#type::tests::Type::arbitrary(),
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

impl Input<&super::StructMember> for &StructMember {
    fn assert(self, output: &super::StructMember) -> TestCaseResult {
        match (self, output) {
            (StructMember::Field(i), super::StructMember::Field(o)) => i.assert(o),
        }
    }
}

impl Arbitrary for StructMember {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Input<&super::StructBody> for &StructBody {
    fn assert(self, output: &super::StructBody) -> TestCaseResult {
        self.struct_member_list
            .as_ref()
            .assert(output.struct_member_list().as_ref())
    }
}

impl Arbitrary for StructBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Input<&super::StructSignature> for &StructSignature {
    fn assert(self, output: &super::StructSignature) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters().as_ref())?;
        self.where_clause
            .as_ref()
            .assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for StructSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Input<&super::Struct> for &Struct {
    fn assert(self, output: &super::Struct) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for Struct {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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
    pub where_clause: Option<WhereClause>,
}

impl Input<&super::EnumSignature> for &EnumSignature {
    fn assert(self, output: &super::EnumSignature) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters().as_ref())?;
        self.where_clause
            .as_ref()
            .assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for EnumSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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

impl Display for EnumSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "enum {}", self.identifier)?;

        if let Some(generic_parameters) = self.generic_parameters.as_ref() {
            Display::fmt(generic_parameters, f)?;
        }

        if let Some(where_clause) = self.where_clause.as_ref() {
            write!(f, " {where_clause}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    pub identifier: Identifier,
    pub association: Option<r#type::tests::Type>,
}

impl Input<&super::Variant> for &Variant {
    fn assert(self, output: &super::Variant) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        match (self.association.as_ref(), output.association().as_ref()) {
            (None, None) => Ok(()),
            (Some(expected), Some(output)) => expected.assert(&output.ty),
            (expected, output) => Err(TestCaseError::fail(format!(
                "expected associated value {expected:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Variant {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(r#type::tests::Type::arbitrary()),
        )
            .prop_map(|(identifier, variant_association)| Self {
                identifier,
                association: variant_association,
            })
            .boxed()
    }
}

impl Display for Variant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)?;

        if let Some(variant_association) = self.association.as_ref() {
            write!(f, "({variant_association})")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumBody {
    pub variant_list: Option<ConnectedList<Variant, ConstantPunctuation<','>>>,
}

impl Input<&super::EnumBody> for &EnumBody {
    fn assert(self, output: &super::EnumBody) -> TestCaseResult {
        self.variant_list
            .as_ref()
            .assert(output.variant_list().as_ref())
    }
}

impl Arbitrary for EnumBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            Variant::arbitrary(),
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
            Display::fmt(&variant_list, f)?;
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

impl Input<&super::Enum> for &Enum {
    fn assert(self, output: &super::Enum) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for Enum {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
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
pub struct ImplementationType {
    pub signature: TypeSignature,
    pub definition: TypeDefinition,
}

impl Input<&super::ImplementationType> for &ImplementationType {
    fn assert(self, output: &super::ImplementationType) -> TestCaseResult {
        self.signature.assert(output.signature())?;
        self.definition.assert(output.definition())
    }
}

impl Arbitrary for ImplementationType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (TypeSignature::arbitrary(), TypeDefinition::arbitrary())
            .prop_map(|(signature, definition)| Self {
                signature,
                definition,
            })
            .boxed()
    }
}

impl Display for ImplementationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.signature, self.definition)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationFunction {
    pub signature: FunctionSignature,
    pub body: FunctionBody,
}

impl Input<&super::ImplementationFunction> for &ImplementationFunction {
    fn assert(self, output: &super::ImplementationFunction) -> TestCaseResult {
        self.signature.assert(output.signature())?;
        self.body.assert(output.body())
    }
}

impl Arbitrary for ImplementationFunction {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (FunctionSignature::arbitrary(), FunctionBody::arbitrary())
            .prop_map(|(signature, body)| Self { signature, body })
            .boxed()
    }
}

impl Display for ImplementationFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.signature, self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationConstant {
    pub identifier: Identifier,
    pub ty: r#type::tests::Type,
    pub expression: expression::tests::Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplementationMember {
    Type(ImplementationType),
    Function(ImplementationFunction),
}

impl Input<&super::ImplementationMember> for &ImplementationMember {
    fn assert(self, output: &super::ImplementationMember) -> TestCaseResult {
        match (self, output) {
            (ImplementationMember::Type(input), super::ImplementationMember::Type(output)) => {
                input.assert(output)
            }
            (
                ImplementationMember::Function(input),
                super::ImplementationMember::Function(output),
            ) => input.assert(output),

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for ImplementationMember {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            ImplementationType::arbitrary().prop_map(Self::Type),
            ImplementationFunction::arbitrary().prop_map(Self::Function),
        ]
        .boxed()
    }
}

impl Display for ImplementationMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type(t) => Display::fmt(t, f),
            Self::Function(t) => Display::fmt(t, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationBody {
    pub members: Vec<ImplementationMember>,
}

impl Input<&super::ImplementationBody> for &ImplementationBody {
    fn assert(self, output: &super::ImplementationBody) -> TestCaseResult {
        self.members.assert(output.members())
    }
}

impl Arbitrary for ImplementationBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (proptest::collection::vec(ImplementationMember::arbitrary(), 0..=6))
            .prop_map(|members| Self { members })
            .boxed()
    }
}

impl Display for ImplementationBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{')?;
        for member in &self.members {
            Display::fmt(member, f)?;
        }
        f.write_char('}')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationSignature {
    pub generic_parameters: Option<GenericParameters>,
    pub is_const: bool,
    pub qualified_identifier: QualifiedIdentifier,
    pub where_clause: Option<WhereClause>,
}

impl Input<&super::ImplementationSignature> for &ImplementationSignature {
    fn assert(self, output: &super::ImplementationSignature) -> TestCaseResult {
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters().as_ref())?;
        self.qualified_identifier
            .assert(output.qualified_identifier())?;
        prop_assert_eq!(self.is_const, output.const_keyword.is_some());
        self.where_clause
            .as_ref()
            .assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for ImplementationSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(GenericParameters::arbitrary()),
            QualifiedIdentifier::arbitrary(),
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

impl Display for ImplementationSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "implements")?;

        if let Some(generic_parameters) = &self.generic_parameters {
            Display::fmt(generic_parameters, f)?;
        }

        if self.is_const {
            write!(f, " const")?;
        }

        write!(f, " {}", self.qualified_identifier)?;

        if let Some(where_clause) = self.where_clause.as_ref() {
            write!(f, " {where_clause}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplementationKind {
    Negative,
    Positive(ImplementationBody),
}

impl Input<&super::ImplementationKind> for &ImplementationKind {
    fn assert(self, output: &super::ImplementationKind) -> TestCaseResult {
        match (self, output) {
            (ImplementationKind::Negative, super::ImplementationKind::Negative(..)) => Ok(()),
            (ImplementationKind::Positive(input), super::ImplementationKind::Positive(output)) => {
                input.assert(output)
            }
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for ImplementationKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Negative),
            ImplementationBody::arbitrary().prop_map(Self::Positive),
        ]
        .boxed()
    }
}

impl Display for ImplementationKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Negative => write!(f, " delete;"),
            Self::Positive(body) => Display::fmt(body, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Implementation {
    pub signature: ImplementationSignature,
    pub kind: ImplementationKind,
}

impl Input<&super::Implementation> for &Implementation {
    fn assert(self, output: &super::Implementation) -> TestCaseResult {
        self.signature.assert(output.signature())?;
        self.kind.assert(output.kind())
    }
}

impl Arbitrary for Implementation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            ImplementationSignature::arbitrary(),
            ImplementationKind::arbitrary(),
        )
            .prop_map(|(signature, kind)| Self { signature, kind })
            .boxed()
    }
}

impl Display for Implementation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.signature, self.kind)
    }
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 819_200,
        ..proptest::test_runner::Config::default()
    })]
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls, clippy::ignored_unit_patterns)]
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
