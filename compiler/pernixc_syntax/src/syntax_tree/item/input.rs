//! Contains the definition of various inputs that correspond to the definitions in defined
//! [`crate::syntax_tree::item`] module.

use std::fmt::{Display, Write};

use pernixc_lexical::token::input::Identifier;
use pernixc_system::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    input::{
        AccessModifier, ConnectedList, ConstantPunctuation, LifetimeArgument, QualifiedIdentifier,
        QualifiedIdentifierArbitraryParameters, TypeAnnotation, TypeSpecifier,
    },
    statement::input::Statement,
};

/// Represents an input for the [`super::LifetimeParameter`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameter {
    /// The identifier of the lifetime parameter.
    pub identifier: Identifier,
}

impl Input for LifetimeParameter {
    type Output = super::LifetimeParameter;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(&output.identifier)
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

/// Represents an input for the [`super::TypeParameter`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameter {
    /// The identifier of the type parameter.
    pub identifier: Identifier,
}

impl Input for TypeParameter {
    type Output = super::TypeParameter;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(&output.identifier)
    }
}

impl Arbitrary for TypeParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Display for TypeParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.identifier, f)
    }
}

/// Represents an input for the [`super::GenericParameter`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum GenericParameter {
    Lifetime(LifetimeParameter),
    Type(TypeParameter),
}

impl Input for GenericParameter {
    type Output = super::GenericParameter;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Lifetime(i), super::GenericParameter::Lifetime(o)) => i.assert(o),
            (Self::Type(i), super::GenericParameter::Type(o)) => i.assert(o),
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
        ]
        .boxed()
    }
}

impl Display for GenericParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lifetime(l) => Display::fmt(l, f),
            Self::Type(t) => Display::fmt(t, f),
        }
    }
}

/// Represents an input for the [`super::GenericParameters`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParameters {
    /// The parameters of the generic.
    pub parameter_list: ConnectedList<GenericParameter, ConstantPunctuation<','>>,
}

impl Input for GenericParameters {
    type Output = super::GenericParameters;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.parameter_list.assert(&output.parameter_list)
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

/// Represents an input for the [`super::TraitBound`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBound {
    /// The qualified identifier of the trait bound.
    pub qualified_identifier: QualifiedIdentifier,
}

impl Input for TraitBound {
    type Output = super::TraitBound;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.qualified_identifier
            .assert(&output.qualified_identifier)
    }
}

impl Arbitrary for TraitBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        QualifiedIdentifier::arbitrary_with(QualifiedIdentifierArbitraryParameters {
            use_turbofish: false,
            inner_type_specifier_strategy: None,
        })
        .prop_map(|qualified_identifier| Self {
            qualified_identifier,
        })
        .boxed()
    }
}

impl Display for TraitBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.qualified_identifier, f)
    }
}

/// Represents an input for the [`super::BoundList`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundList<T> {
    /// The first bound.
    pub first: T,

    /// The rest of the bounds.
    pub rest: Vec<T>,
}

impl<T: Input> Input for BoundList<T> {
    type Output = super::BoundList<T::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.first.assert(&output.first)?;

        prop_assert_eq!(self.rest.len(), output.rest.len());

        for (input, (_, output)) in self.rest.iter().zip(output.rest.iter()) {
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
        (args.clone(), proptest::collection::vec(args, 0..=7))
            .prop_map(|(first, rest)| Self { first, rest })
            .boxed()
    }
}

/// Represents an input for the [`super::LifetimeBound`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeBound {
    /// Lifetime parameter used in the bound.
    pub operand: LifetimeParameter,

    /// The lifetime bounds.
    pub parameters: BoundList<LifetimeArgument>,
}

impl Input for LifetimeBound {
    type Output = super::LifetimeBound;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operand.assert(&output.operand)?;
        self.parameters.assert(&output.parameters)?;

        Ok(())
    }
}

impl Arbitrary for LifetimeBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            LifetimeParameter::arbitrary(),
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

/// Represents an input for the [`super::TypeBoundConstraint`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TypeBoundConstraint {
    TypeSpecifier(TypeSpecifier),
    LifetimeArgument(LifetimeArgument),
}

impl Input for TypeBoundConstraint {
    type Output = super::TypeBoundConstraint;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::TypeSpecifier(a), super::TypeBoundConstraint::TypeSpecifier(b)) => a.assert(b),
            (Self::LifetimeArgument(a), super::TypeBoundConstraint::LifetimeArgument(b)) => {
                a.assert(b)
            }
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for TypeBoundConstraint {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            TypeSpecifier::arbitrary().prop_map(Self::TypeSpecifier),
            LifetimeArgument::arbitrary().prop_map(Self::LifetimeArgument),
        ]
        .boxed()
    }
}

impl Display for TypeBoundConstraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeSpecifier(a) => a.fmt(f),
            Self::LifetimeArgument(a) => a.fmt(f),
        }
    }
}

/// Represents an input for the [`super::TypeBound`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeBound {
    /// The qualified identifier of the type bound.
    pub qualified_identifier: QualifiedIdentifier,

    /// The type bound constraints.
    pub type_bound_constraints: BoundList<TypeBoundConstraint>,
}

impl Input for TypeBound {
    type Output = super::TypeBound;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.qualified_identifier
            .assert(&output.qualified_identifier)?;
        self.type_bound_constraints
            .assert(&output.type_bound_constraints)
    }
}

impl Arbitrary for TypeBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            QualifiedIdentifier::arbitrary_with(QualifiedIdentifierArbitraryParameters {
                use_turbofish: false,
                inner_type_specifier_strategy: None,
            }),
            BoundList::arbitrary_with(TypeBoundConstraint::arbitrary()),
        )
            .prop_map(|(qualified_identifier, type_bound_constraints)| Self {
                qualified_identifier,
                type_bound_constraints,
            })
            .boxed()
    }
}

impl Display for TypeBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {}",
            self.qualified_identifier, self.type_bound_constraints
        )
    }
}

/// Represents an input for the [`super::Constraint`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Constraint {
    TraitBound(TraitBound),
    LifetimeBound(LifetimeBound),
    TypeBound(TypeBound),
}

impl Input for Constraint {
    type Output = super::Constraint;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::TraitBound(i), super::Constraint::TraitBound(o)) => i.assert(o),
            (Self::LifetimeBound(i), super::Constraint::LifetimeBound(o)) => i.assert(o),
            (Self::TypeBound(i), super::Constraint::TypeBound(o)) => i.assert(o),
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
        prop_oneof![
            TraitBound::arbitrary().prop_map(Self::TraitBound),
            LifetimeBound::arbitrary().prop_map(Self::LifetimeBound),
            TypeBound::arbitrary().prop_map(Self::TypeBound),
        ]
        .boxed()
    }
}

impl Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TraitBound(i) => Display::fmt(i, f),
            Self::LifetimeBound(i) => Display::fmt(i, f),
            Self::TypeBound(i) => Display::fmt(i, f),
        }
    }
}

/// Represents an input for the [`super::WhereClause`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhereClause {
    /// The list of constrains in the where clause.
    pub constraint_list: ConnectedList<Constraint, ConstantPunctuation<','>>,
}

impl Input for WhereClause {
    type Output = super::WhereClause;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.constraint_list.assert(&output.constraint_list)
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

/// Represents an input for the [`super::Parameter`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    /// Whether the parameter is mutable.
    pub mutable: bool,

    /// The identifier of the parameter.
    pub identifier: Identifier,

    /// The type annotation of the parameter.
    pub type_annotation: TypeAnnotation,
}

impl Input for Parameter {
    type Output = super::Parameter;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.mutable, output.mutable_keyword.is_some());
        self.identifier.assert(&output.identifier)?;
        self.type_annotation.assert(&output.type_annotation)
    }
}

impl Arbitrary for Parameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            Identifier::arbitrary(),
            TypeAnnotation::arbitrary(),
        )
            .prop_map(|(mutable, identifier, type_annotation)| Self {
                mutable,
                identifier,
                type_annotation,
            })
            .boxed()
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.mutable {
            write!(f, "mutable ")?;
        }
        write!(f, "{}{}", self.identifier, self.type_annotation)
    }
}

/// Represents an input for the [`super::Parameters`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameters {
    /// The list of parameters.
    pub parameter_list: Option<ConnectedList<Parameter, ConstantPunctuation<','>>>,
}

impl Input for Parameters {
    type Output = super::Parameters;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.parameter_list.assert(&output.parameter_list)
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

/// Represents an input for the [`super::ReturnType`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnType {
    /// The type annotation of the return type.
    pub type_annotation: TypeAnnotation,
}

impl Input for ReturnType {
    type Output = super::ReturnType;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.type_annotation.assert(&output.type_annotation)
    }
}

impl Arbitrary for ReturnType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        TypeAnnotation::arbitrary()
            .prop_map(|type_annotation| Self { type_annotation })
            .boxed()
    }
}

impl Display for ReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.type_annotation, f)
    }
}

/// Represents an input for the [`super::FunctionSignature`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionSignature {
    /// The name of the function.
    identifier: Identifier,

    /// The generic parameters of the function.
    generic_parameters: Option<GenericParameters>,

    /// The parameters of the function.
    parameters: Parameters,

    /// The return type of the function.
    return_type: Option<ReturnType>,

    /// The where clause of the function.
    where_clause: Option<WhereClause>,
}

impl Input for FunctionSignature {
    type Output = super::FunctionSignature;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.generic_parameters.assert(&output.generic_parameters)?;
        self.parameters.assert(&output.parameters)?;
        self.return_type.assert(&output.return_type)?;
        self.where_clause.assert(&output.where_clause)
    }
}

impl Arbitrary for FunctionSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
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
        Display::fmt(&self.identifier, formatter)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            Display::fmt(generic_parameters, formatter)?;
        }

        Display::fmt(&self.parameters, formatter)?;

        if let Some(return_type) = &self.return_type {
            Display::fmt(return_type, formatter)?;
        }

        if let Some(where_clause) = &self.where_clause {
            write!(formatter, " {where_clause}")?;
        }

        Ok(())
    }
}

/// Represents an input for the [`super::FunctionBody`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionBody {
    /// The statements of the function body.
    pub statements: Vec<Statement>,
}

impl Input for FunctionBody {
    type Output = super::FunctionBody;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.statements.len(), output.statements.len());

        for (left, right) in self.statements.iter().zip(output.statements.iter()) {
            left.assert(right)?;
        }

        Ok(())
    }
}

impl Arbitrary for FunctionBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(Statement::arbitrary(), 0..=8)
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

/// Represents an input for the [`super::Function`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    /// The access modifier of the function.
    pub access_modifier: AccessModifier,

    /// The signature of the function.
    pub signature: FunctionSignature,

    /// The body of the function.
    pub body: FunctionBody,
}

impl Input for Function {
    type Output = super::Function;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        self.signature.assert(&output.signature)?;
        self.body.assert(&output.body)
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

/// Represents an input for the [`super::Item`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Item {
    Function(Function),
}

impl Input for Item {
    type Output = super::Item;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Function(i), super::Item::Function(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Item {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![Function::arbitrary().prop_map(Self::Function),].boxed()
    }
}

impl Display for Item {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(f) => Display::fmt(f, formatter),
        }
    }
}
