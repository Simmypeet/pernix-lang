use std::fmt::{Debug, Display, Write};

use pernixc_test_input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    expression::strategy::Expression,
    pattern::strategy::Irrefutable,
    predicate::strategy::Predicate,
    r#type,
    statement::strategy::Statements,
    strategy::{
        AccessModifier, ConnectedList, ConstantPunctuation, Identifier,
        LifetimeParameter, QualifiedIdentifier, SimplePath,
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
        write!(f, "{} {}{}", self.access_modifier, self.signature, self.kind)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UsingOne {
    pub simple_path: SimplePath,
    pub alias: Option<Identifier>,
}

impl Input<&super::UsingOne> for &UsingOne {
    fn assert(self, output: &super::UsingOne) -> TestCaseResult {
        self.simple_path.assert(output.simple_path())?;
        self.alias
            .as_ref()
            .assert(output.alias().as_ref().map(|x| &x.identifier))
    }
}

impl Arbitrary for UsingOne {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (SimplePath::arbitrary(), proptest::option::of(Identifier::arbitrary()))
            .prop_map(|(simple_path, alias)| Self { simple_path, alias })
            .boxed()
    }
}

impl Display for UsingOne {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.simple_path)?;

        if let Some(alias) = &self.alias {
            write!(f, " as {alias}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Import {
    pub identifier: Identifier,
    pub alias: Option<Identifier>,
}

impl Input<&super::Import> for &Import {
    fn assert(self, output: &super::Import) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.alias
            .as_ref()
            .assert(output.alias().as_ref().map(|x| &x.identifier))
    }
}

impl Arbitrary for Import {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Identifier::arbitrary(), proptest::option::of(Identifier::arbitrary()))
            .prop_map(|(identifier, alias)| Self { identifier, alias })
            .boxed()
    }
}

impl Display for Import {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)?;

        if let Some(alias) = &self.alias {
            write!(f, " as {alias}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UsingFrom {
    pub imports: Option<ConnectedList<Import, ConstantPunctuation<','>>>,
    pub from: SimplePath,
}

impl Input<&super::UsingFrom> for &UsingFrom {
    fn assert(self, output: &super::UsingFrom) -> TestCaseResult {
        self.imports.as_ref().assert(output.imports.connected_list.as_ref())?;
        self.from.assert(&output.from().simple_path)
    }
}

impl Arbitrary for UsingFrom {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(ConnectedList::arbitrary_with(
                Import::arbitrary(),
                ConstantPunctuation::arbitrary(),
            )),
            SimplePath::arbitrary(),
        )
            .prop_map(|(imports, from)| Self { imports, from })
            .boxed()
    }
}

impl Display for UsingFrom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        if let Some(imports) = &self.imports {
            Display::fmt(imports, f)?;
        }
        write!(f, "}} from {}", self.from)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UsingKind {
    One(UsingOne),
    From(UsingFrom),
}

impl Input<&super::UsingKind> for &UsingKind {
    fn assert(self, output: &super::UsingKind) -> TestCaseResult {
        match (self, output) {
            (UsingKind::One(i), super::UsingKind::One(o)) => i.assert(o),
            (UsingKind::From(i), super::UsingKind::From(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for UsingKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            UsingOne::arbitrary().prop_map(Self::One),
            UsingFrom::arbitrary().prop_map(Self::From),
        ]
        .boxed()
    }
}

impl Display for UsingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::One(one) => Display::fmt(one, f),
            Self::From(from) => Display::fmt(from, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Using {
    pub kind: UsingKind,
}

impl Input<&super::Using> for &Using {
    fn assert(self, output: &super::Using) -> TestCaseResult {
        self.kind.assert(output.kind())
    }
}

impl Arbitrary for Using {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        UsingKind::arbitrary().prop_map(|kind| Self { kind }).boxed()
    }
}

impl Display for Using {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "using {};", self.kind)
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
                input.assert(&output.tree)
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
            (ModuleContent::arbitrary_with(Some(item_strategy)))
                .prop_map(Self::Inline),
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
pub struct ConstantParameter {
    pub identifier: Identifier,
    pub ty: r#type::strategy::Type,
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
            r#type::strategy::Type::arbitrary(),
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
    pub default: Option<r#type::strategy::Type>,
}

impl Input<&super::TypeParameter> for &TypeParameter {
    fn assert(self, output: &super::TypeParameter) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.default.as_ref().assert(output.default.as_ref().map(|x| &x.value))
    }
}

impl Arbitrary for TypeParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(r#type::strategy::Type::arbitrary()),
        )
            .prop_map(|(identifier, ty)| Self { identifier, default: ty })
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
            (
                GenericParameter::Lifetime(i),
                super::GenericParameter::Lifetime(o),
            ) => i.assert(o),
            (GenericParameter::Type(i), super::GenericParameter::Type(o)) => {
                i.assert(o)
            }
            (
                GenericParameter::Const(i),
                super::GenericParameter::Constant(o),
            ) => i.assert(o),
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
    pub parameter_list:
        Option<ConnectedList<GenericParameter, ConstantPunctuation<','>>>,
}

impl Input<&super::GenericParameters> for &GenericParameters {
    fn assert(self, output: &super::GenericParameters) -> TestCaseResult {
        self.parameter_list.as_ref().assert(output.connected_list.as_ref())
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
        ConnectedList::arbitrary_with(
            Predicate::arbitrary(),
            ConstantPunctuation::arbitrary(),
        )
        .prop_map(|constraint_list| Self { predicate_list: constraint_list })
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
    pub r#type: r#type::strategy::Type,
}

impl Input<&super::Parameter> for &Parameter {
    fn assert(self, output: &super::Parameter) -> TestCaseResult {
        self.irrefutable_pattern.assert(output.irrefutable_pattern())?;
        self.r#type.assert(output.r#type())
    }
}

impl Arbitrary for Parameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Irrefutable::arbitrary(), r#type::strategy::Type::arbitrary())
            .prop_map(|(irrefutable_pattern, ty)| Self {
                irrefutable_pattern,
                r#type: ty,
            })
            .boxed()
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.irrefutable_pattern, self.r#type)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParameterKind {
    Regular(Parameter),
    VarArgs,
}

impl Input<&super::ParameterKind> for &ParameterKind {
    fn assert(self, output: &super::ParameterKind) -> TestCaseResult {
        match (self, output) {
            (ParameterKind::Regular(i), super::ParameterKind::Regular(o)) => {
                i.assert(o)
            }
            (ParameterKind::VarArgs, super::ParameterKind::VarArgs(_)) => {
                Ok(())
            }

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for ParameterKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            6 => Parameter::arbitrary().prop_map(Self::Regular),
            1 => Just(Self::VarArgs),
        ]
        .boxed()
    }
}

impl Display for ParameterKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Regular(parameter) => Display::fmt(parameter, f),
            Self::VarArgs => write!(f, "..."),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameters {
    pub parameter_list:
        Option<ConnectedList<ParameterKind, ConstantPunctuation<','>>>,
}

impl Input<&super::Parameters> for &Parameters {
    fn assert(self, output: &super::Parameters) -> TestCaseResult {
        self.parameter_list.as_ref().assert(output.connected_list.as_ref())
    }
}

impl Arbitrary for Parameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            ParameterKind::arbitrary(),
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
    pub r#type: r#type::strategy::Type,
}

impl Input<&super::ReturnType> for &ReturnType {
    fn assert(self, output: &super::ReturnType) -> TestCaseResult {
        self.r#type.assert(output.r#type())
    }
}

impl Arbitrary for ReturnType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        r#type::strategy::Type::arbitrary()
            .prop_map(|ty| Self { r#type: ty })
            .boxed()
    }
}

impl Display for ReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.r#type, f)
    }
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
        self.return_type.as_ref().assert(output.return_type().as_ref())?;
        self.where_clause.as_ref().assert(output.where_clause().as_ref())
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
                |(
                    identifier,
                    generic_parameters,
                    parameters,
                    return_type,
                    where_clause,
                )| {
                    Self {
                        identifier,
                        generic_parameters,
                        parameters,
                        return_type,
                        where_clause,
                    }
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
pub struct Function {
    pub access_modifier: AccessModifier,
    pub is_const: bool,
    pub signature: FunctionSignature,
    pub statements: Statements,
}

impl Input<&super::Function> for &Function {
    fn assert(self, output: &super::Function) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        prop_assert_eq!(self.is_const, output.const_keyword().is_some());
        self.signature.assert(output.signature())?;
        self.statements.assert(output.statements())
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
            Statements::arbitrary(),
        )
            .prop_map(|(access_modifier, is_const, signature, statements)| {
                Self { access_modifier, is_const, signature, statements }
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

        write!(formatter, "{} {}", self.signature, self.statements)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantSignature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub r#type: r#type::strategy::Type,
}

impl Input<&super::ConstantSignature> for &ConstantSignature {
    fn assert(self, output: &super::ConstantSignature) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters.as_ref())?;
        self.r#type.assert(output.r#type())
    }
}

impl Arbitrary for ConstantSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericParameters::arbitrary()),
            r#type::strategy::Type::arbitrary(),
        )
            .prop_map(|(identifier, generic_parameters, r#type)| Self {
                identifier,
                generic_parameters,
                r#type,
            })
            .boxed()
    }
}

impl Display for ConstantSignature {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "const {}", self.identifier)?;

        if let Some(generic_parameters) = &self.generic_parameters {
            Display::fmt(generic_parameters, formatter)?;
        }

        write!(formatter, ": {}", self.r#type)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantDefinition {
    expression: Expression,
    where_clause: Option<WhereClause>,
}

impl Input<&super::ConstantDefinition> for &ConstantDefinition {
    fn assert(self, output: &super::ConstantDefinition) -> TestCaseResult {
        self.expression.assert(output.expression())?;
        self.where_clause.as_ref().assert(output.where_clause.as_ref())
    }
}

impl Arbitrary for ConstantDefinition {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Expression::arbitrary(),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(expression, where_clause)| Self {
                expression,
                where_clause,
            })
            .boxed()
    }
}

impl Display for ConstantDefinition {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, " = {}", self.expression)?;

        if let Some(where_clause) = &self.where_clause {
            write!(formatter, " {where_clause}")?;
        }

        write!(formatter, ";")
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
pub struct MarkerSignature {
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub where_clause: Option<WhereClause>,
}

impl Input<&super::MarkerSignature> for &MarkerSignature {
    fn assert(self, output: &super::MarkerSignature) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters().as_ref())?;
        self.where_clause.as_ref().assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for MarkerSignature {
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

impl Display for MarkerSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "marker {}", self.identifier)?;

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
pub struct Marker {
    pub access_modifier: AccessModifier,
    pub signature: MarkerSignature,
}

impl Input<&super::Marker> for &Marker {
    fn assert(self, output: &super::Marker) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())
    }
}

impl Arbitrary for Marker {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (AccessModifier::arbitrary(), MarkerSignature::arbitrary())
            .prop_map(|(access_modifier, signature)| Self {
                access_modifier,
                signature,
            })
            .boxed()
    }
}

impl Display for Marker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.access_modifier, self.signature)
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
    Marker(Marker),
}

impl Input<&super::Item> for &Item {
    fn assert(self, output: &super::Item) -> TestCaseResult {
        match (self, output) {
            (Item::Function(i), super::Item::Function(o)) => i.assert(o),
            (Item::Trait(i), super::Item::Trait(o)) => i.assert(o),
            (Item::Type(i), super::Item::Type(o)) => i.assert(o),
            (Item::Struct(i), super::Item::Struct(o)) => i.assert(o),
            (Item::Enum(i), super::Item::Enum(o)) => i.assert(o),
            (Item::Implementation(i), super::Item::Implementation(o)) => {
                i.assert(o)
            }
            (Item::Module(i), super::Item::Module(o)) => i.assert(o),
            (Item::Constant(i), super::Item::Constant(o)) => i.assert(o),
            (Item::Marker(i), super::Item::Marker(o)) => i.assert(o),
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
            Marker::arbitrary().prop_map(Self::Marker),
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
            Self::Marker(m) => Display::fmt(m, formatter),
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
        self.where_clause.as_ref().assert(output.where_clause().as_ref())
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
    pub access_modifier: AccessModifier,
    pub function_signature: FunctionSignature,
}

impl Input<&super::TraitFunction> for &TraitFunction {
    fn assert(self, output: &super::TraitFunction) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.function_signature.assert(output.signature())
    }
}

impl Arbitrary for TraitFunction {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (AccessModifier::arbitrary(), FunctionSignature::arbitrary())
            .prop_map(|(access_modifier, function_signature)| Self {
                access_modifier,
                function_signature,
            })
            .boxed()
    }
}

impl Display for TraitFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.access_modifier, self.function_signature)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitType {
    pub access_modifier: AccessModifier,
    pub type_signature: TypeSignature,
    pub where_clause: Option<WhereClause>,
}

impl Input<&super::TraitType> for &TraitType {
    fn assert(self, output: &super::TraitType) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.type_signature.assert(output.signature())?;
        self.where_clause.as_ref().assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for TraitType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            TypeSignature::arbitrary(),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(access_modifier, type_signature, where_clause)| Self {
                access_modifier,
                type_signature,
                where_clause,
            })
            .boxed()
    }
}

impl Display for TraitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.access_modifier, self.type_signature)?;

        if let Some(where_clause) = self.where_clause.as_ref() {
            write!(f, " {where_clause}")?;
        }

        write!(f, ";")?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitConstant {
    pub access_modifier: AccessModifier,
    pub signature: ConstantSignature,
    pub where_clause: Option<WhereClause>,
}

impl Input<&super::TraitConstant> for &TraitConstant {
    fn assert(self, output: &super::TraitConstant) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.signature.assert(output.signature())?;
        self.where_clause.as_ref().assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for TraitConstant {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            ConstantSignature::arbitrary(),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(access_modifier, signature, where_clause)| Self {
                access_modifier,
                signature,
                where_clause,
            })
            .boxed()
    }
}

impl Display for TraitConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.access_modifier, self.signature)?;

        if let Some(where_clause) = self.where_clause.as_ref() {
            write!(f, " {where_clause}")?;
        }

        write!(f, ";")?;

        Ok(())
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
            (TraitMember::Function(f), super::TraitMember::Function(g)) => {
                f.assert(g)
            }
            (TraitMember::Type(f), super::TraitMember::Type(g)) => f.assert(g),
            (TraitMember::Constant(f), super::TraitMember::Constant(g)) => {
                f.assert(g)
            }
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
        self.members.assert(&output.tree)
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
        write!(f, "{} {} {}", self.access_modifier, self.signature, self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefinition {
    pub r#type: r#type::strategy::Type,
    pub where_clause: Option<WhereClause>,
}

impl Input<&super::TypeDefinition> for &TypeDefinition {
    fn assert(self, output: &super::TypeDefinition) -> TestCaseResult {
        self.r#type.assert(output.r#type())?;
        self.where_clause.as_ref().assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for TypeDefinition {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            r#type::strategy::Type::arbitrary(),
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(|(ty, where_clause)| Self { r#type: ty, where_clause })
            .boxed()
    }
}

impl Display for TypeDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "= {}", self.r#type)?;

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
pub struct Field {
    pub access_modifier: AccessModifier,
    pub identifier: Identifier,
    pub ty: r#type::strategy::Type,
}

impl Input<&super::Field> for &Field {
    fn assert(self, output: &super::Field) -> TestCaseResult {
        self.access_modifier.assert(output.access_modifier())?;
        self.identifier.assert(output.identifier())?;
        self.ty.assert(output.r#type())
    }
}

impl Arbitrary for Field {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            AccessModifier::arbitrary(),
            Identifier::arbitrary(),
            r#type::strategy::Type::arbitrary(),
        )
            .prop_map(|(access_modifier, identifier, ty)| Self {
                access_modifier,
                identifier,
                ty,
            })
            .boxed()
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}: {}", self.access_modifier, self.identifier, self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructBody {
    field_list: Option<ConnectedList<Field, ConstantPunctuation<','>>>,
}

impl Input<&super::StructBody> for &StructBody {
    fn assert(self, output: &super::StructBody) -> TestCaseResult {
        self.field_list.as_ref().assert(output.connected_list.as_ref())
    }
}

impl Arbitrary for StructBody {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            Field::arbitrary(),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(|struct_member_list| Self { field_list: struct_member_list })
        .boxed()
    }
}

impl Display for StructBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{')?;

        if let Some(struct_member_list) = &self.field_list {
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
        self.where_clause.as_ref().assert(output.where_clause().as_ref())
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
        write!(f, "{} {} {}", self.access_modifier, self.signature, self.body)
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
        self.where_clause.as_ref().assert(output.where_clause().as_ref())
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
    pub association: Option<r#type::strategy::Type>,
}

impl Input<&super::Variant> for &Variant {
    fn assert(self, output: &super::Variant) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        match (self.association.as_ref(), output.association().as_ref()) {
            (None, None) => Ok(()),
            (Some(expected), Some(output)) => expected.assert(&output.tree),
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
            proptest::option::of(r#type::strategy::Type::arbitrary()),
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
        self.variant_list.as_ref().assert(output.connected_list().as_ref())
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
        write!(f, "{} {} {}", self.access_modifier, self.signature, self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplementationMember {
    Type(Type),
    Function(Function),
    Constant(Constant),
}

impl Input<&super::ImplementationMember> for &ImplementationMember {
    fn assert(self, output: &super::ImplementationMember) -> TestCaseResult {
        match (self, output) {
            (
                ImplementationMember::Type(i),
                super::ImplementationMember::Type(o),
            ) => i.assert(o),
            (
                ImplementationMember::Function(i),
                super::ImplementationMember::Function(o),
            ) => i.assert(o),
            (
                ImplementationMember::Constant(i),
                super::ImplementationMember::Constant(o),
            ) => i.assert(o),

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for ImplementationMember {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Type::arbitrary().prop_map(Self::Type),
            Function::arbitrary().prop_map(Self::Function),
            Constant::arbitrary().prop_map(Self::Constant),
        ]
        .boxed()
    }
}

impl Display for ImplementationMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type(t) => Display::fmt(t, f),
            Self::Function(t) => Display::fmt(t, f),
            Self::Constant(t) => Display::fmt(t, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationBody {
    pub members: Vec<ImplementationMember>,
}

impl Input<&super::ImplementationBody> for &ImplementationBody {
    fn assert(self, output: &super::ImplementationBody) -> TestCaseResult {
        self.members.assert(&output.tree)
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
    pub is_final: bool,
    pub generic_parameters: Option<GenericParameters>,
    pub is_const: bool,
    pub qualified_identifier: QualifiedIdentifier,
    pub where_clause: Option<WhereClause>,
}

impl Input<&super::ImplementationSignature> for &ImplementationSignature {
    fn assert(self, output: &super::ImplementationSignature) -> TestCaseResult {
        prop_assert_eq!(self.is_final, output.final_keyword.is_some());
        self.generic_parameters
            .as_ref()
            .assert(output.generic_parameters().as_ref())?;
        self.qualified_identifier.assert(output.qualified_identifier())?;
        prop_assert_eq!(self.is_const, output.const_keyword.is_some());
        self.where_clause.as_ref().assert(output.where_clause().as_ref())
    }
}

impl Arbitrary for ImplementationSignature {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            proptest::option::of(GenericParameters::arbitrary()),
            QualifiedIdentifier::arbitrary(),
            proptest::bool::ANY,
            proptest::option::of(WhereClause::arbitrary()),
        )
            .prop_map(
                |(
                    is_final,
                    generic_parameters,
                    qualified_identifier,
                    is_const,
                    where_clause,
                )| Self {
                    is_final,
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
        if self.is_final {
            write!(f, "final ")?;
        }

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
    Empty,
}

impl Input<&super::ImplementationKind> for &ImplementationKind {
    fn assert(self, output: &super::ImplementationKind) -> TestCaseResult {
        match (self, output) {
            (
                ImplementationKind::Negative,
                super::ImplementationKind::Negative(..),
            )
            | (
                ImplementationKind::Empty,
                super::ImplementationKind::Empty(_),
            ) => Ok(()),

            (
                ImplementationKind::Positive(input),
                super::ImplementationKind::Positive(output),
            ) => input.assert(output),

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
            Just(Self::Empty),
        ]
        .boxed()
    }
}

impl Display for ImplementationKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Negative => write!(f, " delete;"),
            Self::Positive(body) => Display::fmt(body, f),
            Self::Empty => write!(f, ";"),
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
        (ImplementationSignature::arbitrary(), ImplementationKind::arbitrary())
            .prop_map(|(signature, kind)| Self { signature, kind })
            .boxed()
    }
}

impl Display for Implementation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.signature, self.kind)
    }
}
