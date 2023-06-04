use enum_as_inner::EnumAsInner;
use proptest::{prop_assert_eq, prop_oneof, strategy::Strategy, test_runner::TestCaseError};

use super::{
    Constraint, ConstraintList, Enum, EnumBody, EnumSignature, Function, FunctionBody,
    FunctionSignature, GenericParameter, GenericParameters, Implements, ImplementsBody,
    ImplementsFunction, ImplementsMember, ImplementsSignature, ImplementsType, Item, LifetimeBound,
    LifetimeParameter, Parameter, Parameters, ReturnType, Struct, StructBody, StructField,
    StructMember, StructSignature, Trait, TraitBody, TraitBound, TraitFunction, TraitMember,
    TraitSignature, TraitType, Type, TypeBound, TypeBoundConstraint, TypeDefinition, TypeParameter,
    TypeSignature, WhereClause,
};
use crate::syntax_tree::{
    statement::strategy::StatementInput,
    strategy::{
        qualified_identifier, AccessModifierInput, LifetimeArgumentInput, QualifiedIdentifierInput,
        TypeSpecifierInput,
    },
    ConnectedList,
};

/// Represents an input for [`super::LifetimeParameter`]
#[derive(Debug, Clone)]
pub struct LifetimeParameterInput {
    /// The identifier of the lifetime parameter
    pub identifier: String,
}

impl ToString for LifetimeParameterInput {
    fn to_string(&self) -> String { format!("'{}", self.identifier) }
}

impl LifetimeParameterInput {
    /// Validates the input against the [`super::LifetimeParameter`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &LifetimeParameter) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        Ok(())
    }
}

/// Represents an input for [`super::TypeParameter`]
#[derive(Debug, Clone)]
pub struct TypeParameterInput {
    /// The identifier of the type parameter
    pub identifier: String,
}

impl ToString for TypeParameterInput {
    fn to_string(&self) -> String { self.identifier.clone() }
}

impl TypeParameterInput {
    /// Validates the input against the [`super::TypeParameter`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TypeParameter) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        Ok(())
    }
}

/// Represents an input for [`super::GenericParameter`]
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum GenericParameterInput {
    Lifetime(LifetimeParameterInput),
    Type(TypeParameterInput),
}

impl ToString for GenericParameterInput {
    fn to_string(&self) -> String {
        match self {
            Self::Lifetime(lifetime) => lifetime.to_string(),
            Self::Type(ty) => ty.to_string(),
        }
    }
}

impl GenericParameterInput {
    /// Validates the input against the [`super::GenericParameter`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &GenericParameter) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Lifetime(input), GenericParameter::Lifetime(output)) => input.validate(output),
            (Self::Type(input), GenericParameter::Type(output)) => input.validate(output),
            _ => Err(TestCaseError::fail("Generic parameter mismatch")),
        }
    }
}

/// Represents an input for [`super::GenericParameters`]
#[derive(Debug, Clone)]
pub struct GenericParametersInput {
    /// List of generic parameters
    pub generic_parameters: Vec<GenericParameterInput>,
}

impl ToString for GenericParametersInput {
    fn to_string(&self) -> String {
        let string = self
            .generic_parameters
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");

        format!("<{string}>")
    }
}

impl GenericParametersInput {
    /// Validates the input against the [`super::GenericParameters`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &GenericParameters) -> Result<(), TestCaseError> {
        prop_assert_eq!(
            self.generic_parameters.len(),
            output.generic_parameter_list.len()
        );

        for (input, output) in self
            .generic_parameters
            .iter()
            .zip(output.generic_parameter_list.elements())
        {
            input.validate(output)?;
        }

        Ok(())
    }
}

/// Represents an input for [`super::TraitBound`]
#[derive(Debug, Clone)]
pub struct TraitBoundInput {
    pub qualified_identifier: QualifiedIdentifierInput,
}

impl ToString for TraitBoundInput {
    fn to_string(&self) -> String { self.qualified_identifier.to_string() }
}

impl TraitBoundInput {
    /// Validates the input against the [`super::TraitConstraint`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitBound) -> Result<(), TestCaseError> {
        self.qualified_identifier
            .validate(&output.qualified_identifier)
    }
}

/// Represents an input for [`super::LifetimeBound`]
#[derive(Debug, Clone)]
pub struct LifetimeBoundInput {
    pub lhs_lifetime_parameter: LifetimeParameterInput,
    pub rhs_lifetime_parameter: LifetimeParameterInput,
}

impl ToString for LifetimeBoundInput {
    fn to_string(&self) -> String {
        format!(
            "{}: {}",
            self.lhs_lifetime_parameter.to_string(),
            self.rhs_lifetime_parameter.to_string()
        )
    }
}

impl LifetimeBoundInput {
    /// Validates the input against the [`super::LifetimeBound`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &LifetimeBound) -> Result<(), TestCaseError> {
        self.lhs_lifetime_parameter
            .validate(&output.lhs_lifetime_parameter)?;
        self.rhs_lifetime_parameter
            .validate(&output.rhs_lifetime_parameter)?;

        Ok(())
    }
}

/// Represents an input for [`super::TypeBoundConstraint`]
#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeBoundConstraintInput {
    LifetimeArgument(LifetimeArgumentInput),
    TypeSpecifier(TypeSpecifierInput),
}

impl ToString for TypeBoundConstraintInput {
    fn to_string(&self) -> String {
        match self {
            Self::LifetimeArgument(lifetime_argument) => lifetime_argument.to_string(),
            Self::TypeSpecifier(type_specifier) => type_specifier.to_string(),
        }
    }
}

impl TypeBoundConstraintInput {
    /// Validates the input against the [`super::TypeBoundConstraint`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TypeBoundConstraint) -> Result<(), TestCaseError> {
        match (self, output) {
            (
                Self::LifetimeArgument(lifetime_argument),
                TypeBoundConstraint::LifetimeArgument(output),
            ) => lifetime_argument.validate(output),
            (Self::TypeSpecifier(type_specifier), TypeBoundConstraint::TypeSpecifier(output)) => {
                type_specifier.validate(output)
            }
            _ => Err(TestCaseError::fail("Type bound constraint mismatch")),
        }
    }
}

/// Represents an input for [`super::TypeBound`]
#[derive(Debug, Clone)]
pub struct TypeBoundInput {
    pub qualified_identifier: QualifiedIdentifierInput,
    pub type_bound_constraint: TypeBoundConstraintInput,
}

impl ToString for TypeBoundInput {
    fn to_string(&self) -> String {
        format!(
            "{}: {}",
            self.qualified_identifier.to_string(),
            self.type_bound_constraint.to_string()
        )
    }
}

impl TypeBoundInput {
    /// Validates the input against the [`super::TypeBound`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TypeBound) -> Result<(), TestCaseError> {
        self.qualified_identifier
            .validate(&output.qualified_identifier)?;
        self.type_bound_constraint
            .validate(&output.type_bound_constraint)?;

        Ok(())
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ConstraintInput {
    TraitBound(TraitBoundInput),
    LifetimeArgument(LifetimeArgumentInput),
    LifetimeBound(LifetimeBoundInput),
    TypeBound(TypeBoundInput),
}

impl ToString for ConstraintInput {
    fn to_string(&self) -> String {
        match self {
            Self::TraitBound(c) => c.to_string(),
            Self::LifetimeArgument(c) => c.to_string(),
            Self::LifetimeBound(c) => c.to_string(),
            Self::TypeBound(c) => c.to_string(),
        }
    }
}

impl ConstraintInput {
    /// Validates the input against the [`super::Constraint`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Constraint) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::TraitBound(i), Constraint::TraitBound(o)) => i.validate(o),
            (Self::LifetimeArgument(i), Constraint::LifetimeArgument(o)) => i.validate(o),
            (Self::LifetimeBound(i), Constraint::LifetimeBound(o)) => i.validate(o),
            (Self::TypeBound(i), Constraint::TypeBound(o)) => i.validate(o),
            _ => Err(TestCaseError::fail("Constraint mismatch")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstraintListInput {
    pub constraints: Vec<ConstraintInput>,
}

impl ToString for ConstraintListInput {
    fn to_string(&self) -> String {
        self.constraints
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    }
}

impl ConstraintListInput {
    /// Validates the input against the [`super::ConstraintList`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &ConstraintList) -> Result<(), TestCaseError> {
        prop_assert_eq!(self.constraints.len(), output.len());

        for (i, o) in self.constraints.iter().zip(output.elements()) {
            i.validate(o)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct WhereClauseInput {
    pub constraint_list: ConstraintListInput,
}

impl ToString for WhereClauseInput {
    fn to_string(&self) -> String { format!("where: {}", self.constraint_list.to_string()) }
}

impl WhereClauseInput {
    /// Validates the input against the [`super::WhereClause`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &WhereClause) -> Result<(), TestCaseError> {
        self.constraint_list.validate(&output.constraint_list)
    }
}

#[derive(Debug, Clone)]
pub struct TraitSignatureInput {
    pub identifier: String,
    pub generic_parameters: Option<GenericParametersInput>,
    pub where_clause: Option<WhereClauseInput>,
}

impl ToString for TraitSignatureInput {
    fn to_string(&self) -> String {
        let mut s = format!("trait {}", self.identifier);

        if let Some(generic_parameters) = &self.generic_parameters {
            s.push_str(&generic_parameters.to_string());
        }

        if let Some(where_clause) = &self.where_clause {
            s.push_str(&format!(" {}", where_clause.to_string()));
        }

        s
    }
}

impl TraitSignatureInput {
    /// Validates the input against the [`super::TraitSignature`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitSignature) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        match (&self.generic_parameters, &output.generic_parameters) {
            (Some(i), Some(o)) => i.validate(o)?,
            (None, None) => (),
            _ => return Err(TestCaseError::fail("generic parameters mismatch")),
        }
        match (&self.where_clause, &output.where_clause) {
            (Some(i), Some(o)) => {
                for (i, o) in i
                    .constraint_list
                    .constraints
                    .iter()
                    .zip(o.constraint_list.elements())
                {
                    i.validate(o)?;
                }
            }
            (None, None) => (),
            _ => return Err(TestCaseError::fail("where clause mismatch")),
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TraitBodyInput {
    pub trait_members: Vec<TraitMemberInput>,
}

impl ToString for TraitBodyInput {
    fn to_string(&self) -> String {
        let mut s = String::new();

        s.push('{');
        for member in &self.trait_members {
            s.push_str(&member.to_string());
        }
        s.push('}');

        s
    }
}

impl TraitBodyInput {
    /// Validates the input against the [`super::TraitBody`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitBody) -> Result<(), TestCaseError> {
        for (i, o) in self.trait_members.iter().zip(output.trait_members.iter()) {
            i.validate(o)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TraitFunctionInput {
    pub function_signature: FunctionSignatureInput,
}

impl ToString for TraitFunctionInput {
    fn to_string(&self) -> String { format!("{};", self.function_signature.to_string()) }
}

impl TraitFunctionInput {
    /// Validates the input against the [`super::TraitFunction`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitFunction) -> Result<(), TestCaseError> {
        self.function_signature
            .validate(&output.function_signature)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TraitTypeInput {
    pub type_signature: TypeSignatureInput,
    pub where_clause: Option<WhereClauseInput>,
}

impl ToString for TraitTypeInput {
    fn to_string(&self) -> String {
        let mut s = self.type_signature.to_string();

        if let Some(where_clause) = &self.where_clause {
            s.push_str(&format!(" {}", where_clause.to_string()));
        }

        s.push(';');

        s
    }
}

impl TraitTypeInput {
    /// Validates the input against the [`super::TraitType`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitType) -> Result<(), TestCaseError> {
        self.type_signature.validate(&output.type_signature)?;

        match (&self.where_clause, &output.where_clause) {
            (Some(i), Some(o)) => i.validate(o)?,
            (None, None) => (),
            _ => return Err(TestCaseError::fail("where clause mismatch")),
        }

        Ok(())
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TraitMemberInput {
    Function(TraitFunctionInput),
    Type(TraitTypeInput),
}

impl ToString for TraitMemberInput {
    fn to_string(&self) -> String {
        match self {
            Self::Function(f) => f.to_string(),
            Self::Type(t) => t.to_string(),
        }
    }
}

impl TraitMemberInput {
    /// Validates the input against the [`super::TraitMember`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitMember) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Function(i), TraitMember::Function(o)) => i.validate(o),
            (Self::Type(i), TraitMember::Type(o)) => i.validate(o),
            _ => Err(TestCaseError::fail("trait member mismatch")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParameterInput {
    pub mutable: bool,
    pub identifier: String,
    pub type_specifier: TypeSpecifierInput,
}

impl ToString for ParameterInput {
    fn to_string(&self) -> String {
        let mut s = String::new();
        if self.mutable {
            s.push_str("mutable ");
        }
        s.push_str(&self.identifier);
        s.push_str(": ");
        s.push_str(&self.type_specifier.to_string());
        s
    }
}

impl ParameterInput {
    /// Validates the input against the [`super::Parameter`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Parameter) -> Result<(), TestCaseError> {
        prop_assert_eq!(self.mutable, output.mutable_keyword.is_some());
        prop_assert_eq!(&self.identifier, output.identifier.span.str());
        self.type_specifier
            .validate(&output.type_annotation.type_specifier)
    }
}

#[derive(Debug, Clone)]
pub struct ParametersInput {
    pub parameters: Vec<ParameterInput>,
}

impl ToString for ParametersInput {
    fn to_string(&self) -> String {
        format!(
            "({})",
            self.parameters
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl ParametersInput {
    /// Validates the input against the [`super::Parameters`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Parameters) -> Result<(), TestCaseError> {
        prop_assert_eq!(
            self.parameters.len(),
            output.parameter_list.as_ref().map_or(0, ConnectedList::len)
        );

        for (input, output) in self.parameters.iter().zip(
            output
                .parameter_list
                .iter()
                .flat_map(ConnectedList::elements),
        ) {
            input.validate(output)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ReturnTypeInput {
    pub type_specifier: TypeSpecifierInput,
}

impl ToString for ReturnTypeInput {
    fn to_string(&self) -> String { format!(": {}", self.type_specifier.to_string()) }
}

impl ReturnTypeInput {
    /// Validates the input against the [`super::ReturnType`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &ReturnType) -> Result<(), TestCaseError> {
        self.type_specifier
            .validate(&output.type_annotation.type_specifier)
    }
}

#[derive(Debug, Clone)]
pub struct StructSignatureInput {
    pub identifier: String,
    pub generic_parameters: Option<GenericParametersInput>,
    pub where_clause: Option<WhereClauseInput>,
}

impl ToString for StructSignatureInput {
    fn to_string(&self) -> String {
        let mut s = "struct".to_string();
        s.push_str(&format!(" {}", self.identifier));

        if let Some(generic_parameters) = &self.generic_parameters {
            s.push_str(&generic_parameters.to_string());
        }

        if let Some(where_clause) = &self.where_clause {
            s.push_str(&format!(" {}", where_clause.to_string()));
        }

        s
    }
}

impl StructSignatureInput {
    /// Validates the input against the [`super::StructSignature`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &StructSignature) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        match (&self.generic_parameters, &output.generic_parameters) {
            (Some(i), Some(o)) => i.validate(o)?,
            (None, None) => (),
            _ => return Err(TestCaseError::fail("generic parameters mismatch")),
        }

        match (&self.where_clause, &output.where_clause) {
            (Some(i), Some(o)) => i.validate(o)?,
            (None, None) => (),
            _ => return Err(TestCaseError::fail("where clause mismatch")),
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct StructBodyInput {
    pub struct_members: Vec<StructMemberInput>,
}

impl ToString for StructBodyInput {
    fn to_string(&self) -> String {
        let mut s = "{".to_string();

        for member in &self.struct_members {
            s.push_str(&member.to_string());
        }

        s.push('}');

        s
    }
}

impl StructBodyInput {
    /// Validates the input against the [`super::StructBody`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &StructBody) -> Result<(), TestCaseError> {
        prop_assert_eq!(self.struct_members.len(), output.struct_members.len());

        for (i, o) in self.struct_members.iter().zip(&output.struct_members) {
            i.validate(o)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TypeDefinitionInput {
    pub type_specifier: TypeSpecifierInput,
}

impl ToString for TypeDefinitionInput {
    fn to_string(&self) -> String { format!("= {}", self.type_specifier.to_string()) }
}

impl TypeDefinitionInput {
    /// Validates the input against the [`super::TypeDefinition`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TypeDefinition) -> Result<(), TestCaseError> {
        self.type_specifier.validate(&output.type_specifier)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TypeSignatureInput {
    pub identifier: String,
    pub generic_parameters: Option<GenericParametersInput>,
}

impl ToString for TypeSignatureInput {
    fn to_string(&self) -> String {
        let mut s = "type ".to_string();

        s.push_str(&self.identifier);

        if let Some(generic_parameters) = &self.generic_parameters {
            s.push_str(&generic_parameters.to_string());
        }

        s
    }
}

impl TypeSignatureInput {
    /// Validates the input against the [`super::TypeSignature`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TypeSignature) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        match (&self.generic_parameters, &output.generic_parameters) {
            (None, None) => Ok(()),
            (Some(generic_parameters), Some(output_generic_parameters)) => {
                generic_parameters.validate(output_generic_parameters)
            }
            _ => Err(TestCaseError::fail("generic parameters mismatch")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructFieldInput {
    pub access_modifier: AccessModifierInput,
    pub identifier: String,
    pub type_specifier: TypeSpecifierInput,
}

impl ToString for StructFieldInput {
    fn to_string(&self) -> String {
        format!(
            "{} {}: {};",
            self.access_modifier.to_string(),
            self.identifier,
            self.type_specifier.to_string()
        )
    }
}

impl StructFieldInput {
    /// Validates the input against the [`super::StructField`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &StructField) -> Result<(), TestCaseError> {
        self.access_modifier.validate(&output.access_modifier)?;
        self.type_specifier
            .validate(&output.type_annotation.type_specifier)?;

        Ok(())
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum StructMemberInput {
    Field(StructFieldInput),
}

impl ToString for StructMemberInput {
    fn to_string(&self) -> String {
        match self {
            Self::Field(field) => field.to_string(),
        }
    }
}

impl StructMemberInput {
    /// Validates the input against the [`super::StructMember`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &StructMember) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Field(input), StructMember::Field(output)) => input.validate(output),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSignatureInput {
    pub identifier: String,
    pub generic_parameters: Option<GenericParametersInput>,
    pub parameters: ParametersInput,
    pub return_type: Option<ReturnTypeInput>,
    pub where_clause: Option<WhereClauseInput>,
}

impl ToString for FunctionSignatureInput {
    fn to_string(&self) -> String {
        let mut s = self.identifier.clone();

        if let Some(generic_parameters) = &self.generic_parameters {
            s.push_str(&generic_parameters.to_string());
        }

        s.push_str(&self.parameters.to_string());

        if let Some(return_type) = &self.return_type {
            s.push_str(&return_type.to_string());
        }

        if let Some(where_clause) = &self.where_clause {
            s.push_str(&format!(" {}", where_clause.to_string()));
        }

        s
    }
}

impl FunctionSignatureInput {
    /// Validates the input against the [`super::FunctionSignature`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &FunctionSignature) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        match (&self.generic_parameters, &output.generic_parameters) {
            (Some(i), Some(o)) => {
                i.validate(o)?;
            }
            (None, None) => {}
            _ => return Err(TestCaseError::fail("Generic parameters do not match")),
        }

        self.parameters.validate(&output.parameters)?;

        match (&self.return_type, &output.return_type) {
            (Some(i), Some(o)) => {
                i.validate(o)?;
            }
            (None, None) => {}
            _ => return Err(TestCaseError::fail("Return type do not match")),
        }

        match (&self.where_clause, &output.where_clause) {
            (Some(i), Some(o)) => {
                i.validate(o)?;
            }
            (None, None) => {}
            _ => return Err(TestCaseError::fail("Where clause do not match")),
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct TraitInput {
    pub access_modifier: AccessModifierInput,
    pub trait_signature: TraitSignatureInput,
    pub trait_body: TraitBodyInput,
}

impl ToString for TraitInput {
    fn to_string(&self) -> String {
        format!(
            "{} {} {}",
            self.access_modifier.to_string(),
            self.trait_signature.to_string(),
            self.trait_body.to_string()
        )
    }
}

impl TraitInput {
    /// Validates the input against the [`super::Trait`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Trait) -> Result<(), TestCaseError> {
        self.access_modifier.validate(&output.access_modifier)?;
        self.trait_signature.validate(&output.trait_signature)?;
        self.trait_body.validate(&output.trait_body)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionBodyInput {
    pub statements: Vec<StatementInput>,
}

impl ToString for FunctionBodyInput {
    fn to_string(&self) -> String {
        let mut string = String::new();
        string.push('{');

        for statement in &self.statements {
            string.push_str(&statement.to_string());
        }

        string.push('}');
        string
    }
}

impl FunctionBodyInput {
    /// Validates the input against the [`super::FunctionBody`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &FunctionBody) -> Result<(), TestCaseError> {
        prop_assert_eq!(self.statements.len(), output.statements.len());

        for (input, output) in self.statements.iter().zip(output.statements.iter()) {
            input.validate(output)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionInput {
    pub access_modifier: AccessModifierInput,
    pub function_signature: FunctionSignatureInput,
    pub function_body: FunctionBodyInput,
}

impl ToString for FunctionInput {
    fn to_string(&self) -> String {
        format!(
            "{} {} {}",
            self.access_modifier.to_string(),
            self.function_signature.to_string(),
            self.function_body.to_string()
        )
    }
}

impl FunctionInput {
    /// Validates the input against the [`super::Function`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Function) -> Result<(), TestCaseError> {
        self.access_modifier.validate(&output.access_modifier)?;
        self.function_signature
            .validate(&output.function_signature)?;
        self.function_body.validate(&output.function_body)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct StructInput {
    pub access_modifier: AccessModifierInput,
    pub struct_signature: StructSignatureInput,
    pub struct_body: StructBodyInput,
}

impl ToString for StructInput {
    fn to_string(&self) -> String {
        format!(
            "{} {} {}",
            self.access_modifier.to_string(),
            self.struct_signature.to_string(),
            self.struct_body.to_string()
        )
    }
}

impl StructInput {
    /// Validates the input against the [`super::Struct`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Struct) -> Result<(), TestCaseError> {
        self.access_modifier.validate(&output.access_modifier)?;
        self.struct_signature.validate(&output.struct_signature)?;
        self.struct_body.validate(&output.struct_body)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TypeInput {
    pub access_modifier: AccessModifierInput,
    pub type_signature: TypeSignatureInput,
    pub type_definition: TypeDefinitionInput,
}

impl ToString for TypeInput {
    fn to_string(&self) -> String {
        format!(
            "{} {} {};",
            self.access_modifier.to_string(),
            self.type_signature.to_string(),
            self.type_definition.to_string()
        )
    }
}

impl TypeInput {
    /// Validates the input against the [`super::Type`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Type) -> Result<(), TestCaseError> {
        self.access_modifier.validate(&output.access_modifier)?;
        self.type_signature.validate(&output.type_signature)?;
        self.type_definition.validate(&output.type_definition)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct EnumSignatureInput {
    pub identifier: String,
}

impl ToString for EnumSignatureInput {
    fn to_string(&self) -> String { format!("enum {}", self.identifier) }
}

impl EnumSignatureInput {
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &EnumSignature) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct EnumBodyInput {
    pub variants: Vec<String>,
}

impl ToString for EnumBodyInput {
    fn to_string(&self) -> String {
        let variants = self.variants.clone().join(", ");
        format!("{{ {variants} }}")
    }
}

impl EnumBodyInput {
    /// Validates the input against the [`super::EnumBody`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &EnumBody) -> Result<(), TestCaseError> {
        prop_assert_eq!(
            self.variants.len(),
            output
                .enum_variant_list
                .as_ref()
                .map_or(0, ConnectedList::len)
        );

        for (variant, output_variant) in self.variants.iter().zip(
            output
                .enum_variant_list
                .iter()
                .flat_map(ConnectedList::elements),
        ) {
            prop_assert_eq!(variant, output_variant.span.str());
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct EnumInput {
    pub access_modifier: AccessModifierInput,
    pub enum_signature: EnumSignatureInput,
    pub enum_body: EnumBodyInput,
}

impl ToString for EnumInput {
    fn to_string(&self) -> String {
        format!(
            "{access_modifier} {enum_signature} {enum_body}",
            access_modifier = self.access_modifier.to_string(),
            enum_signature = self.enum_signature.to_string(),
            enum_body = self.enum_body.to_string(),
        )
    }
}

impl EnumInput {
    /// Validates the input against the [`super::Enum`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Enum) -> Result<(), TestCaseError> {
        self.access_modifier.validate(&output.access_modifier)?;
        self.enum_signature.validate(&output.enum_signature)?;
        self.enum_body.validate(&output.enum_body)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ImplementsSignatureInput {
    pub generic_parameters: Option<GenericParametersInput>,
    pub qualified_identifier: QualifiedIdentifierInput,
}

impl ToString for ImplementsSignatureInput {
    fn to_string(&self) -> String {
        let mut string = "implements".to_string();

        if let Some(generic_parameters) = &self.generic_parameters {
            string.push_str(&generic_parameters.to_string());
        }

        string.push(' ');
        string.push_str(&self.qualified_identifier.to_string());

        string
    }
}

impl ImplementsSignatureInput {
    /// Validates the input against the [`super::ImplementsSignature`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &ImplementsSignature) -> Result<(), TestCaseError> {
        match (&self.generic_parameters, &output.generic_parameters) {
            (Some(generic_parameters), Some(output_generic_parameters)) => {
                generic_parameters.validate(output_generic_parameters)?;
            }
            (None, None) => (),
            _ => {
                return Err(TestCaseError::fail(
                    "Expected generic parameters to be either both present or both absent.",
                ))
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ImplementsFunctionInput {
    pub function_signature: FunctionSignatureInput,
    pub function_body: FunctionBodyInput,
}

impl ToString for ImplementsFunctionInput {
    fn to_string(&self) -> String {
        format!(
            "{} {}",
            self.function_signature.to_string(),
            self.function_body.to_string()
        )
    }
}

impl ImplementsFunctionInput {
    /// Validates the input against the [`super::ImplementsFunction`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &ImplementsFunction) -> Result<(), TestCaseError> {
        self.function_signature
            .validate(&output.function_signature)?;
        self.function_body.validate(&output.function_body)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ImplementsTypeInput {
    pub type_signature: TypeSignatureInput,
    pub type_definition: TypeDefinitionInput,
}

impl ToString for ImplementsTypeInput {
    fn to_string(&self) -> String {
        format!(
            "{} {};",
            self.type_signature.to_string(),
            self.type_definition.to_string()
        )
    }
}

impl ImplementsTypeInput {
    /// Validates the input against the [`super::ImplementsType`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &ImplementsType) -> Result<(), TestCaseError> {
        self.type_signature.validate(&output.type_signature)?;
        self.type_definition.validate(&output.type_definition)?;

        Ok(())
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ImplementsMemberInput {
    Function(ImplementsFunctionInput),
    Type(ImplementsTypeInput),
}

impl ToString for ImplementsMemberInput {
    fn to_string(&self) -> String {
        match self {
            Self::Function(i) => i.to_string(),
            Self::Type(i) => i.to_string(),
        }
    }
}

impl ImplementsMemberInput {
    /// Validates the input against the [`super::ImplementsMember`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &ImplementsMember) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Function(i), ImplementsMember::Function(o)) => i.validate(o)?,
            (Self::Type(i), ImplementsMember::Type(o)) => i.validate(o)?,
            _ => {
                return Err(TestCaseError::fail(format!(
                    "Expected input and output to be of the same variant, but got: {self:?} and \
                     {output:?}"
                )))
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ImplementsBodyInput {
    pub implements_members: Vec<ImplementsMemberInput>,
}

impl ToString for ImplementsBodyInput {
    fn to_string(&self) -> String {
        let mut string = "{\n".to_string();

        for member in &self.implements_members {
            string.push_str(&format!("    {}\n", member.to_string()));
        }

        string.push('}');

        string
    }
}

impl ImplementsBodyInput {
    /// Validates the input against the [`super::ImplementsBody`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &ImplementsBody) -> Result<(), TestCaseError> {
        prop_assert_eq!(
            self.implements_members.len(),
            output.implements_members.len()
        );

        for (input, output) in self
            .implements_members
            .iter()
            .zip(output.implements_members.iter())
        {
            input.validate(output)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ImplementsInput {
    pub implements_signature: ImplementsSignatureInput,
    pub implements_body: ImplementsBodyInput,
}

impl ToString for ImplementsInput {
    fn to_string(&self) -> String {
        format!(
            "{} {}",
            self.implements_signature.to_string(),
            self.implements_body.to_string()
        )
    }
}

impl ImplementsInput {
    /// Validates the input against the [`super::Implements`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Implements) -> Result<(), TestCaseError> {
        self.implements_signature
            .validate(&output.implements_signature)?;
        self.implements_body.validate(&output.implements_body)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ItemInput {
    Trait(TraitInput),
    Function(FunctionInput),
    Struct(StructInput),
    Type(TypeInput),
    Enum(EnumInput),
    Implements(ImplementsInput),
}

impl ToString for ItemInput {
    fn to_string(&self) -> String {
        match self {
            Self::Trait(i) => i.to_string(),
            Self::Function(i) => i.to_string(),
            Self::Struct(i) => i.to_string(),
            Self::Type(i) => i.to_string(),
            Self::Enum(i) => i.to_string(),
            Self::Implements(i) => i.to_string(),
        }
    }
}

impl ItemInput {
    /// Validates the input against the [`super::Item`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Item) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Trait(i), Item::Trait(o)) => i.validate(o),
            (Self::Function(i), Item::Function(o)) => i.validate(o),
            (Self::Struct(i), Item::Struct(o)) => i.validate(o),
            (Self::Type(i), Item::Type(o)) => i.validate(o),
            (Self::Enum(i), Item::Enum(o)) => i.validate(o),
            (Self::Implements(i), Item::Implements(o)) => i.validate(o),
            _ => Err(TestCaseError::fail("Item types do not match")),
        }
    }
}

fn generic_parameter() -> impl Strategy<Value = GenericParameterInput> {
    prop_oneof![
        pernixc_lexical::token::strategy::identifier()
            .prop_map(|identifier| GenericParameterInput::Type(TypeParameterInput { identifier })),
        pernixc_lexical::token::strategy::identifier().prop_map(|identifier| {
            GenericParameterInput::Lifetime(LifetimeParameterInput { identifier })
        })
    ]
}

fn generic_parameters() -> impl Strategy<Value = GenericParametersInput> {
    proptest::collection::vec(generic_parameter(), 1..=4)
        .prop_map(|generic_parameters| GenericParametersInput { generic_parameters })
}

fn where_clause() -> impl Strategy<Value = WhereClauseInput> {
    proptest::collection::vec(
        prop_oneof![
            qualified_identifier(false).prop_map(|qualified_identifier| {
                ConstraintInput::TraitBound(TraitBoundInput {
                    qualified_identifier,
                })
            }),
            crate::syntax_tree::strategy::lifetime_argument()
                .prop_map(ConstraintInput::LifetimeArgument),
            (qualified_identifier(false), prop_oneof![
                crate::syntax_tree::strategy::type_specifier()
                    .prop_map(TypeBoundConstraintInput::TypeSpecifier),
                crate::syntax_tree::strategy::lifetime_argument()
                    .prop_map(TypeBoundConstraintInput::LifetimeArgument)
            ])
                .prop_map(|(qualified_identifier, type_bound_constraint)| {
                    ConstraintInput::TypeBound(TypeBoundInput {
                        qualified_identifier,
                        type_bound_constraint,
                    })
                }),
            (
                pernixc_lexical::token::strategy::identifier(),
                pernixc_lexical::token::strategy::identifier(),
            )
                .prop_map(|(lhs, rhs)| {
                    ConstraintInput::LifetimeBound(LifetimeBoundInput {
                        lhs_lifetime_parameter: LifetimeParameterInput { identifier: lhs },
                        rhs_lifetime_parameter: LifetimeParameterInput { identifier: rhs },
                    })
                })
        ],
        1..=4,
    )
    .prop_map(|x| WhereClauseInput {
        constraint_list: ConstraintListInput { constraints: x },
    })
}

fn trait_signature() -> impl Strategy<Value = TraitSignatureInput> {
    (
        pernixc_lexical::token::strategy::identifier(),
        proptest::option::of(generic_parameters()),
        proptest::option::of(where_clause()),
    )
        .prop_map(
            |(identifier, generic_parameters, where_clause)| TraitSignatureInput {
                identifier,
                generic_parameters,
                where_clause,
            },
        )
}

fn parameter() -> impl Strategy<Value = ParameterInput> {
    (
        proptest::bool::ANY,
        pernixc_lexical::token::strategy::identifier(),
        crate::syntax_tree::strategy::type_specifier(),
    )
        .prop_map(|(mutable, identifier, type_specifier)| ParameterInput {
            mutable,
            identifier,
            type_specifier,
        })
}

fn return_type() -> impl Strategy<Value = ReturnTypeInput> {
    crate::syntax_tree::strategy::type_specifier()
        .prop_map(|type_specifier| ReturnTypeInput { type_specifier })
}

fn function_signature() -> impl Strategy<Value = FunctionSignatureInput> {
    (
        pernixc_lexical::token::strategy::identifier(),
        proptest::option::of(generic_parameters()),
        proptest::collection::vec(parameter(), 0..=4),
        proptest::option::of(return_type()),
        proptest::option::of(where_clause()),
    )
        .prop_map(
            |(identifier, generic_parameters, parameters, return_type, where_clause)| {
                FunctionSignatureInput {
                    identifier,
                    generic_parameters,
                    parameters: ParametersInput { parameters },
                    return_type,
                    where_clause,
                }
            },
        )
}

fn trait_member() -> impl Strategy<Value = TraitMemberInput> {
    prop_oneof![
        function_signature().prop_map(|function_signature| TraitMemberInput::Function(
            TraitFunctionInput { function_signature }
        )),
        (type_signature(), proptest::option::of(where_clause())).prop_map(
            |(type_signature, where_clause)| TraitMemberInput::Type(TraitTypeInput {
                type_signature,
                where_clause
            })
        )
    ]
}

fn implements_member() -> impl Strategy<Value = ImplementsMemberInput> {
    prop_oneof![
        (function_signature(), function_body()).prop_map(|(function_signature, function_body)| {
            ImplementsMemberInput::Function(ImplementsFunctionInput {
                function_signature,
                function_body,
            })
        },),
        (type_signature(), type_definition()).prop_map(|(type_signature, type_definition)| {
            ImplementsMemberInput::Type(ImplementsTypeInput {
                type_signature,
                type_definition,
            })
        })
    ]
}

fn implements_signature() -> impl Strategy<Value = ImplementsSignatureInput> {
    (
        proptest::option::of(generic_parameters()),
        qualified_identifier(false),
    )
        .prop_map(
            |(generic_parameters, qualified_identifier)| ImplementsSignatureInput {
                generic_parameters,
                qualified_identifier,
            },
        )
}

fn implements_body() -> impl Strategy<Value = ImplementsBodyInput> {
    proptest::collection::vec(implements_member(), 0..=4)
        .prop_map(|implements_members| ImplementsBodyInput { implements_members })
}

fn trait_body() -> impl Strategy<Value = TraitBodyInput> {
    proptest::collection::vec(trait_member(), 1..=4)
        .prop_map(|trait_members| TraitBodyInput { trait_members })
}

fn function_body() -> impl Strategy<Value = FunctionBodyInput> {
    proptest::collection::vec(crate::syntax_tree::statement::strategy::statement(), 0..=4)
        .prop_map(|statements| FunctionBodyInput { statements })
}

fn type_definition() -> impl Strategy<Value = TypeDefinitionInput> {
    (crate::syntax_tree::strategy::type_specifier())
        .prop_map(|type_specifier| TypeDefinitionInput { type_specifier })
}

fn type_signature() -> impl Strategy<Value = TypeSignatureInput> {
    (
        pernixc_lexical::token::strategy::identifier(),
        proptest::option::of(generic_parameters()),
    )
        .prop_map(|(identifier, generic_parameters)| TypeSignatureInput {
            identifier,
            generic_parameters,
        })
}

fn struct_field() -> impl Strategy<Value = StructFieldInput> {
    (
        crate::syntax_tree::strategy::access_modifier(),
        crate::syntax_tree::strategy::type_specifier(),
        pernixc_lexical::token::strategy::identifier(),
    )
        .prop_map(
            |(access_modifier, type_specifier, identifier)| StructFieldInput {
                access_modifier,
                identifier,
                type_specifier,
            },
        )
}

fn struct_member() -> impl Strategy<Value = StructMemberInput> {
    struct_field().prop_map(StructMemberInput::Field)
}

fn struct_signature() -> impl Strategy<Value = StructSignatureInput> {
    (
        pernixc_lexical::token::strategy::identifier(),
        proptest::option::of(generic_parameters()),
        proptest::option::of(where_clause()),
    )
        .prop_map(
            |(identifier, generic_parameters, where_clause)| StructSignatureInput {
                identifier,
                generic_parameters,
                where_clause,
            },
        )
}

fn struct_body() -> impl Strategy<Value = StructBodyInput> {
    proptest::collection::vec(struct_member(), 0..=4)
        .prop_map(|struct_members| StructBodyInput { struct_members })
}

fn enum_signature() -> impl Strategy<Value = EnumSignatureInput> {
    pernixc_lexical::token::strategy::identifier()
        .prop_map(|identifier| EnumSignatureInput { identifier })
}

fn enum_body() -> impl Strategy<Value = EnumBodyInput> {
    proptest::collection::vec(pernixc_lexical::token::strategy::identifier(), 0..=4)
        .prop_map(|variants| EnumBodyInput { variants })
}

pub fn item() -> impl Strategy<Value = ItemInput> {
    prop_oneof![
        (
            crate::syntax_tree::strategy::access_modifier(),
            trait_signature(),
            trait_body()
        )
            .prop_map(
                |(access_modifier, trait_signature, trait_body)| ItemInput::Trait(TraitInput {
                    access_modifier,
                    trait_signature,
                    trait_body,
                })
            ),
        (
            crate::syntax_tree::strategy::access_modifier(),
            function_signature(),
            function_body()
        )
            .prop_map(|(access_modifier, function_signature, function_body)| {
                ItemInput::Function(FunctionInput {
                    access_modifier,
                    function_signature,
                    function_body,
                })
            }),
        (
            crate::syntax_tree::strategy::access_modifier(),
            struct_signature(),
            struct_body()
        )
            .prop_map(|(access_modifier, struct_signature, struct_body)| {
                ItemInput::Struct(StructInput {
                    access_modifier,
                    struct_signature,
                    struct_body,
                })
            }),
        (
            crate::syntax_tree::strategy::access_modifier(),
            type_signature(),
            type_definition()
        )
            .prop_map(|(access_modifier, type_signature, type_definition)| {
                ItemInput::Type(TypeInput {
                    access_modifier,
                    type_signature,
                    type_definition,
                })
            }),
        (implements_signature(), implements_body()).prop_map(
            |(implements_signature, implements_body)| ItemInput::Implements(ImplementsInput {
                implements_signature,
                implements_body
            })
        ),
        (
            crate::syntax_tree::strategy::access_modifier(),
            enum_signature(),
            enum_body()
        )
            .prop_map(
                |(access_modifier, enum_signature, enum_body)| ItemInput::Enum(EnumInput {
                    access_modifier,
                    enum_signature,
                    enum_body,
                })
            ),
    ]
}
