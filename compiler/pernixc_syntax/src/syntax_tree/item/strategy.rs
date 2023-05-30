use enum_as_inner::EnumAsInner;
use proptest::{prop_assert_eq, test_runner::TestCaseError};

use super::{
    AccessModifier, Constraint, ConstraintList, FunctionSignature, GenericParameter,
    GenericParameters, LifetimeParameter, Parameter, Parameters, ReturnType, Trait, TraitBody,
    TraitConstraint, TraitFunction, TraitMember, TraitSignature, TypeParameter, WhereClause,
};
use crate::syntax_tree::{
    strategy::{LifetimeArgumentInput, QualifiedIdentifierInput, TypeSpecifierInput},
    ConnectedList,
};

#[derive(Debug, Clone)]
pub struct LifetimeParameterInput {
    pub identifier: String,
}

impl ToString for LifetimeParameterInput {
    fn to_string(&self) -> String { format!("'{}", self.identifier) }
}

impl LifetimeParameterInput {
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &LifetimeParameter) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TypeParameterInput {
    pub identifier: String,
}

impl ToString for TypeParameterInput {
    fn to_string(&self) -> String { self.identifier.clone() }
}

impl TypeParameterInput {
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TypeParameter) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        Ok(())
    }
}

#[derive(Debug, Clone, EnumAsInner)]
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
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &GenericParameter) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Lifetime(input), GenericParameter::Lifetime(output)) => input.validate(output),
            (Self::Type(input), GenericParameter::Type(output)) => input.validate(output),
            _ => Err(TestCaseError::fail("Generic parameter mismatch")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenericParametersInput {
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

#[derive(Debug, Clone)]
pub struct TraitConstraintInput {
    pub qualified_identifier: QualifiedIdentifierInput,
}

impl ToString for TraitConstraintInput {
    fn to_string(&self) -> String { self.qualified_identifier.to_string() }
}

impl TraitConstraintInput {
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitConstraint) -> Result<(), TestCaseError> {
        self.qualified_identifier
            .validate(&output.qualified_identifier)
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ConstraintInput {
    TraitConstraint(TraitConstraintInput),
    LifetimeArgument(LifetimeArgumentInput),
}

impl ToString for ConstraintInput {
    fn to_string(&self) -> String {
        match self {
            Self::TraitConstraint(c) => c.to_string(),
            Self::LifetimeArgument(c) => c.to_string(),
        }
    }
}

impl ConstraintInput {
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Constraint) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::TraitConstraint(i), Constraint::TraitConstraint(o)) => i.validate(o),
            (Self::LifetimeArgument(i), Constraint::LifetimeArgument(o)) => i.validate(o),
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
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &WhereClause) -> Result<(), TestCaseError> {
        self.constraint_list.validate(&output.constraint_list)
    }
}

#[derive(Debug, Clone)]
pub struct TraitSignatureInput {
    pub identifier: String,
    pub generic_parameters: GenericParametersInput,
    pub where_clause: Option<WhereClauseInput>,
}

impl ToString for TraitSignatureInput {
    fn to_string(&self) -> String {
        let mut s = format!("trait {}", self.identifier);

        s.push_str(&self.generic_parameters.to_string());

        if let Some(where_clause) = &self.where_clause {
            s.push_str(&format!(" {}", where_clause.to_string()));
        }

        s
    }
}

impl TraitSignatureInput {
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitSignature) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        self.generic_parameters
            .validate(&output.generic_parameters)?;

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
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitFunction) -> Result<(), TestCaseError> {
        self.function_signature
            .validate(&output.function_signature)?;

        Ok(())
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TraitMemberInput {
    Function(TraitFunctionInput),
}

impl ToString for TraitMemberInput {
    fn to_string(&self) -> String {
        match self {
            Self::Function(f) => f.to_string(),
        }
    }
}

impl TraitMemberInput {
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitMember) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Function(i), TraitMember::Function(o)) => i.validate(o),
        }
    }
}

#[derive(Debug, Clone, Copy, EnumAsInner)]
pub enum AccessModifierInput {
    Public,
    Private,
    Internal,
}

impl ToString for AccessModifierInput {
    fn to_string(&self) -> String {
        match self {
            Self::Public => "public".to_string(),
            Self::Private => "private".to_string(),
            Self::Internal => "internal".to_string(),
        }
    }
}

impl AccessModifierInput {
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &AccessModifier) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Public, AccessModifier::Public(_))
            | (Self::Private, AccessModifier::Private(_))
            | (Self::Internal, AccessModifier::Internal(_)) => Ok(()),
            _ => Err(TestCaseError::fail("access modifier mismatch")),
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
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &ReturnType) -> Result<(), TestCaseError> {
        self.type_specifier
            .validate(&output.type_annotation.type_specifier)
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
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &Trait) -> Result<(), TestCaseError> {
        self.access_modifier.validate(&output.access_modifier)?;
        self.trait_signature.validate(&output.trait_signature)?;
        self.trait_body.validate(&output.trait_body)?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ItemInput {
    Trait(TraitInput),
}
