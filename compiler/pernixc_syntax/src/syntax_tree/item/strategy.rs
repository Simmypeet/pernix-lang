use enum_as_inner::EnumAsInner;
use proptest::{prop_assert_eq, prop_oneof, strategy::Strategy, test_runner::TestCaseError};

use super::{
    Constraint, ConstraintList, Enum, EnumBody, EnumSignature, Function, FunctionBody,
    FunctionSignature, GenericParameter, GenericParameters, Item, LifetimeParameter, Parameter,
    Parameters, ReturnType, Struct, StructBody, StructField, StructMember, StructSignature,
    StructType, Trait, TraitBody, TraitConstraint, TraitFunction, TraitMember, TraitSignature,
    Type, TypeDefinition, TypeParameter, TypeSignature, WhereClause,
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

/// Represents an input for [`super::TraitConstraint`]
#[derive(Debug, Clone)]
pub struct TraitConstraintInput {
    pub qualified_identifier: QualifiedIdentifierInput,
}

impl ToString for TraitConstraintInput {
    fn to_string(&self) -> String { self.qualified_identifier.to_string() }
}

impl TraitConstraintInput {
    /// Validates the input against the [`super::TraitConstraint`] output.
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
    /// Validates the input against the [`super::Constraint`] output.
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
    /// Validates the input against the [`super::TraitMember`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &TraitMember) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Function(i), TraitMember::Function(o)) => i.validate(o),
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
            "{} let {}: {};",
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

#[derive(Debug, Clone)]
pub struct StructTypeInput {
    pub access_modifier: AccessModifierInput,
    pub type_signature: TypeSignatureInput,
    pub type_definition: TypeDefinitionInput,
}

impl ToString for StructTypeInput {
    fn to_string(&self) -> String {
        format!(
            "{} {} {};",
            self.access_modifier.to_string(),
            self.type_signature.to_string(),
            self.type_definition.to_string()
        )
    }
}

impl StructTypeInput {
    /// Validates the input against the [`super::StructType`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &StructType) -> Result<(), TestCaseError> {
        self.access_modifier.validate(&output.access_modifier)?;
        self.type_signature.validate(&output.type_signature)?;
        self.type_definition.validate(&output.type_definition)?;

        Ok(())
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum StructMemberInput {
    Field(StructFieldInput),
    Type(StructTypeInput),
}

impl ToString for StructMemberInput {
    fn to_string(&self) -> String {
        match self {
            Self::Field(field) => field.to_string(),
            Self::Type(ty) => ty.to_string(),
        }
    }
}

impl StructMemberInput {
    /// Validates the input against the [`super::StructMember`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &StructMember) -> Result<(), TestCaseError> {
        match (self, output) {
            (Self::Field(input), StructMember::Field(output)) => input.validate(output),
            (Self::Type(input), StructMember::Type(output)) => input.validate(output),
            _ => Err(TestCaseError::fail("struct member mismatch")),
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
pub enum ItemInput {
    Trait(TraitInput),
    Function(FunctionInput),
    Struct(StructInput),
    Type(TypeInput),
    Enum(EnumInput),
}

impl ToString for ItemInput {
    fn to_string(&self) -> String {
        match self {
            Self::Trait(i) => i.to_string(),
            Self::Function(i) => i.to_string(),
            Self::Struct(i) => i.to_string(),
            Self::Type(i) => i.to_string(),
            Self::Enum(i) => i.to_string(),
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
                ConstraintInput::TraitConstraint(TraitConstraintInput {
                    qualified_identifier,
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
        ))
    ]
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

fn struct_type() -> impl Strategy<Value = StructTypeInput> {
    (
        crate::syntax_tree::strategy::access_modifier(),
        type_signature(),
        type_definition(),
    )
        .prop_map(
            |(access_modifier, type_signature, type_definition)| StructTypeInput {
                access_modifier,
                type_signature,
                type_definition,
            },
        )
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
    prop_oneof![
        struct_type().prop_map(StructMemberInput::Type),
        struct_field().prop_map(StructMemberInput::Field),
    ]
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
