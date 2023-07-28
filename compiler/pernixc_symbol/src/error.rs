//! Contains the definition of all semantic errors that can occur during the symbol
//! resolution/analysis.

use pernixc_print::LogSeverity;
use pernixc_source::Span;
use pernixc_syntax::syntax_tree::item::StructField;
use pernixc_system::arena;

use crate::{
    table::Table, ty, Field, Function, GlobalID, ImplementsFunction, LifetimeArgument,
    LifetimeParameter, Module, Parameter, Struct, Symbol, TraitBound, TraitFunction, TraitMemberID,
    TypeParameter, ID,
};

/// No target was found with the given name.
#[derive(Debug, Clone)]
pub struct TargetNotFound {
    /// The span of the unknown target name.
    pub unknown_target_span: Span,
}

impl TargetNotFound {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "the target name `{}` didn't exist",
                self.unknown_target_span.str()
            )
            .as_str(),
        );
    }
}

/// No module was found with the given name in the given module.
#[derive(Debug, Clone)]
pub struct ModuleNotFound {
    /// The module where the modules were searched for.
    pub in_module_id: arena::ID<Module>,

    /// The span of the unknown module name.
    pub unknown_module_span: Span,
}

impl ModuleNotFound {
    /// Prints the error message to the stdout.
    ///
    /// # Errors
    /// - If the module with the given ID does not exist in the table.
    pub fn print(&self, table: &Table) -> Result<(), arena::Error> {
        let in_module = table.modules().get_as_ok(self.in_module_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "the module `{}` didn't exist in the module `{}`",
                self.unknown_module_span.str(),
                in_module.name
            )
            .as_str(),
        );

        pernixc_print::print_source_code(&self.unknown_module_span, None);

        Ok(())
    }
}

/// A using statement was found duplicatig a previous using statement.
#[derive(Debug, Clone)]
pub struct UsingDuplication {
    /// The span of the previous using statement.
    pub previous_using_span: Span,

    /// The span of the duplicate using statement.
    pub duplicate_using_span: Span,
}

impl UsingDuplication {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "the `{}` using was already defined");

        pernixc_print::print_source_code(
            &self.previous_using_span,
            Some("was previously defined here"),
        );
        pernixc_print::print_source_code(
            &self.duplicate_using_span,
            Some("duplication found here"),
        );
    }
}

/// A using statement was found using a module that is the same as the module that it is in.
#[derive(Debug, Clone)]
pub struct UsingOwnModule {
    /// The module that is being used.
    pub module_id: arena::ID<Module>,

    /// The span of the using statement.
    pub using_span: Span,
}

impl UsingOwnModule {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "`using` statement can't use the same module that it is in",
        );

        pernixc_print::print_source_code(&self.using_span, None);
    }
}

/// A lifetime parameter was found to be declared after a type parameter.
#[derive(Debug, Clone)]
pub struct LifetimeParameterMustBeDeclaredPriorToTypeParameter {
    /// The span of the lifetime parameter.
    pub lifetime_parameter_span: Span,
}

impl LifetimeParameterMustBeDeclaredPriorToTypeParameter {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "lifetime parameters must be declared prior to type parameters",
        );

        pernixc_print::print_source_code(
            &self.lifetime_parameter_span,
            Some("the lifetime parameter was declared after type parameters"),
        );
    }
}

/// Symbol redefinition error.
#[derive(Debug, Clone)]
pub struct SymbolRedefinition<T> {
    /// The id of the symbol that is being redefined.
    pub previous_definition_id: T,

    /// Span to the syntax node that is redefining the symbol.
    pub redefinition_span: Span,
}

impl<T: Into<ID> + Clone> SymbolRedefinition<T> {
    /// Prints the error message to the stdout.
    ///
    /// # Errors
    /// If the `Self::previous_definition_id` is not in the passed table.
    pub fn print(&self, table: &Table) -> Result<(), arena::Error> {
        let previous_definition = table.get_symbol(self.previous_definition_id.clone().into())?;

        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "the symbol `{}` was already defined",
                previous_definition.name()
            )
            .as_str(),
        );

        pernixc_print::print_source_code(&self.redefinition_span, Some("redifinition found here"));
        if let Some(previously_defined_span) = previous_definition.symbol_span() {
            pernixc_print::print_source_code(
                &previously_defined_span,
                Some("previously defined here"),
            );
        }

        Ok(())
    }
}

/// No lifetime with the given name was found.
#[derive(Debug, Clone)]
pub struct LifetimeNotFound {
    /// The span of the unknown lifetime name.
    pub unknown_lifetime_span: Span,
}

impl LifetimeNotFound {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "the lifetime `{}` didn't exist",
                self.unknown_lifetime_span.str()
            )
            .as_str(),
        );

        pernixc_print::print_source_code(&self.unknown_lifetime_span, None);
    }
}

/// The struct filed is more accessible than the struct itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldMoreAccessibleThanStruct {
    /// The id of the field.
    pub field_id: arena::ID<Field>,

    /// The id of the struct.
    pub struct_id: arena::ID<Struct>,
}

/// The symbol is already defined in the current scope.
impl FieldMoreAccessibleThanStruct {
    /// Prints the error message to the stdout.
    ///
    /// # Errors
    /// If the `Self::field_id` or `Self::struct_id` are not in the passed table.
    pub fn print(&self, table: &Table) -> Result<(), arena::Error> {
        let field_sym = table.fields().get_as_ok(self.field_id)?;
        let struct_sym = table.structs().get_as_ok(self.struct_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "the field `{}` was more accessible than the struct `{}`",
                field_sym.name, struct_sym.name,
            ),
        );

        if let Some(field_identifier) = field_sym.syntax_tree.as_ref().map(StructField::identifier)
        {
            pernixc_print::print_source_code(
                &field_identifier.span,
                Some(&format!(
                    "the field had accessibility of `{}` while struct had `{}`",
                    field_sym.accessibility, struct_sym.accessibility,
                )),
            );
        }

        Ok(())
    }
}

/// There is a cyclic dependency between the symbols.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CyclicDependency {
    /// The list of symbols that are involved in the cycle.
    pub participants: Vec<ID>,
}

impl CyclicDependency {
    /// Prints the error message to the stdout.
    ///
    /// # Errors
    /// If any of the participants are not in the passed table.
    pub fn print(&self, table: &Table) -> Result<(), arena::Error> {
        let iter: Result<Vec<_>, arena::Error> = self
            .participants
            .iter()
            .map(|x| table.get_symbol(*x))
            .collect();
        let iter = iter?;

        pernixc_print::print(LogSeverity::Error, "cyclic dependency found");

        for symbol in iter {
            if let Some(symbol_span) = symbol.symbol_span() {
                pernixc_print::print_source_code(&symbol_span, None);
            }
        }

        Ok(())
    }
}

/// Multiples symbol candidates were found for the same symbol reference.
#[derive(Debug, Clone)]
pub struct ResolutionAmbiguity {
    /// The span of the symbol reference.
    pub span: Span,

    /// The list of candidates.
    pub candidates: Vec<GlobalID>,
}

impl ResolutionAmbiguity {
    /// Prints the error message to the stdout.
    ///
    /// # Errors
    /// If any of the candidates are not in the passed table.
    pub fn print(&self, table: &Table) -> Result<(), arena::Error> {
        pernixc_print::print(LogSeverity::Error, "resolution ambiguity found");

        pernixc_print::print_source_code(&self.span, None);

        let iter: Result<Vec<_>, arena::Error> = self
            .candidates
            .iter()
            .map(|x| table.get_global(*x))
            .collect();
        let iter = iter?;

        for candidate in iter {
            if let Some(candidate_span) = candidate.symbol_span() {
                pernixc_print::print_source_code(&candidate_span, None);
            }
        }

        Ok(())
    }
}

/// Symbol with the given name was not found.
#[derive(Debug, Clone)]
pub struct SymbolNotFound {
    /// In which scope the symbol was searched.
    pub searched_global_id: GlobalID,

    /// The span of the symbol reference.
    pub symbol_reference_span: Span,
}

impl SymbolNotFound {
    /// Prints the error message to the stdout.
    ///
    /// # Errors
    /// If the `Self::searched_global_id` is not in the passed table.
    pub fn print(&self, table: &Table) -> Result<(), arena::Error> {
        let global_name = table.get_qualified_name(self.searched_global_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "the symbol `{}` was not found in the scope `{}`",
                self.symbol_reference_span.str(),
                global_name
            ),
        );

        pernixc_print::print_source_code(&self.symbol_reference_span, None);

        Ok(())
    }
}

/// Found a qualified identifier can't be used as a path to a trait.
///
/// This error occurs when resolving the trait symbol of a qualified identifier in the `implements`
/// block.
#[derive(Debug, Clone)]
pub struct InvalidTraitPath {
    /// The span of the qualified identifier.
    pub span: Span,
}

impl InvalidTraitPath {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "invalid trait path");

        pernixc_print::print_source_code(
            &self.span,
            Some("the qualified identifier can't be used as a path to a trait"),
        );
    }
}

/// Expects the symbol to be module but found something else.
#[derive(Debug, Clone)]
pub struct ModuleExpected {
    /// The span of the symbol reference.
    pub symbol_reference_span: Span,

    /// The symbol that was found instead.
    pub found: GlobalID,
}

impl ModuleExpected {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "module expected");

        let symbol_kind_name = match self.found {
            GlobalID::Module(_) => "module",
            GlobalID::Struct(_) => "struct",
            GlobalID::Enum(_) => "enum",
            GlobalID::EnumVariant(_) => "enum variant",
            GlobalID::Function(_) => "function",
            GlobalID::Type(_) => "type",
            GlobalID::Trait(_) => "trait",
            GlobalID::TraitFunction(_) => "trait function",
            GlobalID::TraitType(_) => "trait type",
        };

        pernixc_print::print_source_code(
            &self.symbol_reference_span,
            Some(format!("found `{symbol_kind_name}` instead").as_str()),
        );
    }
}

/// Expects the symbol to be a trait but found something else.
#[derive(Debug, Clone)]
pub struct TraitExpected {
    /// The span of the symbol reference.
    pub symbol_reference_span: Span,
}

impl TraitExpected {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "trait expected");

        pernixc_print::print_source_code(&self.symbol_reference_span, None);
    }
}

/// Lifetime parameter shadowing is not allowed.
#[derive(Debug, Clone)]
pub struct LifetimeParameterShadowing {
    /// The span of the lifetime parameter.
    pub lifetime_parameter_declaration_span: Span,

    /// The ID of the shadowed lifetime parameter.
    pub shadowed_lifetime_parameter_id: arena::ID<LifetimeParameter>,
}

impl LifetimeParameterShadowing {
    /// Prints the error message to the stdout.
    ///
    /// # Errors
    /// If the `Self::shadowed_lifetime_parameter_id` is not in the passed table.
    pub fn print(&self, table: &Table) -> Result<(), arena::Error> {
        let previous_lifetime_parameter = table
            .lifetime_parameters()
            .get_as_ok(self.shadowed_lifetime_parameter_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            "lifetime parameter shadowing is not allowed",
        );

        pernixc_print::print_source_code(
            &self.lifetime_parameter_declaration_span,
            Some("this lifetime parameter shadowed the previous one"),
        );

        if let Some(previous_lifetime_parameter_span) = previous_lifetime_parameter.symbol_span() {
            pernixc_print::print_source_code(
                &previous_lifetime_parameter_span,
                Some("the previous lifetime parameter was declared here"),
            );
        }

        Ok(())
    }
}

/// Type parameter shadowing is not allowed.
#[derive(Debug, Clone)]
pub struct TypeParameterShadowing {
    /// The span of the type parameter.
    pub span: Span,

    /// The ID of the shadowed type parameter.
    pub shadowed_type_parameter_id: arena::ID<TypeParameter>,
}

impl TypeParameterShadowing {
    /// Prints the error message to the stdout.
    ///
    /// # Errors
    /// If the `Self::shadowed_type_parameter_id` is not in the passed table.
    pub fn print(&self, table: &Table) -> Result<(), arena::Error> {
        let previous_type_parameter = table
            .type_parameters()
            .get_as_ok(self.shadowed_type_parameter_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            "type parameter shadowing is not allowed",
        );

        pernixc_print::print_source_code(
            &self.span,
            Some("this type parameter shadowed the previous one"),
        );

        if let Some(previous_type_parameter_span) = previous_type_parameter.symbol_span() {
            pernixc_print::print_source_code(
                &previous_type_parameter_span,
                Some("the previous type parameter was declared here"),
            );
        }

        Ok(())
    }
}

/// Found lifetime arguments after type arguments.
#[derive(Debug, Clone)]
pub struct LifetimeArgumentMustBeSuppliedPriorToTypeArgument {
    /// The span of the lifetime argument.
    pub lifetime_argument_span: Span,
}

impl LifetimeArgumentMustBeSuppliedPriorToTypeArgument {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "lifetime arguments must be supplied prior to type arguments",
        );

        pernixc_print::print_source_code(
            &self.lifetime_argument_span,
            Some("the lifetime argument was supplied after type parameters"),
        );
    }
}

/// The number of supplied lifetime arguments does not match the number of expected lifetime
#[derive(Debug, Clone)]
pub struct LifetimeArgumentCountMismatch {
    /// The number of supplied lifetime arguments.
    pub supplied: usize,

    /// The number of expected lifetime arguments.
    pub expected: usize,

    /// The span to the generic arguments that contain the lifetime arguments.
    pub generic_arguments_span: Span,
}

impl LifetimeArgumentCountMismatch {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "the number of supplied lifetime arguments ({}) did not match the number of \
                 expected lifetime arguments ({})",
                self.supplied, self.expected,
            )
            .as_str(),
        );

        pernixc_print::print_source_code(&self.generic_arguments_span, None);
    }
}

/// The number of supplied type arguments does not match the number of expected type arguments.
#[derive(Debug, Clone)]
pub struct TypeArgumentCountMismatch {
    /// The number of supplied type arguments.
    pub supplied: usize,

    /// The number of expected type arguments.
    pub expected: usize,

    /// The span to the generic arguments that contain the type arguments.
    pub generic_arguments_span: Span,
}

impl TypeArgumentCountMismatch {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "the number of supplied type arguments ({}) did not match the number of expected \
                 type arguments ({})",
                self.supplied, self.expected,
            )
            .as_str(),
        );

        pernixc_print::print_source_code(&self.generic_arguments_span, None);
    }
}

/// Lifetime arguments must be supplied to the type in this context.
#[derive(Debug, Clone)]
pub struct LifetimeArgumentsRequired {
    /// The span of the type with missing lifetime arguments.
    pub span: Span,
}

/// There is no member on this type.
#[derive(Debug, Clone)]
pub struct NoMemberOnThisType {
    /// The span of the member name, trying to access.
    pub symbol_reference_span: Span,

    /// The type that does not have the member.
    pub ty: ty::Type,
}

/// Trait resolution is not allowed in this context.
#[derive(Debug, Clone)]
pub struct TraitResolutionNotAllowed {
    /// The location to the generics identifier that causes the trait resolution to occur.
    pub trait_resolution_span: Span,
}

/// The given symbol couldn't be used as a type.
#[derive(Debug, Clone)]
pub struct TypeExpected {
    /// The span of the symbol reference.
    pub non_type_symbol_span: Span,
}

impl TypeExpected {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "type expected");

        pernixc_print::print_source_code(
            &self.non_type_symbol_span,
            Some("this symbol couldn't be used as a type"),
        );
    }
}

/// The symbol is not accessible from the given referring site.
#[derive(Debug, Clone)]
pub struct SymbolIsNotAccessible {
    /// The span of the symbol reference.
    pub span: Span,

    /// Where the symbol is being referred from.
    pub referring_site: ID,

    /// The symbol that is not accessible.
    pub referred: GlobalID,
}

/// The symbol does not require any generic arguments but some were supplied.
#[derive(Debug, Clone)]
pub struct NoGenericArgumentsRequired {
    /// The span of the generic arguments.
    pub span: Span,
}

/// The more private symbol is being exposed in a more public scope.
#[derive(Debug, Clone)]
pub struct PrivateSymbolLeakage {
    /// The span of the symbol reference.
    pub span: Span,
}

/// The supplied lifetime argument does not outlive the required lifetime.
#[derive(Debug, Clone)]
pub struct LifetimeDoesNotOutlive {
    /// The supplied lifetime that does not outlive the required lifetime.
    pub passed_lifetime_parameter: arena::ID<LifetimeParameter>,

    /// The lifetime that the supplied argument must outlive.
    pub required_lifetime_argument: LifetimeArgument,

    /// Location where the lifetime argument is supplied.
    pub type_span: Span,
}

/// The required trait bound is not satisfied.
#[derive(Debug, Clone)]
pub struct TraitBoundNotSatisfied {
    /// The required trait bound that is not satisfied.
    pub required_trait_bound: TraitBound,

    /// The span to the generics identifier that causes the trait bound check to occur.
    pub generics_identifier_span: Span,
}

/// The required trait type bound is not satisfied.
#[derive(Debug, Clone)]
pub struct TraitTypeBoundNotSatisfied {
    /// The required trait type bound that is not satisfied.
    pub required_type: ty::Type,

    /// The span to the generics identifier that causes the trait type bound check to occur.
    pub generics_identifier_span: Span,
}

/// The trait function does not have the member.
#[derive(Debug, Clone)]
pub struct NoMemberOnThisImplementsFunction {
    /// The span of the member name, trying to access.
    pub symbol_reference_span: Span,

    /// The implements function that does not have the member.
    pub implements_function_id: arena::ID<ImplementsFunction>,
}

/// The type does not outlive the required lifetime argument.
#[derive(Debug, Clone)]
pub struct TypeDoesNotOutliveLifetimeArgument {
    /// The required lifetime argument that the type must outlive.
    pub required_lifetime_argument: LifetimeArgument,

    /// The type that does not outlive the required lifetime argument.
    pub ty: ty::Type,

    /// Location where the type is supplied.
    pub generics_identifier_span: Span,
}

/// Is an enumeration of all errors occurring during the symbol resolution/analysis.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Error {
    TypeExpected(TypeExpected),
    TargetNotFound(TargetNotFound),
    ModuleNotFound(ModuleNotFound),
    UsingDuplication(UsingDuplication),
    UsingOwnModule(UsingOwnModule),
    SymbolRedefinition(SymbolRedefinition<GlobalID>),
    LifetimeParameterRedefinition(SymbolRedefinition<arena::ID<LifetimeParameter>>),
    TypeParameterRedefinition(SymbolRedefinition<arena::ID<TypeParameter>>),
    LifetimeArgumentMustBeSuppliedPriorToTypeArgument(
        LifetimeArgumentMustBeSuppliedPriorToTypeArgument,
    ),
    LifetimeParameterMustBeDeclaredPriorToTypeParameter(
        LifetimeParameterMustBeDeclaredPriorToTypeParameter,
    ),
    FunctionParameterRedefinition(SymbolRedefinition<arena::ID<Parameter<Function>>>),
    TraitFunctionParameterRedefinition(SymbolRedefinition<arena::ID<Parameter<TraitFunction>>>),
    ImplementsFunctionParameterRedefinition(
        SymbolRedefinition<arena::ID<Parameter<ImplementsFunction>>>,
    ),
    FieldRedefinition(SymbolRedefinition<arena::ID<Field>>),
    FieldMoreAccessibleThanStruct(FieldMoreAccessibleThanStruct),
    TraitMemberRedefinition(SymbolRedefinition<TraitMemberID>),
    CyclicDependency(CyclicDependency),
    ResolutionAmbiguity(ResolutionAmbiguity),
    SymbolNotFound(SymbolNotFound),
    InvalidTraitPath(InvalidTraitPath),
    ModuleExpected(ModuleExpected),
    TraitExpected(TraitExpected),
    LifetimeParameterShadowing(LifetimeParameterShadowing),
    TypeParameterShadowing(TypeParameterShadowing),
    LifetimeNotFound(LifetimeNotFound),
    LifetimeArgumentCountMismatch(LifetimeArgumentCountMismatch),
    TypeArgumentCountMismatch(TypeArgumentCountMismatch),
    LifetimeArgumentsRequired(LifetimeArgumentsRequired),
    NoMemberOnThisType(NoMemberOnThisType),
    TraitResolutionNotAllowed(TraitResolutionNotAllowed),
    SymbolIsNotAccessible(SymbolIsNotAccessible),
    NoGenericArgumentsRequired(NoGenericArgumentsRequired),
    PrivateSymbolLeakage(PrivateSymbolLeakage),
    LifetimeDoesNotOutlive(LifetimeDoesNotOutlive),
    TraitBoundNotSatisfied(TraitBoundNotSatisfied),
    TraitTypeBoundNotSatisfied(TraitTypeBoundNotSatisfied),
    NoMemberOnThisImplementsFunction(NoMemberOnThisImplementsFunction),
    TypeDoesNotOutliveLifetimeArgument(TypeDoesNotOutliveLifetimeArgument),
}

impl Error {
    /// Prints the error message to the stdout.
    ///
    /// # Errors
    /// If this error instance did not come from the given table.
    pub fn print(&self, table: &Table) -> Result<(), arena::Error> {
        match self {
            Self::TypeExpected(err) => {
                err.print();
                Ok(())
            }
            Self::TargetNotFound(err) => {
                err.print();
                Ok(())
            }
            Self::ModuleNotFound(err) => err.print(table),
            Self::UsingDuplication(err) => {
                err.print();
                Ok(())
            }
            Self::UsingOwnModule(err) => {
                err.print();
                Ok(())
            }
            Self::SymbolRedefinition(err) => err.print(table),
            Self::LifetimeParameterRedefinition(err) => err.print(table),
            Self::TypeParameterRedefinition(err) => err.print(table),
            Self::LifetimeArgumentMustBeSuppliedPriorToTypeArgument(err) => {
                err.print();
                Ok(())
            }
            Self::LifetimeParameterMustBeDeclaredPriorToTypeParameter(err) => {
                err.print();
                Ok(())
            }
            Self::FunctionParameterRedefinition(err) => err.print(table),
            Self::TraitFunctionParameterRedefinition(err) => err.print(table),
            Self::ImplementsFunctionParameterRedefinition(err) => err.print(table),
            Self::FieldRedefinition(err) => err.print(table),
            Self::FieldMoreAccessibleThanStruct(err) => err.print(table),
            Self::TraitMemberRedefinition(err) => err.print(table),
            Self::CyclicDependency(err) => err.print(table),
            Self::ResolutionAmbiguity(err) => err.print(table),
            Self::ModuleExpected(err) => {
                err.print();
                Ok(())
            }
            Self::InvalidTraitPath(err) => {
                err.print();
                Ok(())
            }
            Self::NoMemberOnThisImplementsFunction(_)
            | Self::SymbolNotFound(_)
            | Self::TraitExpected(_)
            | Self::LifetimeParameterShadowing(_)
            | Self::TypeParameterShadowing(_)
            | Self::LifetimeNotFound(_)
            | Self::LifetimeArgumentCountMismatch(_)
            | Self::TypeArgumentCountMismatch(_)
            | Self::LifetimeArgumentsRequired(_)
            | Self::NoMemberOnThisType(_)
            | Self::TraitResolutionNotAllowed(_)
            | Self::SymbolIsNotAccessible(_)
            | Self::NoGenericArgumentsRequired(_)
            | Self::PrivateSymbolLeakage(_)
            | Self::LifetimeDoesNotOutlive(_)
            | Self::TypeDoesNotOutliveLifetimeArgument(_)
            | Self::TraitTypeBoundNotSatisfied(_)
            | Self::TraitBoundNotSatisfied(_) => Ok(()),
        }
    }
}
