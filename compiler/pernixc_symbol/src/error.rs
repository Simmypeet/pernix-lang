//! Contains the definition of all semantic errors that can occur during the symbol
//! resolution/analysis.
use pernixc_print::LogSeverity;
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::item::StructField;
use pernixc_system::arena;

use crate::{
    table::Table, ty, Field, Function, Genericable, GlobalID, Implements, ImplementsFunction,
    ImplementsMemberID, LifetimeArgument, LifetimeParameter, Module, Parameter, Struct, Symbol,
    Trait, TraitFunction, TraitMemberID, TypeParameter, ID,
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

        pernixc_print::print_source_code(&self.unknown_target_span, None);
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
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let Some(in_module) = table.modules().get(self.in_module_id) else {
            return false;
        };

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

        true
    }
}

/// Not all trait members were implemented.
#[derive(Debug, Clone)]
pub struct NotAllTraitMembersWereImplemented {
    /// THe trait members that weren't implemented
    pub unimplemented_members: Vec<TraitMemberID>,

    /// The trait id that was being implemented.
    pub trait_id: arena::ID<Trait>,

    /// The span of the implements.
    pub implements_span: Span,
}

impl NotAllTraitMembersWereImplemented {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let unimplemented_member_strings: Option<Vec<&str>> = self
            .unimplemented_members
            .iter()
            .copied()
            .map(|x| table.get_symbol(x.into()).map(Symbol::name))
            .collect();

        let (Some(unimplemented_member_strings), Some(trait_name)) = (
            unimplemented_member_strings,
            table.get_qualified_name(self.trait_id.into()),
        ) else {
            return false;
        };
        let unimplemented_member_string = unimplemented_member_strings.join(", ");

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "not all trait members were implemented for trait `{trait_name}`. The following \
                 members weren't implemented: {unimplemented_member_string}",
            ),
        );

        pernixc_print::print_source_code(&self.implements_span, None);

        true
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
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    pub fn print(&self, table: &Table) -> bool {
        let Some(previous_definition) =
            table.get_symbol(self.previous_definition_id.clone().into())
        else {
            return false;
        };

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

        true
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

impl FieldMoreAccessibleThanStruct {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    pub fn print(&self, table: &Table) -> bool {
        let (Some(field_sym), Some(struct_sym)) = (
            table.fields().get(self.field_id),
            table.structs().get(self.struct_id),
        ) else {
            return false;
        };

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

        true
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
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let iter: Option<Vec<_>> = self
            .participants
            .iter()
            .map(|x| table.get_symbol(*x))
            .collect();
        let Some(iter) = iter else { return false };

        pernixc_print::print(LogSeverity::Error, "cyclic dependency found");

        for symbol in iter {
            if let Some(symbol_span) = symbol.symbol_span() {
                pernixc_print::print_source_code(&symbol_span, None);
            }
        }

        true
    }
}

/// The trait didn't have a member with the given name.
#[derive(Debug, Clone)]
pub struct UnknownTraitMemberInImplements {
    /// The span of the unknown member name.
    pub unknown_member_span: Span,

    /// The id of the trait.
    pub trait_id: arena::ID<Trait>,
}

impl UnknownTraitMemberInImplements {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let Some(trait_name) = table.get_qualified_name(self.trait_id.into()) else {
            return false;
        };

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "the trait `{}` didn't have a member named `{}`",
                trait_name,
                self.unknown_member_span.str(),
            ),
        );

        pernixc_print::print_source_code(&self.unknown_member_span, None);

        true
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
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        pernixc_print::print(LogSeverity::Error, "resolution ambiguity found");

        pernixc_print::print_source_code(&self.span, None);

        let iter: Option<Vec<_>> = self
            .candidates
            .iter()
            .map(|x| table.get_global(*x))
            .collect();
        let Some(iter) = iter else {
            return false;
        };

        for candidate in iter {
            if let Some(candidate_span) = candidate.symbol_span() {
                pernixc_print::print_source_code(&candidate_span, None);
            }
        }

        true
    }
}

/// Symbol with the given name was not found.
#[derive(Debug, Clone)]
pub struct SymbolNotFound {
    /// In which scope the symbol was searched.
    pub searched_global_id: Option<GlobalID>,

    /// The span of the symbol reference.
    pub symbol_reference_span: Span,
}

impl SymbolNotFound {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        if let Some(searched_global_id) = self.searched_global_id {
            let Some(global_name) = table.get_qualified_name(searched_global_id) else {
                return false;
            };

            pernixc_print::print(
                LogSeverity::Error,
                &format!(
                    "the symbol `{}` was not found in the scope `{}`",
                    self.symbol_reference_span.str(),
                    global_name
                ),
            );
        } else {
            pernixc_print::print(
                LogSeverity::Error,
                &format!(
                    "the symbol `{}` was not found in the current scope",
                    self.symbol_reference_span.str(),
                ),
            );
        }

        pernixc_print::print_source_code(&self.symbol_reference_span, None);

        true
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
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let Some(previous_lifetime_parameter) = table
            .lifetime_parameters()
            .get(self.shadowed_lifetime_parameter_id)
        else {
            return false;
        };

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

        true
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
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let Some(previous_type_parameter) =
            table.type_parameters().get(self.shadowed_type_parameter_id)
        else {
            return false;
        };

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

        true
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

impl LifetimeArgumentsRequired {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "lifetime arguments must be supplied to the type in this context",
        );

        pernixc_print::print_source_code(&self.span, None);
    }
}

/// There is no member on this type.
#[derive(Debug, Clone)]
pub struct NoMemberOnThisType {
    /// The span of the member name, trying to access.
    pub symbol_reference_span: Span,

    /// The type that does not have the member.
    pub ty: ty::Type,
}

impl NoMemberOnThisType {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let Some(ty_string) = table.get_type_string(&self.ty) else {
            return false;
        };

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "couldn't find the member `{}` on `{ty_string}` implements function ",
                self.symbol_reference_span.str(),
            ),
        );

        pernixc_print::print_source_code(&self.symbol_reference_span, None);

        true
    }
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
pub struct SymbolWasNotAccessible {
    /// The span of the symbol reference.
    pub span: Span,

    /// Where the symbol is being referred from.
    pub referring_module_site: arena::ID<Module>,

    /// The symbol that is not accessible.
    pub referred: GlobalID,
}

impl SymbolWasNotAccessible {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// Returns `true` if the error was printed, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let (Some(referring_module_name), Some(referred_name)) = (
            table.get_qualified_name(self.referring_module_site.into()),
            table.get_qualified_name(self.referred),
        ) else {
            return false;
        };

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "the symbol `{referred_name}` was not accessible from `{referring_module_name}`",
            ),
        );

        pernixc_print::print_source_code(&self.span, None);

        true
    }
}

/// Couldn't find a trait implements with the given generic arguments.
#[derive(Debug, Clone)]
pub struct NoImplementsFound {
    /// The span of the generic arguments.
    pub span: Span,
}

impl NoImplementsFound {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "no implements found");

        pernixc_print::print_source_code(
            &self.span,
            Some("couldn't find a trait implements with the given generic arguments"),
        );
    }
}

/// The symbol does not require any generic arguments but some were supplied.
#[derive(Debug, Clone)]
pub struct NoGenericArgumentsRequired {
    /// The span of the generic arguments.
    pub span: Span,
}

impl NoGenericArgumentsRequired {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "no generic arguments required");

        pernixc_print::print_source_code(&self.span, None);
    }
}

/// The more private symbol is being exposed in a more public scope.
#[derive(Debug, Clone)]
pub struct PrivateSymbolLeakage {
    /// The span of the symbol reference.
    pub span: Span,
}

impl PrivateSymbolLeakage {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "the less acessibile symbol was being exposed in a more public scope",
        );

        pernixc_print::print_source_code(&self.span, None);
    }
}

/// Trait resolutions weren't allowed in this context.
#[derive(Debug, Clone)]
pub struct TraitResolutionNotAllowed {
    /// Span to the location where trait resolution occurred.
    pub trait_resolution_span: Span,
}

impl TraitResolutionNotAllowed {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "trait resolutions weren't allowed in this context",
        );

        pernixc_print::print_source_code(&self.trait_resolution_span, None);
    }
}

/// The supplied lifetime argument does not outlive the required lifetime.
#[derive(Debug, Clone)]
pub struct LifetimeDoesNotOutlive {
    /// The supplied lifetime that does not outlive the required lifetime.
    pub passed_lifetime_parameter: Option<arena::ID<LifetimeParameter>>,

    /// The lifetime that the supplied argument must outlive.
    pub required_lifetime_argument: LifetimeArgument,

    /// Location where the lifetime bound check occurs.
    pub bound_check_span: Span,
}

impl LifetimeDoesNotOutlive {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// Returns `true` if the error was printed, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let required_lifetime_name = match self.required_lifetime_argument {
            LifetimeArgument::Static => "static",
            LifetimeArgument::Parameter(parameter) => {
                let Some(lifetime_parameter) = table.lifetime_parameters().get(parameter) else {
                    return false;
                };
                &lifetime_parameter.name
            }
        };

        if let Some(passed_lifetime_parameter) = self.passed_lifetime_parameter {
            let Some(passed_lifetime_parameter_name) = table
                .lifetime_parameters()
                .get(passed_lifetime_parameter)
                .map(|x| &x.name)
            else {
                return false;
            };

            pernixc_print::print(
                LogSeverity::Error,
                &format!(
                    "the lifetime `{passed_lifetime_parameter_name}` didn't outlive the lifetime \
                     `'{required_lifetime_name}`"
                ),
            );
        } else {
            pernixc_print::print(
                LogSeverity::Error,
                &format!(
                    "the elided lifetieme didn't outlive the lifetime `'{required_lifetime_name}`"
                ),
            );
        }

        pernixc_print::print_source_code(&self.bound_check_span, None);

        true
    }
}

/// The required trait bound is not satisfied.
#[derive(Debug, Clone)]
pub struct TraitBoundNotSatisfied {
    /// The required trait bound that is not satisfied.
    pub required_trait_bound_string: String,

    /// The span to the generics identifier that causes the trait bound check to occur.
    pub generic_identifier_span: Span,
}

impl TraitBoundNotSatisfied {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "the required trait bound `{}` is not satisfied",
                self.required_trait_bound_string
            ),
        );

        pernixc_print::print_source_code(&self.generic_identifier_span, None);
    }
}

/// The required trait type bound is not satisfied.
#[derive(Debug, Clone)]
pub struct TraitTypeBoundNotSatisfied {
    /// The required trait type bound that is not satisfied.
    pub required_type: ty::Type,

    /// The span to the generics identifier that causes the trait type bound check to occur.
    pub generics_identifier_span: Span,
}

impl TraitTypeBoundNotSatisfied {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// Returns `true` if the error was printed, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let Some(type_string) = table.get_type_string(&self.required_type) else {
            return false;
        };

        pernixc_print::print(
            LogSeverity::Error,
            &format!("the required trait type bound `{type_string}` was not satisfied",),
        );

        pernixc_print::print_source_code(&self.generics_identifier_span, None);

        true
    }
}

/// The trait function does not have the member.
#[derive(Debug, Clone)]
pub struct NoMemberOnThisImplementsFunction {
    /// The span of the member name, trying to access.
    pub symbol_reference_span: Span,

    /// The implements function that does not have the member.
    pub implements_function_id: arena::ID<ImplementsFunction>,
}

impl NoMemberOnThisImplementsFunction {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// Returns `true` if the error was printed, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let Some(implements_function_name) = table
            .implements_functions()
            .get(self.implements_function_id)
            .map(|x| &x.name)
        else {
            return false;
        };

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "couldn't find the member `{}` on `{implements_function_name}` implements \
                 function ",
                self.symbol_reference_span.str(),
            ),
        );

        pernixc_print::print_source_code(&self.symbol_reference_span, None);

        true
    }
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

impl TypeDoesNotOutliveLifetimeArgument {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// Returns `true` if the error was printed, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let (Some(required_lifetime_argument_string), Some(ty_string)) = (
            match self.required_lifetime_argument {
                LifetimeArgument::Static => Some("static"),
                LifetimeArgument::Parameter(lifetime_parameter) => table
                    .lifetime_parameters()
                    .get(lifetime_parameter)
                    .map(|x| x.name.as_str()),
            },
            table.get_type_string(&self.ty),
        ) else {
            return false;
        };

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "the type `{ty_string}` did not outlive the lifetime \
                 `'{required_lifetime_argument_string}`",
            ),
        );

        pernixc_print::print_source_code(&self.generics_identifier_span, None);

        true
    }
}

/// There was already an implements with the same/similar specialization.
#[derive(Debug, Clone)]
pub struct AmbiguousImplements {
    /// The existing implements.
    pub existing_implements: arena::ID<Implements>,

    /// The span to the new implements.
    pub new_implements_span: Span,
}

/// Type lifetime parameters weren't all used
#[derive(Debug, Clone)]
pub struct UnusedLifetimeParameters {
    /// The spans to the unused lifetime parameters.
    pub unused_lifetime_parameters: Vec<Span>,
}

impl UnusedLifetimeParameters {
    /// Prints the error message to the stdout.
    pub fn print(&self) -> bool {
        let names: Vec<&str> = self
            .unused_lifetime_parameters
            .iter()
            .map(pernixc_source::Span::str)
            .collect();

        let names = names.join(", ");

        pernixc_print::print(
            LogSeverity::Error,
            &format!("unused lifetime parameters: {names}"),
        );

        let Some(Some(type_parameters_span)) = self
            .unused_lifetime_parameters
            .iter()
            .cloned()
            .map(Some)
            .reduce(|lhs, rhs| {
                if let (Some(lhs), Some(rhs)) = (lhs, rhs) {
                    lhs.join(&rhs)
                } else {
                    None
                }
            })
        else {
            return false;
        };

        pernixc_print::print_source_code(&type_parameters_span, None);

        true
    }
}

/// The where clause of the implements member was incompatible to its trait member.
#[derive(Debug, Clone)]
pub struct IncompatibleImplementsMemberWhereClause {
    /// The span to the implements member that had the incompatible where clause.
    pub implements_member_span: Span,
}

impl IncompatibleImplementsMemberWhereClause {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "the where clause of the implements member was incompatible to its trait member",
        );

        pernixc_print::print_source_code(&self.implements_member_span, None);
    }
}

/// The trait type bound with this type has already been specified in another scope.
#[derive(Debug, Clone)]
pub struct TraitTypeBoundHasAlreadyBeenSpecified {
    /// The span to the trait type bound.
    pub trait_type_bound_span: Span,
}

impl TraitTypeBoundHasAlreadyBeenSpecified {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "the trait type bound with this type has already been specified in another scope",
        );

        pernixc_print::print_source_code(&self.trait_type_bound_span, None);
    }
}

impl AmbiguousImplements {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// Returns `true` if the error was printed, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let Some(existing_implements) = table.implements().get(self.existing_implements) else {
            return false;
        };

        pernixc_print::print(
            LogSeverity::Error,
            "ambiguous implements, there was already an implements with the same/similar \
             specialization",
        );

        pernixc_print::print_source_code(&self.new_implements_span, None);

        if let Some(existing_implements_signature) = existing_implements.syntax_tree.as_ref() {
            pernixc_print::print_source_code(
                &existing_implements_signature.span(),
                Some("existing implements"),
            );
        }

        true
    }
}

/// The implements member didn't have matching type parameters.
#[derive(Debug, Clone)]
pub struct ImplementsTypeParameterCountMismatch {
    /// Actual type parameter count.
    pub implements_member_type_parameter_count: usize,

    /// Which trait member is this implements member implementing.
    pub trait_member_id: TraitMemberID,

    /// The span to the implements's type parameters.
    pub implements_member_span: Span,
}

impl ImplementsTypeParameterCountMismatch {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// Returns `true` if the error was printed, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let Some((trait_member_name, type_parameter_count)) = (match self.trait_member_id {
            TraitMemberID::Function(id) => table.trait_functions().get(id).map(|x| {
                (
                    x.name(),
                    x.generic_parameters().type_parameter_ids_by_name.len(),
                )
            }),
            TraitMemberID::Type(id) => table.trait_types().get(id).map(|x| {
                (
                    x.name(),
                    x.generic_parameters().type_parameter_ids_by_name.len(),
                )
            }),
        }) else {
            return false;
        };

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "trait member `{trait_member_name}` had {type_parameter_count} type parameters, \
                 but the implements member had {} type parameters",
                self.implements_member_type_parameter_count,
            ),
        );

        pernixc_print::print_source_code(&self.implements_member_span, None);

        true
    }
}

/// The implements member didn't have matching lifetime parameters.
#[derive(Debug, Clone)]
pub struct ImplementsLifetimeParameterCountMismatch {
    /// Actual type parameter count.
    pub implements_member_lifetime_parameter_count: usize,

    /// Which trait member is this implements member implementing.
    pub trait_member_id: TraitMemberID,

    /// The span to the implements's type parameters.
    pub implements_member_span: Span,
}

impl ImplementsLifetimeParameterCountMismatch {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// Returns `true` if the error was printed, `false` otherwise.
    #[must_use]
    pub fn print(&self, table: &Table) -> bool {
        let Some((trait_member_name, lifetime_parameter_count)) = (match self.trait_member_id {
            TraitMemberID::Function(id) => table.trait_functions().get(id).map(|x| {
                (
                    x.name(),
                    x.generic_parameters().lifetime_parameter_ids_by_name.len(),
                )
            }),
            TraitMemberID::Type(id) => table.trait_types().get(id).map(|x| {
                (
                    x.name(),
                    x.generic_parameters().lifetime_parameter_ids_by_name.len(),
                )
            }),
        }) else {
            return false;
        };

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "trait member `{trait_member_name}` had {lifetime_parameter_count} type \
                 parameters, but the implements member had {} type parameters",
                self.implements_member_lifetime_parameter_count,
            ),
        );

        pernixc_print::print_source_code(&self.implements_member_span, None);

        true
    }
}

/// Is an enumeration representing all kinds of trait members.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TraitMemberKind {
    Function,
    Type,
}

/// Expected a member of a different kind.
#[derive(Debug, Clone)]
pub struct TraitMemberKindMismatch {
    /// The expected kind of the trait member.
    pub expected_kind: TraitMemberKind,

    /// The actual kind of the trait member.
    pub actual_kind: TraitMemberKind,

    /// The span of the trait member.
    pub span: Span,
}

impl TraitMemberKindMismatch {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        let expected_kind = match self.expected_kind {
            TraitMemberKind::Function => "function",
            TraitMemberKind::Type => "type",
        };

        let actual_kind = match self.actual_kind {
            TraitMemberKind::Function => "function",
            TraitMemberKind::Type => "type",
        };

        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "expected a member of kind `{expected_kind}`, found a member of kind \
                 `{actual_kind}`",
            )
            .as_str(),
        );

        pernixc_print::print_source_code(&self.span, None);
    }
}

/// The type parameters weren't all used.
#[derive(Debug, Clone)]
pub struct UnusedTypeParameters {
    /// The spans to the unused type parameters.
    pub unused_type_parameter_spans: Vec<Span>,
}

impl UnusedTypeParameters {
    /// Prints the error message to the stdout.
    pub fn print(&self) -> bool {
        let names: Vec<&str> = self
            .unused_type_parameter_spans
            .iter()
            .map(pernixc_source::Span::str)
            .collect();

        let names = names.join(", ");

        pernixc_print::print(
            LogSeverity::Error,
            &format!("unused type parameters: {names}"),
        );

        let Some(Some(type_parameters_span)) = self
            .unused_type_parameter_spans
            .iter()
            .cloned()
            .map(Some)
            .reduce(|lhs, rhs| {
                if let (Some(lhs), Some(rhs)) = (lhs, rhs) {
                    lhs.join(&rhs)
                } else {
                    None
                }
            })
        else {
            return false;
        };

        pernixc_print::print_source_code(&type_parameters_span, None);

        true
    }
}

/// The implements function didn't have matching parameter count.
#[derive(Debug, Clone)]
pub struct ImplementsFunctionParameterCountMismatch {
    /// The expected parameter count.
    pub expected_parameter_count: usize,

    /// The actual parameter count.
    pub actual_parameter_count: usize,

    /// The span of the implements function.
    pub implements_member_span: Span,
}

impl ImplementsFunctionParameterCountMismatch {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "expected {} parameters, found {} parameters",
                self.expected_parameter_count, self.actual_parameter_count
            ),
        );

        pernixc_print::print_source_code(&self.implements_member_span, None);
    }
}

/// The implements function didn't have matching return type.
#[derive(Debug, Clone)]
pub struct ImplementsFunctionReturnTypeMismatch {
    /// The expected return type.
    pub expected_return_type_string: String,

    /// The actual return type.
    pub actual_return_type_string: String,

    /// The span of the implements function.
    pub implements_function_return_type_span: Span,
}

impl ImplementsFunctionReturnTypeMismatch {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "expected return type `{}`, found return type `{}`",
                self.expected_return_type_string, self.actual_return_type_string
            ),
        );

        pernixc_print::print_source_code(&self.implements_function_return_type_span, None);
    }
}

/// The implements function didn't have matching parameter types.
#[derive(Debug, Clone)]
pub struct ImplementsFunctionParameterTypeMismatch {
    /// The expected parameter type.
    pub expected_parameter_type_string: String,

    /// The actual parameter type.
    pub actual_parameter_type_string: String,

    /// The span of the implements function.
    pub implements_function_parameter_type_span: Span,
}

impl ImplementsFunctionParameterTypeMismatch {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "expected parameter type `{}`, found parameter type `{}`",
                self.expected_parameter_type_string, self.actual_parameter_type_string
            ),
        );

        pernixc_print::print_source_code(&self.implements_function_parameter_type_span, None);
    }
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
    AmbiguousImplements(AmbiguousImplements),
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
    UnusedLifetimeParameters(UnusedLifetimeParameters),
    ImplementsMemberRedefinition(SymbolRedefinition<ImplementsMemberID>),
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
    SymbolWasNotAccessible(SymbolWasNotAccessible),
    NoGenericArgumentsRequired(NoGenericArgumentsRequired),
    PrivateSymbolLeakage(PrivateSymbolLeakage),
    LifetimeDoesNotOutlive(LifetimeDoesNotOutlive),
    TraitMemberKindMismatch(TraitMemberKindMismatch),
    TraitBoundNotSatisfied(TraitBoundNotSatisfied),
    UnknownTraitMemberInImplements(UnknownTraitMemberInImplements),
    TraitTypeBoundNotSatisfied(TraitTypeBoundNotSatisfied),
    NoMemberOnThisImplementsFunction(NoMemberOnThisImplementsFunction),
    TypeDoesNotOutliveLifetimeArgument(TypeDoesNotOutliveLifetimeArgument),
    TraitTypeBoundHasAlreadyBeenSpecified(TraitTypeBoundHasAlreadyBeenSpecified),
    NotAllTraitMembersWereImplemented(NotAllTraitMembersWereImplemented),
    NoImplementsFound(NoImplementsFound),
    UnusedTypeParameters(UnusedTypeParameters),
    ImplementsTypeParameterCountMismatch(ImplementsTypeParameterCountMismatch),
    ImplementsLifetimeParameterCountMismatch(ImplementsLifetimeParameterCountMismatch),
    TraitResolutionNotAllowed(TraitResolutionNotAllowed),
    IncompatibleImplementsMemberWhereClause(IncompatibleImplementsMemberWhereClause),
    ImplementsFunctionParameterCountMismatch(ImplementsFunctionParameterCountMismatch),
    ImplementsFunctionReturnTypeMismatch(ImplementsFunctionReturnTypeMismatch),
    ImplementsFunctionParameterTypeMismatch(ImplementsFunctionParameterTypeMismatch),
}

impl Error {
    /// Prints the error message to the stdout.
    ///
    /// # Returns
    /// - `true` if the error was printed successfully, `false` otherwise.
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn print(&self, table: &Table) -> bool {
        match self {
            Self::TypeExpected(err) => {
                err.print();
                true
            }
            Self::ImplementsMemberRedefinition(err) => {
                err.print(table);
                true
            }
            Self::NotAllTraitMembersWereImplemented(err) => err.print(table),
            Self::TargetNotFound(err) => {
                err.print();
                true
            }
            Self::NoImplementsFound(err) => {
                err.print();
                true
            }
            Self::ModuleNotFound(err) => err.print(table),
            Self::UsingDuplication(err) => {
                err.print();
                true
            }
            Self::UsingOwnModule(err) => {
                err.print();
                true
            }
            Self::SymbolRedefinition(err) => err.print(table),
            Self::LifetimeParameterRedefinition(err) => err.print(table),
            Self::TypeParameterRedefinition(err) => err.print(table),
            Self::LifetimeArgumentMustBeSuppliedPriorToTypeArgument(err) => {
                err.print();
                true
            }
            Self::LifetimeParameterMustBeDeclaredPriorToTypeParameter(err) => {
                err.print();
                true
            }
            Self::UnusedLifetimeParameters(err) => err.print(),
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
                true
            }
            Self::InvalidTraitPath(err) => {
                err.print();
                true
            }
            Self::UnusedTypeParameters(err) => {
                err.print();
                true
            }
            Self::SymbolNotFound(err) => err.print(table),
            Self::AmbiguousImplements(err) => err.print(table),
            Self::UnknownTraitMemberInImplements(err) => err.print(table),
            Self::SymbolWasNotAccessible(err) => err.print(table),
            Self::TraitTypeBoundHasAlreadyBeenSpecified(err) => {
                err.print();
                true
            }
            Self::TraitMemberKindMismatch(err) => {
                err.print();
                true
            }
            Self::TraitBoundNotSatisfied(err) => {
                err.print();
                true
            }
            Self::ImplementsTypeParameterCountMismatch(err) => err.print(table),
            Self::ImplementsLifetimeParameterCountMismatch(err) => err.print(table),
            Self::LifetimeArgumentsRequired(err) => {
                err.print();
                true
            }
            Self::NoMemberOnThisImplementsFunction(err) => err.print(table),
            Self::TraitExpected(err) => {
                err.print();
                true
            }
            Self::LifetimeParameterShadowing(err) => err.print(table),
            Self::TypeParameterShadowing(err) => err.print(table),
            Self::LifetimeNotFound(err) => {
                err.print();
                true
            }
            Self::LifetimeArgumentCountMismatch(err) => {
                err.print();
                true
            }
            Self::TypeArgumentCountMismatch(err) => {
                err.print();
                true
            }
            Self::NoMemberOnThisType(err) => err.print(table),
            Self::NoGenericArgumentsRequired(err) => {
                err.print();
                true
            }
            Self::PrivateSymbolLeakage(err) => {
                err.print();
                true
            }
            Self::LifetimeDoesNotOutlive(err) => err.print(table),
            Self::TraitResolutionNotAllowed(err) => {
                err.print();
                true
            }
            Self::TypeDoesNotOutliveLifetimeArgument(err) => err.print(table),
            Self::IncompatibleImplementsMemberWhereClause(err) => {
                err.print();
                true
            }
            Self::TraitTypeBoundNotSatisfied(err) => err.print(table),
            Self::ImplementsFunctionParameterCountMismatch(err) => {
                err.print();
                true
            }
            Self::ImplementsFunctionReturnTypeMismatch(err) => {
                err.print();
                true
            }
            Self::ImplementsFunctionParameterTypeMismatch(err) => {
                err.print();
                true
            }
        }
    }
}
