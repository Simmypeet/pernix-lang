//! Contains the definitions of the semantic errors related to the binding/HIR building phase.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_print::LogSeverity;
use pernixc_source::Span;
use pernixc_system::arena::InvalidIDError;

use crate::{
    cfg::BasicBlockID,
    infer::{Constraint, InferableType},
    symbol::{
        table::Table,
        ty::{PrimitiveType, Type},
        FieldID, GlobalID, OverloadID, OverloadSetID, ScopedID, StructID,
    },
};

/// Is a semantic error that occurs during the binding/HIR building phase.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Error {
    InvalidNumericLiteralSuffix(InvalidNumericLiteralSuffix),
    FloatingPointLiteralHasIntegralSuffix(FloatingPointLiteralHasIntegralSuffix),
    SymbolNotCallable(SymbolNotCallable),
    NoAccessibleOverload(NoAccessibleOverload),
    NoOverloadWithMatchingNumberOfArguments(NoOverloadWithMatchingNumberOfArguments),
    NoOverloadWithMatchingArgumentTypes(NoOverloadWithMatchingArgumentTypes),
    TypeMismatch(TypeMismatch),
    AmbiguousFunctionCall(AmbiguousFunctionCall),
    ValueExpected(ValueExpected),
    UninitializedFields(UninitializedFields),
    FieldInaccessible(FieldInaccessible),
    DuplicateFieldInitialization(DuplicateFieldInitialization),
    StructExpected(StructExpected),
    UnknownField(UnknownField),
    NoFieldOnType(NoFieldOnType),
    LValueExpected(LValueExpected),
    MutableLValueExpected(MutableLValueExpected),
    ExpressOutsideBlock(ExpressOutsideBlock),
    NoBlockWithGivenLabelFound(NoBlockWithGivenLabelFound),
    NotAllFlowPathExpressValue(NotAllFlowPathExpressValue),
    ReturnValueExpected(ReturnValueExpected),
    LoopControlExressionOutsideLoop(LoopControlExressionOutsideLoop),
    NoCastAvailable(NoCastAvailable),
}

impl Error {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        match self {
            Self::InvalidNumericLiteralSuffix(err) => err.print(),
            Self::FloatingPointLiteralHasIntegralSuffix(err) => err.print(),
            Self::SymbolNotCallable(err) => err.print(table)?,
            Self::NoAccessibleOverload(err) => err.print(table)?,
            Self::NoOverloadWithMatchingNumberOfArguments(err) => err.print(table)?,
            Self::NoOverloadWithMatchingArgumentTypes(err) => err.print(table)?,
            Self::TypeMismatch(err) => err.print(table)?,
            Self::AmbiguousFunctionCall(err) => err.print(table)?,
            Self::ValueExpected(err) => err.print(),
            Self::UninitializedFields(err) => err.print(table)?,
            Self::FieldInaccessible(err) => err.print(table)?,
            Self::DuplicateFieldInitialization(err) => err.print(table)?,
            Self::StructExpected(err) => err.print(table)?,
            Self::UnknownField(err) => err.print(table)?,
            Self::NoFieldOnType(err) => err.print(table)?,
            Self::LValueExpected(err) => err.print(),
            Self::MutableLValueExpected(err) => err.print(),
            Self::ExpressOutsideBlock(err) => err.print(),
            Self::NoBlockWithGivenLabelFound(err) => err.print(),
            Self::NotAllFlowPathExpressValue(err) => err.print(),
            Self::ReturnValueExpected(err) => err.print(),
            Self::LoopControlExressionOutsideLoop(err) => err.print(),
            Self::NoCastAvailable(err) => err.print(table)?,
        };

        Ok(())
    }
}

/// The numeric literal suffix is not applicable to the literal's type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct InvalidNumericLiteralSuffix {
    /// Specifies the location of the numeric literal suffix.
    #[get = "pub"]
    pub(super) suffix_span: Span,
}

impl InvalidNumericLiteralSuffix {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            &format!("invalid numeric literal suffix {}", self.suffix_span.str()),
        );

        pernixc_print::print_source_code(&self.suffix_span, None);
    }
}

/// The numeric literal suffix is for integral types, but it's applied to a floating point literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct FloatingPointLiteralHasIntegralSuffix {
    /// Specifies the location of the floating point literal.
    #[get = "pub"]
    pub(super) floating_point_span: Span,
}

impl FloatingPointLiteralHasIntegralSuffix {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "floating point literal has integral suffix",
        );

        pernixc_print::print_source_code(&self.floating_point_span, None);
    }
}

/// The symbol is not callable.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct SymbolNotCallable {
    /// The symbol that was attempted to be called.
    #[get_copy = "pub"]
    pub(super) found_id: GlobalID,

    /// Specifies the location where the symbol was attempted to be called.
    #[get = "pub"]
    pub(super) symbol_span: Span,
}

fn symbol_kind_string(global_id: GlobalID) -> &'static str {
    match global_id {
        GlobalID::Module(..) => "module",
        GlobalID::Struct(..) => "struct",
        GlobalID::Enum(..) => "enum",
        GlobalID::EnumVariant(..) => "enum variant",
        GlobalID::OverloadSet(..) => "function",
        GlobalID::TypeAlias(..) => "type",
    }
}
impl SymbolNotCallable {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let symbol_kind = symbol_kind_string(self.found_id);
        let symbol_name = table.get_full_name_of(self.found_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!("{symbol_kind} `{symbol_name}` is not callable"),
        );

        pernixc_print::print_source_code(&self.symbol_span, None);

        Ok(())
    }
}

/// No overload accessible for resolving the call.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NoAccessibleOverload {
    /// The overload set that contains all inaccessible overloads.
    #[get_copy = "pub"]
    pub(super) overload_set_id: OverloadSetID,

    /// Specifies the location where the symbol was attempted to be accessed.
    #[get = "pub"]
    pub(super) symbol_span: Span,
}

impl NoAccessibleOverload {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let symbol_name = table.get_full_name_of(self.overload_set_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!("no accessible overload for `{symbol_name}`"),
        );

        pernixc_print::print_source_code(&self.symbol_span, None);

        Ok(())
    }
}

/// No overload with matching number of arguments found.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NoOverloadWithMatchingNumberOfArguments {
    /// The overload set that the function call was attempted to resolve to.
    #[get_copy = "pub"]
    pub(super) overload_set_id: OverloadSetID,

    /// The number of arguments that were passed to the overload set.
    #[get_copy = "pub"]
    pub(super) argument_count: usize,

    /// Specifies the location where the symbol was attempted to be resolved.
    #[get = "pub"]
    pub(super) symbol_span: Span,
}

impl NoOverloadWithMatchingNumberOfArguments {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let symbol_name = table.get_full_name_of(self.overload_set_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!("no overload with matching number of arguments for `{symbol_name}`",),
        );

        pernixc_print::print_source_code(&self.symbol_span, None);

        Ok(())
    }
}

/// No overload with matching argument types found.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NoOverloadWithMatchingArgumentTypes {
    /// The overload set that the function call was attempted to resolve to.
    #[get_copy = "pub"]
    pub(super) overload_set_id: OverloadSetID,

    /// Specifes the location where the symbol was attempted to be resolved.
    #[get = "pub"]
    pub(super) symbol_span: Span,

    /// The types of the arguments that were passed to the overload set.
    #[get = "pub"]
    pub(super) argument_types: Vec<InferableType>,
}

impl NoOverloadWithMatchingArgumentTypes {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let symbol_name = table.get_full_name_of(self.overload_set_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!("no overload with matching argument types for `{symbol_name}`",),
        );

        pernixc_print::print_source_code(&self.symbol_span, None);

        Ok(())
    }
}

/// Multiple overloads are available to be called.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct AmbiguousFunctionCall {
    /// The span of the function call.
    #[get = "pub"]
    pub(super) function_call_span: Span,

    /// The list of overloads that are available to be called.
    #[get = "pub"]
    pub(super) candidate_overloads: Vec<OverloadID>,

    /// The overload set that the function call was attempted to resolve to.
    #[get_copy = "pub"]
    pub(super) overload_set_id: OverloadSetID,
}

impl AmbiguousFunctionCall {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let symbol_name = table.get_full_name_of(self.overload_set_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!("ambiguous function call to `{symbol_name}`",),
        );

        pernixc_print::print_source_code(&self.function_call_span, None);

        Ok(())
    }
}

/// The type of an expression does not match the expected type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct TypeMismatch {
    /// The span of the expression.
    #[get = "pub"]
    pub(super) expression_span: Span,

    /// The expected type.
    #[get_copy = "pub"]
    pub(super) expected: InferableType,

    /// The type that was found.
    #[get_copy = "pub"]
    pub(super) found: InferableType,
}

fn inferable_type_to_string(
    inferable_type: InferableType,
    table: &Table,
) -> Result<String, InvalidIDError> {
    Ok(match inferable_type {
        InferableType::Type(ty) => match ty {
            Type::PrimitiveType(primitive_type) => match primitive_type {
                PrimitiveType::Void => "void",
                PrimitiveType::Bool => "bool",
                PrimitiveType::Float32 => "float32",
                PrimitiveType::Float64 => "float64",
                PrimitiveType::Int8 => "int8",
                PrimitiveType::Int16 => "int16",
                PrimitiveType::Int32 => "int32",
                PrimitiveType::Int64 => "int64",
                PrimitiveType::Uint8 => "uint8",
                PrimitiveType::Uint16 => "uint16",
                PrimitiveType::Uint32 => "uint32",
                PrimitiveType::Uint64 => "uint64",
            }
            .to_string(),
            Type::TypedID(type_id) => table.get_full_name_of(type_id)?,
        },
        InferableType::Constraint(constraint) => match constraint {
            Constraint::All => "{all}",
            Constraint::PrimitiveType => "{primitive type}",
            Constraint::Number => "{number}",
            Constraint::Signed => "{signed number}",
            Constraint::Float => "{float}",
        }
        .to_string(),
    })
}

impl TypeMismatch {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let expected = inferable_type_to_string(self.expected, table)?;
        let found = inferable_type_to_string(self.found, table)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!("expected type `{expected}`, found `{found}`",),
        );

        pernixc_print::print_source_code(&self.expression_span, None);

        Ok(())
    }
}

/// Expected a value from symbol resolution, but got a non-value symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct ValueExpected {
    /// The span of the expression.
    #[get = "pub"]
    pub(super) expression_span: Span,

    /// The found symbol that was not a value.
    #[get_copy = "pub"]
    pub(super) found_symbol: GlobalID,
}

impl ValueExpected {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self) {
        let symbol_string = symbol_kind_string(self.found_symbol);

        pernixc_print::print(
            LogSeverity::Error,
            &format!("expected a value, found `{symbol_string}`"),
        );

        pernixc_print::print_source_code(&self.expression_span, None);
    }
}

/// Not all fields of a struct literal were initialized.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct UninitializedFields {
    /// The span of the struct literal.
    #[get = "pub"]
    pub(super) struct_literal_span: Span,

    /// The struct that was not fully initialized.
    #[get_copy = "pub"]
    pub(super) struct_id: StructID,

    /// The fields that were not initialized.
    #[get = "pub"]
    pub(super) uninitialized_fields: Vec<FieldID>,
}

impl UninitializedFields {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let struct_name = table.get_full_name_of(self.struct_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!("struct `{struct_name}` is not fully initialized"),
        );

        pernixc_print::print_source_code(&self.struct_literal_span, None);

        Ok(())
    }
}

/// A field of a struct was initialized multiple times.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct DuplicateFieldInitialization {
    /// The span of the duplicate initialization.
    #[get = "pub"]
    pub(super) duplicate_initialization_span: Span,

    /// The span of the previous initialization.
    #[get = "pub"]
    pub(super) previous_initialization_span: Span,

    /// The ID of the field that was initialized multiple times.
    #[get_copy = "pub"]
    pub(super) field_id: FieldID,

    /// The ID of the struct that the field belongs to.
    #[get_copy = "pub"]
    pub(super) struct_id: StructID,
}

impl DuplicateFieldInitialization {
    /// Prints the error message to the console.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let struct_name = table.get_full_name_of(self.struct_id)?;
        let field_name = table.get_field(self.field_id)?.name();

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "field `{field_name}` of struct `{struct_name}` was initialized multiple times",
            ),
        );

        pernixc_print::print_source_code(&self.duplicate_initialization_span, None);
        pernixc_print::print_source_code(&self.previous_initialization_span, None);

        Ok(())
    }
}

/// The field of the struct is not accessible from the current scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct FieldInaccessible {
    /// The ID of the field that was not accessible.
    #[get_copy = "pub"]
    pub(super) field_id: FieldID,

    /// The ID of the struct that the field belongs to.
    #[get_copy = "pub"]
    pub(super) struct_id: StructID,

    /// The span of the field access.
    #[get = "pub"]
    pub(super) field_span: Span,

    /// The ID of the scope that the field was accessed from.
    #[get_copy = "pub"]
    pub(super) current_scope: ScopedID,
}

impl FieldInaccessible {
    /// Prints the error message.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let field_name = table.get_field(self.field_id)?.name();
        let struct_name = table.get_full_name_of(self.struct_id)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "field `{field_name}` of struct `{struct_name}` is not accessible from this scope",
            ),
        );

        pernixc_print::print_source_code(&self.field_span, None);

        Ok(())
    }
}

/// The symbol found in the struct literal syntax is not a struct symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct StructExpected {
    /// The ID of the symbol that was found in the struct literal syntax.
    #[get_copy = "pub"]
    pub(super) found_id: GlobalID,

    /// The span of the symbol that was found in the struct literal syntax.
    #[get = "pub"]
    pub(super) symbol_span: Span,
}

impl StructExpected {
    /// Prints the error message.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let found_name = table.get_full_name_of(self.found_id)?;
        let symbol_string = symbol_kind_string(self.found_id);

        pernixc_print::print(
            LogSeverity::Error,
            &format!("expected struct, found {symbol_string} `{found_name}`"),
        );

        pernixc_print::print_source_code(&self.symbol_span, None);

        Ok(())
    }
}

/// The struct does not have a field with the given name.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct UnknownField {
    /// The ID of the struct that does not have the field.
    #[get_copy = "pub"]
    pub(super) struct_id: StructID,

    /// The name of the field that was not found.
    #[get = "pub"]
    pub(super) field_name_span: Span,
}

impl UnknownField {
    /// Prints the error message.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let struct_name = table.get_full_name_of(self.struct_id)?;
        let field_name = self.field_name_span.str();

        pernixc_print::print(
            LogSeverity::Error,
            &format!("struct `{struct_name}` does not have a field named `{field_name}`"),
        );

        pernixc_print::print_source_code(&self.field_name_span, None);

        Ok(())
    }
}

/// The expression cannot be accessed as a field.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NoFieldOnType {
    /// The span of the expression that was used to access the field.
    #[get = "pub"]
    pub(super) operand_span: Span,

    /// The type of the expression that was used to access the field.
    #[get_copy = "pub"]
    pub(super) operand_type: InferableType,
}

impl NoFieldOnType {
    /// Prints the error message.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let operand_type = inferable_type_to_string(self.operand_type, table)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!("cannot access field on type `{operand_type}`"),
        );

        pernixc_print::print_source_code(&self.operand_span, None);

        Ok(())
    }
}

/// Expected an lvalue, but got an rvalue.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct LValueExpected {
    /// The span of the expression that was used as an lvalue.
    #[get = "pub"]
    pub(super) expression_span: Span,
}

impl LValueExpected {
    /// Prints the error message.
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "expected an lvalue, but got an rvalue");

        pernixc_print::print_source_code(&self.expression_span, None);
    }
}

/// The given lvalue is not mutable.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct MutableLValueExpected {
    /// The span of the expression that was used as an lvalue.
    #[get = "pub"]
    pub(super) expression_span: Span,
}

impl MutableLValueExpected {
    /// Prints the error message.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "expected a mutable lvalue, but got an immutable lvalue",
        );

        pernixc_print::print_source_code(&self.expression_span, None);
    }
}

/// `express` expression has a label that refers to a block that does not exist.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct NoBlockWithGivenLabelFound {
    /// The span of the label in the `express` expression.
    #[get = "pub"]
    pub(super) label_span: Span,
}

impl NoBlockWithGivenLabelFound {
    /// Prints the error message.
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "no block with the given label found");

        pernixc_print::print_source_code(&self.label_span, None);
    }
}

/// `express` expression was used outside the block.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct ExpressOutsideBlock {
    /// The span of the invalid `express` expression.
    #[get = "pub"]
    pub(super) express_span: Span,
}

impl ExpressOutsideBlock {
    /// Prints the error message.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "`express` expression was used outside the block",
        );

        pernixc_print::print_source_code(&self.express_span, None);
    }
}

/// Loop expression (`continue`, `break`) was used outside the loop.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct LoopControlExressionOutsideLoop {
    /// The span of the invalid `loop` expression.
    #[get = "pub"]
    pub(super) loop_control_span: Span,
}

impl LoopControlExressionOutsideLoop {
    /// Prints the error message.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "loop expression (`continue`, `break`) was used outside the loop",
        );

        pernixc_print::print_source_code(&self.loop_control_span, None);
    }
}

///  Not all flow paths in the `block` express a value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct NotAllFlowPathExpressValue {
    /// The span of the `block` expression.
    #[get = "pub"]
    pub(super) block_span: Span,

    /// List of the successors of the `block` expression that do not express a value.
    #[get = "pub"]
    pub(super) missing_value_basic_blocks: Vec<BasicBlockID>,
}

impl NotAllFlowPathExpressValue {
    /// Prints the error message.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "not all flow paths in the `block` express a value",
        );

        pernixc_print::print_source_code(&self.block_span, None);
    }
}

/// Expects a return value for non-void function.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct ReturnValueExpected {
    /// The span of the `return` expression.
    #[get = "pub"]
    pub(super) return_span: Span,
}

impl ReturnValueExpected {
    /// Prints the error message.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "expects a return value for non-void function",
        );

        pernixc_print::print_source_code(&self.return_span, None);
    }
}

/// No cast expression is available for the given types.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct NoCastAvailable {
    /// The span of the `cast` expression.
    #[get = "pub"]
    pub(super) cast_span: Span,

    /// The type of the expression.
    #[get = "pub"]
    pub(super) expression_type: InferableType,

    /// The type to cast to.
    #[get = "pub"]
    pub(super) cast_type: InferableType,
}

impl NoCastAvailable {
    /// Prints the error message.
    #[allow(clippy::missing_errors_doc)]
    pub fn print(&self, table: &Table) -> Result<(), InvalidIDError> {
        let expression_type_string = inferable_type_to_string(self.expression_type, table)?;
        let cast_type_string = inferable_type_to_string(self.cast_type, table)?;

        pernixc_print::print(
            LogSeverity::Error,
            &format!(
                "no cast expression is available for the given types: `{expression_type_string}` \
                 to `{cast_type_string}`"
            ),
        );

        pernixc_print::print_source_code(&self.cast_span, None);

        Ok(())
    }
}
