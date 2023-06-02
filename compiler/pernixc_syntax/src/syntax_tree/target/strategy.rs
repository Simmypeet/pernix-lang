//! Contains various definition of input strategies for the target syntax.

use proptest::{prop_assert_eq, strategy::Strategy, test_runner::TestCaseError};

use super::ModulePath;
use crate::syntax_tree::{item::strategy::ItemInput, strategy::AccessModifierInput};

/// Represents an input for [`super::ModulePath`]
#[derive(Debug, Clone)]
pub struct ModulePathInput {
    /// List of identifiers that make up the path.
    pub identifiers: Vec<String>,
}

impl ToString for ModulePathInput {
    fn to_string(&self) -> String { self.identifiers.join("::") }
}

impl ModulePathInput {
    /// Validates the input against the [`super::ModulePath`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &ModulePath) -> Result<(), TestCaseError> {
        prop_assert_eq!(self.identifiers.len(), output.len());

        for (identifier, output_identifier) in self.identifiers.iter().zip(output.elements()) {
            prop_assert_eq!(identifier, output_identifier.span.str());
        }

        Ok(())
    }
}

/// Represents an input for [`super::Using`]
#[derive(Debug, Clone)]
pub struct UsingInput {
    /// The path to the module.
    pub path: ModulePathInput,
}

impl ToString for UsingInput {
    fn to_string(&self) -> String { format!("using {};", self.path.to_string()) }
}

impl UsingInput {
    /// Validates the input against the [`super::Using`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &super::Using) -> Result<(), TestCaseError> {
        self.path.validate(&output.module_path)?;

        Ok(())
    }
}

/// Returns a strategy that produces [`ModulePathInput`]
pub fn module_path() -> impl Strategy<Value = ModulePathInput> {
    proptest::collection::vec(pernixc_lexical::token::strategy::identifier(), 1..=4)
        .prop_map(|identifiers| ModulePathInput { identifiers })
}

/// Returns a strategy that produces [`UsingInput`]
pub fn using() -> impl Strategy<Value = UsingInput> {
    module_path().prop_map(|path| UsingInput { path })
}

/// Represents an input for [`super::Module`]
#[derive(Debug, Clone)]
pub struct ModuleInput {
    /// The access modifier of the module.
    pub access_modifier: AccessModifierInput,

    /// The identifier of the module.
    pub identifier: String,
}

impl ToString for ModuleInput {
    fn to_string(&self) -> String { format!("module {};", self.identifier) }
}

impl ModuleInput {
    /// Validates the input against the [`super::Module`] output.
    #[allow(clippy::missing_errors_doc)]
    pub fn validate(&self, output: &super::Module) -> Result<(), TestCaseError> {
        prop_assert_eq!(&self.identifier, output.identifier.span.str());

        self.access_modifier.validate(&output.access_modifier)?;

        Ok(())
    }
}

/// Returns a strategy that produces [`ModuleInput`]
pub fn module() -> impl Strategy<Value = ModuleInput> {
    (
        crate::syntax_tree::strategy::access_modifier(),
        pernixc_lexical::token::strategy::identifier(),
    )
        .prop_map(|(access_modifier, identifier)| ModuleInput {
            access_modifier,
            identifier,
        })
}

/// Represents an input for [`super::Submodule`]
#[derive(Debug, Clone)]
pub struct SubmoduleInput {
    /// The module declaration syntax tree of the submodule.
    pub module: ModuleInput,

    /// The content of the submodule.
    pub file: FileInput,
}

/// Returns a strategy that produces [`super::File`]
#[derive(Debug, Clone)]
pub struct FileInput {
    /// List of using statements.
    pub usings: Vec<UsingInput>,

    /// List of submodules.
    pub submodules: Vec<SubmoduleInput>,

    /// List of items.
    pub items: Vec<ItemInput>,
}

/// Returns a strategy that produces [`super::File`]
pub fn file() -> impl Strategy<Value = FileInput> {
    (
        proptest::collection::vec(using(), 0..=4),
        proptest::collection::vec(crate::syntax_tree::item::strategy::item(), 0..=4),
    )
        .prop_map(|(usings, items)| FileInput {
            usings,
            submodules: Vec::new(),
            items,
        })
        .prop_recursive(4, 16, 4, |inner| {
            (
                proptest::collection::vec(using(), 0..=4),
                proptest::collection::hash_map(
                    pernixc_lexical::token::strategy::identifier(),
                    (crate::syntax_tree::strategy::access_modifier(), inner),
                    0..=4,
                ),
                proptest::collection::vec(crate::syntax_tree::item::strategy::item(), 0..=4),
            )
                .prop_map(|(usings, submodules, items)| FileInput {
                    usings,
                    submodules: submodules
                        .into_iter()
                        .map(|(k, v)| SubmoduleInput {
                            module: ModuleInput {
                                access_modifier: v.0,
                                identifier: k,
                            },
                            file: v.1,
                        })
                        .collect(),
                    items,
                })
        })
}
