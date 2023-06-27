//! Contains the definition of various inputs that correspond to the definitions in defined
//! [`crate::syntax_tree::target`] module.

use std::{fmt::Display, path::Path};

use derive_more::From;
use pernixc_lexical::token::input::Identifier;
use pernixc_system::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq,
    strategy::{BoxedStrategy, Strategy},
    test_runner::TestCaseResult,
};

use crate::syntax_tree::{input::AccessModifier, item::input::Item};

/// Represents an input for the [`super::ModulePath`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModulePath {
    /// The first identifier in the module path
    pub first: Identifier,

    /// The rest of the identifiers in the module path
    pub rest: Vec<Identifier>,
}

impl Input for ModulePath {
    type Output = super::ModulePath;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.first.assert(&output.first)?;
        prop_assert_eq!(self.rest.len(), output.rest.len());

        for (input, (_, output)) in self.rest.iter().zip(output.rest.iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl Arbitrary for ModulePath {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::collection::vec(Identifier::arbitrary(), 0..=7),
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

/// Represents an input for the [`super::Using`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Using {
    /// The module path
    pub module_path: ModulePath,
}

impl Input for Using {
    type Output = super::Using;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.module_path.assert(&output.module_path)
    }
}

impl Arbitrary for Using {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
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

/// Represents an input for the [`super::Module`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Module {
    /// The access modifier of the module.
    pub access_modifier: AccessModifier,

    /// The name of the submodule.
    pub identifier: Identifier,
}

impl Input for Module {
    type Output = super::Module;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.access_modifier.assert(&output.access_modifier)?;
        self.identifier.assert(&output.identifier)
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} module {};", self.access_modifier, self.identifier)
    }
}

impl Arbitrary for Module {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (AccessModifier::arbitrary(), Identifier::arbitrary())
            .prop_map(|(access_modifier, identifier)| Self {
                access_modifier,
                identifier,
            })
            .boxed()
    }
}

/// Represents an input for the [`super::Submodule`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Submodule {
    /// The module declaration part of the submodule.
    pub module: Module,

    /// The content of the submodule.
    pub file: File,
}

impl Input for Submodule {
    type Output = super::Submodule;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.module.assert(&output.module)?;
        self.file.assert(&output.file)
    }
}

/// Represents an input for the [`super::File`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct File {
    usings: Vec<Using>,
    submodules: Vec<Submodule>,
    items: Vec<Item>,
}

impl Input for File {
    type Output = super::File;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.usings.assert(&output.usings)?;
        self.submodules.assert(&output.submodules)?;
        self.items.assert(&output.items)
    }
}

impl Arbitrary for File {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        let leaf = (
            proptest::collection::vec(Using::arbitrary(), 0..=4),
            proptest::collection::vec(Item::arbitrary(), 0..=4),
        )
            .prop_map(|(usings, items)| Self {
                usings,
                submodules: Vec::new(),
                items,
            });

        leaf.prop_recursive(4, 16, 4, |inner| {
            (
                proptest::collection::vec(Using::arbitrary(), 0..=4),
                proptest::collection::hash_map(
                    Identifier::arbitrary().prop_map(|mut identifier| {
                        identifier.string = identifier.string.to_lowercase();
                        identifier
                    }),
                    (AccessModifier::arbitrary(), inner),
                    0..=4,
                )
                .prop_filter("filter out submodule named `main`", |modules| {
                    modules.keys().all(|module| module.string != "main")
                }),
                proptest::collection::vec(Item::arbitrary(), 0..=4),
            )
                .prop_map(|(usings, submodules, items)| Self {
                    usings,
                    submodules: submodules
                        .into_iter()
                        .map(|(k, v)| Submodule {
                            module: Module {
                                access_modifier: v.0,
                                identifier: k,
                            },
                            file: v.1,
                        })
                        .collect(),
                    items,
                })
        })
        .boxed()
    }
}

/// Is an error that can occur when calling [`File::create_target()`].
#[derive(Debug, thiserror::Error, From)]
#[allow(missing_docs)]
pub enum TargetCreateError {
    #[error("{0}")]
    Io(std::io::Error),

    #[error("{0}")]
    Fmt(std::fmt::Error),
}

impl File {
    fn create_file(&self, file_path: &Path, is_root: bool) -> Result<(), TargetCreateError> {
        use std::io::Write;

        let mut file = std::fs::File::create(file_path).unwrap();

        // write usings to the file
        for using in &self.usings {
            writeln!(file, "{using}")?;
        }

        if !self.submodules.is_empty() && !is_root {
            // create sub directory
            std::fs::create_dir(file_path.with_extension(""))?;
        }

        // write submodules
        for submodule in &self.submodules {
            submodule.file.create_file(
                &if is_root {
                    file_path
                        .parent()
                        .unwrap()
                        .join(format!("{}.pnx", submodule.module.identifier))
                } else {
                    let mut file_path = file_path.with_extension("");
                    file_path.push(format!("{}.pnx", submodule.module.identifier));
                    file_path
                },
                false,
            )?;

            // write items
            writeln!(&mut file, "{}", submodule.module)?;
        }

        // write items
        for item in &self.items {
            writeln!(&mut file, "{item}")?;
        }

        Ok(())
    }

    /// Creates the target file tree as if this file was the root module file.
    ///
    /// # Errors
    /// [`TargetCreateError`]: If encountered any [`std::io::Error`] or [`std::fmt::Error`].
    ///
    /// # Returns
    /// Returns the created temporary directory containing the target file tree. The root source
    /// file will be named `main.pnx` located in the root directory of the temporary directory.
    pub fn create_target(&self) -> Result<tempfile::TempDir, TargetCreateError> {
        let tempdir = tempfile::tempdir()?;

        self.create_file(&tempdir.path().join("main.pnx"), true)?;

        Ok(tempdir)
    }
}
