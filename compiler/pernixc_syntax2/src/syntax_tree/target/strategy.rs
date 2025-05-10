use std::{collections::HashMap, path::Path, str::FromStr};

use derive_more::From;
use drain_filter_polyfill::VecExt;
use pernixc_lexical::token::KeywordKind;
use pernixc_test_input::Input;
use proptest::{
    prelude::Arbitrary, prop_assert_eq, strategy::Strategy,
    test_runner::TestCaseError,
};

use crate::syntax_tree::{
    self,
    item::{self, module},
    strategy::{self, AccessModifier, IndentDisplay, IndentDisplayItem},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleTree {
    pub signature: Option<AccessModifier>,
    pub module_content: item::module::strategy::Body,
    pub submodules_by_name: HashMap<String, ModuleTree>,
}

impl Input<&super::ModuleTree> for &ModuleTree {
    fn assert(
        self,
        output: &super::ModuleTree,
    ) -> proptest::test_runner::TestCaseResult {
        match (&self.signature, &output.signature) {
            (Some(self_signature), Some(output_signature)) => {
                self_signature.assert(&output_signature.access_modifier)?;
            }
            (None, None) => (),
            _ => {
                return Err(TestCaseError::fail(format!(
                    "expected signature: {:#?}, got: {:#?}",
                    self.signature, output.signature
                )))
            }
        }

        self.module_content.assert(&output.module_content)?;

        prop_assert_eq!(
            self.submodules_by_name.len(),
            output.submodules_by_name.len()
        );

        for (name, submodule) in &self.submodules_by_name {
            let Some(output_submodule) = output.submodules_by_name.get(name)
            else {
                return Err(TestCaseError::fail(format!(
                    "submodule with name '{name}' not found",
                )));
            };

            submodule.assert(output_submodule)?;
        }

        Ok(())
    }
}

impl Arbitrary for ModuleTree {
    type Parameters = ();
    type Strategy = proptest::strategy::BoxedStrategy<Self>;

    #[allow(clippy::redundant_closure)]
    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let module_content =
            module::strategy::Body::arbitrary().prop_map(|mut x| {
                x.members.drain_filter(|x| {
                    x.as_syntax_tree()
                        .is_some_and(module::strategy::Member::is_module)
                });

                x
            });
        let submodule_name = proptest::string::string_regex("[a-z]+")
            .unwrap()
            .prop_filter("filter out keyword", |x| {
                KeywordKind::from_str(x).is_err()
            })
            .boxed();

        let leaf = (
            syntax_tree::strategy::AccessModifier::arbitrary(),
            module_content.clone(),
        )
            .prop_map(|(access_modifier, module_content)| Self {
                signature: Some(access_modifier),
                module_content,
                submodules_by_name: HashMap::new(),
            });

        leaf.prop_recursive(3, 6, 2, move |inner| {
            (
                syntax_tree::strategy::AccessModifier::arbitrary(),
                module_content.clone(),
                proptest::collection::hash_map(
                    submodule_name.clone(),
                    inner,
                    0..=2,
                ),
            )
                .prop_map(
                    |(access_modifier, module_content, submodules_by_name)| {
                        Self {
                            signature: Some(access_modifier),
                            module_content,
                            submodules_by_name,
                        }
                    },
                )
        })
        .boxed()
    }
}

impl IndentDisplay for ModuleTree {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.module_content.indent_fmt(f, indent)?;

        for (name, content) in &self.submodules_by_name {
            writeln!(
                f,
                "{} module {}",
                match content.signature {
                    Some(strategy::AccessModifier::Public) | None => "public",
                    Some(strategy::AccessModifier::Private) => "private",
                    Some(strategy::AccessModifier::Internal) => "internal",
                },
                name
            )?;
        }

        Ok(())
    }
}

#[derive(Debug, thiserror::Error, From)]
#[allow(missing_docs)]
pub enum TargetCreateError {
    #[error(transparent)]
    Io(std::io::Error),

    #[error(transparent)]
    Fmt(std::fmt::Error),
}

impl ModuleTree {
    fn create_file(
        &self,
        file_path: &Path,
        is_root: bool,
    ) -> Result<(), TargetCreateError> {
        use std::io::Write;

        let mut file = std::fs::File::create(file_path).unwrap();

        if !self.submodules_by_name.is_empty() && !is_root {
            std::fs::create_dir(file_path.with_extension(""))?;
        }

        write!(file, "{}", IndentDisplayItem(0, self))?;

        for (name, content) in &self.submodules_by_name {
            content.create_file(
                &if is_root {
                    file_path.parent().unwrap().join(format!("{name}.pnx"))
                } else {
                    let mut file_path = file_path.with_extension("");
                    file_path.push(format!("{name}.pnx"));
                    file_path
                },
                false,
            )?;
        }

        Ok(())
    }

    #[allow(clippy::missing_errors_doc)]
    pub fn create_target(
        &self,
    ) -> Result<tempfile::TempDir, TargetCreateError> {
        let tempdir = tempfile::tempdir()?;

        self.create_file(&tempdir.path().join("main.pnx"), true)?;

        Ok(tempdir)
    }
}
