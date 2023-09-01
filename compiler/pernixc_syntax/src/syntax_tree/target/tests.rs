use std::{collections::HashMap, fmt::Display, path::Path, str::FromStr};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::KeywordKind;
use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use pernixc_tests::input::Input;
use proptest::{
    prelude::Arbitrary, prop_assert_eq, proptest, strategy::Strategy, test_runner::TestCaseError,
};

use crate::syntax_tree::{self, item, target::Target, tests::AccessModifier};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleTree {
    signature: Option<AccessModifier>,
    module_content: item::tests::ModuleContent,
    submodules_by_name: HashMap<String, ModuleTree>,
}

impl Input for ModuleTree {
    type Output = super::ModuleTree;

    fn assert(&self, output: &Self::Output) -> proptest::test_runner::TestCaseResult {
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
            let Some(output_submodule) = output.submodules_by_name.get(name) else {
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

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        let module_content = item::tests::ModuleContent::arbitrary_with(Some(
            item::tests::Item::arbitrary_with(true),
        ));
        let submodule_name = proptest::string::string_regex("[a-z]+")
            .unwrap()
            .prop_filter("filter out keyword", |x| KeywordKind::from_str(x).is_err())
            .boxed();

        let leaf = (
            syntax_tree::tests::AccessModifier::arbitrary(),
            module_content.clone(),
        )
            .prop_map(|(access_modifier, module_content)| Self {
                signature: Some(access_modifier),
                module_content,
                submodules_by_name: HashMap::new(),
            });

        leaf.prop_recursive(3, 6, 2, move |inner| {
            (
                syntax_tree::tests::AccessModifier::arbitrary(),
                module_content.clone(),
                proptest::collection::hash_map(submodule_name.clone(), inner, 0..=2),
            )
                .prop_map(|(access_modifier, module_content, submodules_by_name)| {
                    Self {
                        signature: Some(access_modifier),
                        module_content,
                        submodules_by_name,
                    }
                })
        })
        .boxed()
    }
}

impl Display for ModuleTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use syntax_tree::tests::AccessModifier::*;

        write!(f, "{}", self.module_content)?;

        for (name, content) in &self.submodules_by_name {
            writeln!(
                f,
                "{} module {};",
                match content.signature {
                    Some(Public) | None => "public",
                    Some(Private) => "private",
                    Some(Internal) => "internal",
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
    #[error("{0}")]
    Io(std::io::Error),

    #[error("{0}")]
    Fmt(std::fmt::Error),
}

impl ModuleTree {
    fn create_file(&self, file_path: &Path, is_root: bool) -> Result<(), TargetCreateError> {
        use std::io::Write;

        let mut file = std::fs::File::create(file_path).unwrap();

        if !self.submodules_by_name.is_empty() && !is_root {
            std::fs::create_dir(file_path.with_extension(""))?;
        }

        write!(file, "{self}")?;

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

    pub fn create_target(&self) -> Result<tempfile::TempDir, TargetCreateError> {
        let tempdir = tempfile::tempdir()?;

        self.create_file(&tempdir.path().join("main.pnx"), true)?;

        Ok(tempdir)
    }
}

proptest! {

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn target_test(
        mut target_module_tree in ModuleTree::arbitrary()
    ) {
        target_module_tree.signature = None;

        let target_dir = target_module_tree.create_target()?;
        let root_source_file = SourceFile::load(&target_dir.path().join("main.pnx"))?;
        let storage = Storage::<Error>::new();
        let target = Target::parse(&root_source_file, "test".to_string(), &storage);

        if !storage.as_vec().is_empty() {
            return Err(TestCaseError::fail(format!("parsing error: {:#?}",storage.as_vec())));
        }

        target_module_tree.assert(target.module_tree())?;
    }
}

#[derive(Debug, EnumAsInner, From)]
enum Error {
    Lexical(pernixc_lexical::error::Error),
    Syntax(crate::error::Error),
    Target(syntax_tree::target::Error),
}
