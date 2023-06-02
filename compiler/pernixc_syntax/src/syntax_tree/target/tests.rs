use std::path::Path;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use proptest::proptest;

use super::strategy::FileInput;
use crate::syntax_tree::target::Target;

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn module_path_test(
        module_path_input in super::strategy::module_path()
    ) {
        let source = module_path_input.to_string();

        println!("{source}");

        let module_path = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_module_path(handler)
        )?;

        module_path_input.validate(&module_path)?;
    }

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn using_test(
        using_input in super::strategy::using()
    ) {
        let source = using_input.to_string();

        println!("{source}");

        let using = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_using(handler)
        )?;

        using_input.validate(&using)?;
    }

    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn module_test(
        module_input in super::strategy::module()
    ) {
        let source = module_input.to_string();

        println!("{source}");

        let module = crate::syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_module(handler)
        )?;

        module_input.validate(&module)?;
    }


    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn target_test(
        file_input in super::strategy::file()
    ) {
        let tempdir = tempfile::tempdir()?;
        let mut files = Vec::new();
        create_target(&file_input, tempdir.path(), &mut files);

        {
            let root_source_file = SourceFile::load(&tempdir.path().join("main.pnx"))?;
            let stroage = Storage::<Error>::new();
            let target = Target::parse(root_source_file, &stroage);

            file_input.validate(&target.root_file)?;
        }
    }
}

#[derive(Debug, EnumAsInner, From)]
enum Error {
    Lexical(pernixc_lexical::error::Error),
    Syntax(crate::error::Error),
    Target(crate::syntax_tree::target::Error),
}

fn create_file(
    file_input: &FileInput,
    file_path: &Path,
    is_root: bool,
    files: &mut Vec<std::fs::File>,
) {
    use std::io::Write;

    let mut file = std::fs::File::create(file_path).unwrap();

    for using in &file_input.usings {
        writeln!(&mut file, "{}", using.to_string()).unwrap();
    }

    for submodule in &file_input.submodules {
        create_file(
            &submodule.file,
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
            files,
        );

        writeln!(&mut file, "{}", submodule.module.to_string()).unwrap();
    }

    for item in &file_input.items {
        writeln!(&mut file, "{}", item.to_string()).unwrap();
    }

    files.push(file);
}

fn create_target(file_input: &FileInput, directory_path: &Path, files: &mut Vec<std::fs::File>) {
    create_file(file_input, &directory_path.join("main.pnx"), true, files);
}
