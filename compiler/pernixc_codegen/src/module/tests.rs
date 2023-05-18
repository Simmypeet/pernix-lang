use std::{path::PathBuf, sync::Arc};

use pernixc_semantic::{
    hir::{AllHirError, Target},
    symbol::{error::Error as SymbolError, table::Table},
};
use pernixc_source::SourceFile;
use pernixc_syntax::target_parsing::{AllParsingError, TargetParsing};
use pernixc_system::error_handler::ErrorVec;

use crate::context::Context;

#[test]
#[allow(clippy::too_many_lines)]
pub fn test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/sample/main.pnx"),
        vec!["test".to_string()],
    )?;

    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target = TargetParsing::parse(source_file, &errors)?;
    assert_eq!(errors.into_vec().len(), 0);

    let errors: ErrorVec<SymbolError> = ErrorVec::new();
    let table = Arc::new(Table::analyze(target, &errors));
    assert_eq!(errors.into_vec().len(), 0);

    let errors: ErrorVec<AllHirError> = ErrorVec::new();
    let target = Target::bind(table.clone(), &errors).unwrap();

    let context = Context::new();
    let module = context.create_module("test".to_string(), target);

    for overload_sym in table.overloads() {
        let generator = module.create_generator(overload_sym.id()).unwrap();
    }

    module.dump_module();

    Ok(())
}
