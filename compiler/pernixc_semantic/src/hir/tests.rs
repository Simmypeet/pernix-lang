use std::{path::PathBuf, sync::Arc};

use pernixc_source::SourceFile;
use pernixc_syntax::target_parsing::{AllParsingError, TargetParsing};
use pernixc_system::error_handler::ErrorVec;

use crate::{
    hir::{AllHirError, Hir},
    symbol::{error::Error as SymbolError, table::Table},
};

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn auto_move_test() -> Result<(), Box<dyn std::error::Error>> {
    let source_file = SourceFile::load(
        &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resource/hir/autoMove/main.pnx"),
        vec!["test".to_string()],
    )?;

    let errors: ErrorVec<AllParsingError> = ErrorVec::new();
    let target = TargetParsing::parse(source_file, &errors)?;
    assert_eq!(errors.into_vec().len(), 0);

    let errors: ErrorVec<SymbolError> = ErrorVec::new();
    let table = Arc::new(Table::analyze(target, &errors));
    assert_eq!(errors.into_vec().len(), 0);

    let overload_set = table.get_overload_set(
        table
            .get_global_id_by_full_name(["test", "main"].into_iter())
            .unwrap()
            .into_overload_set()
            .unwrap(),
    )?;
    let overload_id = overload_set.overloads()[0];
    let errors: ErrorVec<AllHirError> = ErrorVec::new();

    let hir = Hir::bind(&table, overload_id, &errors);
    dbg!(errors.as_vec());
    dbg!(hir);

    Ok(())
}
