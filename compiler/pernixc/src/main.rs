use std::process::ExitCode;

use clap::Parser;
use pernixc_driver::Arguments;

fn main() -> ExitCode {
    pernixc_driver::run(Arguments {
        file: "example/typeAlias.pnx".into(),
        target_name: None,
    })
}
