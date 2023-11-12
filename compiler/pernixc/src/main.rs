use std::process::ExitCode;

use clap::Parser;
use pernixc_driver::Arguments;

fn main() -> ExitCode { pernixc_driver::run(Arguments::parse()) }
