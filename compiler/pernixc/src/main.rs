use pernixc_driver::{Argument, Parser};

fn main() {
    let argument = Argument::parse();
    pernixc_driver::run(&argument)
}
