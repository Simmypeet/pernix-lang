use pernixc_driver::argument::{Compilation, Parser};

fn main() {
    let arg = Compilation::parse();

    println!("{arg:?}")
}
