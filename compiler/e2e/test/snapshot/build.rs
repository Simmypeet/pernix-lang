#![allow(missing_docs)]

fn main() {
    // if there's any change in the ./snaphost/ directory, the build script will
    // be executed and the test will be run again.
    println!("cargo:rerun-if-changed=./snapshot/");
}
