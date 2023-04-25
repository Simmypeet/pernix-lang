//! The `pernixc_common` crate provides common types and utilities used by the other crates of the
//! compiler. This crate is meant to be used by the other crates of the compiler, but it is not
//! meant to be used by the end-user.

#![deny(
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

pub mod printing;
pub mod source_file;
