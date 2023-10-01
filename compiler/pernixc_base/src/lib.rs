//! Contains the code related to the basic functionality of the Pernix compiler.

#![deny(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

pub mod diagnostic;
pub mod log;
pub mod source_file;
