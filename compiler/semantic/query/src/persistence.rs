//! Defines the protocol for presisting the query session state into the
//! storage.
//!
//! # Motivation
//!
//! Naive approach of persisting the query session state is to serialize the
//! entire database into a one big file and deserialize it back when needed.
//!
//! However, this approach one biggest problem: it is painfully slow to
//! deserialize the whole database at once, especially for large projects.
//! Moreover, some of the values in the database will be discarded in the new
//! session, so it is a waste of time to deserialize them.
