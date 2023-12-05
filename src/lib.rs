#![crate_name = "filtr"]
#![cfg_attr(all(doc, CHANNEL_NIGHTLY), feature(doc_auto_cfg))]

//! Data structures and tools for representing and evaluating predicate trees.
//!
//! It's a less-sophisticated version of [predicates](https://docs.rs/predicates/latest/predicates) with a focus on a simple recursive
//! structure for representing predicate trees ([`PredicateTree`]). This structure is designed with serialisability in mind - whether
//! via serde, or otherwise.
//!
//! ## Feature flags
//!
//! - `serde`: Enables serialization and deserialization of `PredicateTree` via serde.
#![doc(html_root_url = "https://docs.rs/filtr/0.1.0/")]

mod predicate;
mod tree;

pub use predicate::*;
pub use tree::*;
