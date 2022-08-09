#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::missing_errors_doc, clippy::must_use_candidate)]
#![allow(
    clippy::cast_possible_wrap,
    clippy::missing_panics_doc,
    clippy::use_self,
    clippy::module_name_repetitions,
    clippy::vec_init_then_push,
    clippy::large_enum_variant
)]
pub mod analyze;
pub mod common;
pub mod error;
pub mod generate;
pub mod parse;
pub mod tokenize;
