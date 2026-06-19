#![warn(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]
//! Public library entry points for parsing and formatting clingo source code.

/// Configuration loading and formatter option types.
mod config;
/// Core formatting engine.
mod formatter;
/// Tree-sitter parser construction helpers.
mod parser;

/// User-facing formatter configuration loaded from `.clingofmt`.
pub use config::{Config, FormatOptions};
/// Formatting entry points for parsed trees and raw source text.
pub use formatter::{format_program, format_source};
/// Parses clingo source text into a tree-sitter syntax tree.
pub use parser::parse_source;

#[cfg(test)]
/// Unit tests covering formatter behavior.
mod tests;
