#![warn(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]
//! Parser setup for `tree-sitter-clingo`.

use anyhow::{anyhow, Context, Result};
use tree_sitter::{Parser, Tree};

/// Parses clingo source code into a tree-sitter syntax tree.
pub fn parse_source(source_code: &[u8]) -> Result<Tree> {
    let mut parser = Parser::new();
    let language = tree_sitter_clingo::LANGUAGE.into();
    parser
        .set_language(&language)
        .context("Error loading clingo grammar")?;

    parser
        .parse(source_code, None)
        .ok_or_else(|| anyhow!("Error parsing clingo source"))
}
