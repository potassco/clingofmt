#![warn(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]
//! Formatter configuration types loaded from `.clingofmt`.

use serde::{Deserialize, Serialize};

/// User-facing formatter configuration loaded from `.clingofmt`.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
#[serde(default)]
pub struct Config {
    /// Flattened formatter options loaded from the configuration file.
    #[serde(flatten)]
    pub format: FormatOptions,
}

impl Config {
    /// Returns the effective formatter options for this configuration.
    pub fn format_options(&self) -> &FormatOptions {
        &self.format
    }
}

/// Formatting options used by the formatter engine.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(default)]
pub struct FormatOptions {
    /// Whether to prefer a line break after the head of a rule once the soft
    /// flush limit is exceeded.
    pub break_after_head: bool,
    /// Whether to insert a line break after each body atom in a rule.
    pub break_after_body_atom: bool,
    /// Whether to insert a line break after a colon in specific contexts.
    pub break_after_colon: bool,
    /// Whether wrapped continuations after colon-based constructs should align
    /// under the first item after the colon instead of using the normal indent.
    pub align_continuation_after_colon: bool,
    /// Number of spaces to use for each indentation level.
    pub indent_width: usize,
    /// The maximum number of characters before a soft flush occurs.
    pub soft_flush_limit: usize,
}

impl Default for FormatOptions {
    /// Returns the default formatter settings.
    fn default() -> Self {
        Self {
            break_after_head: true,
            break_after_body_atom: false,
            break_after_colon: true,
            align_continuation_after_colon: true,
            indent_width: 4,
            soft_flush_limit: 60,
        }
    }
}
