#![warn(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]
//! Core formatting engine for clingo syntax trees.

use crate::{parser::parse_source, FormatOptions};
use anyhow::Result;
use log::{debug, warn};
use std::io::Write;
use std::ops::{Deref, DerefMut};

/// Derived formatting flags used while traversing a single statement.
#[derive(Copy, Clone, Debug, Default)]
struct FormattingPolicy {
    /// Whether head breaks should be preferred once the soft limit is exceeded.
    break_after_head: bool,
    /// Whether body-atom separators should trigger immediate hard breaks.
    break_after_body_atom: bool,
    /// Whether colon-based constructs should break immediately.
    break_after_colon: bool,
    /// Whether continuations after colons should align to the following token.
    align_continuation_after_colon: bool,
    /// Number of spaces to use for each logical indentation level.
    indent_width: usize,
    /// Maximum buffered line length before a soft wrap is flushed.
    soft_flush_limit: usize,
}

impl From<&FormatOptions> for FormattingPolicy {
    /// Builds a traversal policy from user-facing formatter options.
    fn from(options: &FormatOptions) -> Self {
        Self {
            break_after_head: options.break_after_head,
            break_after_body_atom: options.break_after_body_atom,
            break_after_colon: options.break_after_colon,
            align_continuation_after_colon: options.align_continuation_after_colon,
            indent_width: options.indent_width,
            soft_flush_limit: options.soft_flush_limit,
        }
    }
}

impl FormattingPolicy {
    /// Returns whether a colon in the current state should trigger a hard break.
    fn should_break_after_colon(self, state: &StatementState) -> bool {
        self.break_after_colon && (state.in_conjunction || state.in_optcondition)
    }

    /// Returns whether a top-level body separator should trigger a hard break.
    fn should_break_after_body_atom(self, state: &StatementState) -> bool {
        self.break_after_body_atom && state.in_termvec == 0 && !state.is_show
    }

    /// Returns whether a rule head break should be preferred for this state.
    fn should_break_after_head(self, state: &StatementState) -> bool {
        state.has_head_like && self.break_after_head
    }
}

/// A source slice that can either be formatted normally or copied verbatim.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum FormatSegment<'a> {
    /// Source that should be parsed and formatted normally.
    Formattable(&'a [u8]),
    /// Source that should be written exactly as it appeared in the input.
    Raw(&'a [u8]),
}

/// Formatter control marker found on a standalone line.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum FormatMarker {
    /// Starts a raw, unformatted region.
    Off,
    /// Ends a raw, unformatted region.
    On,
}

/// Lightweight lexical state used before parsing to avoid recognizing formatter
/// control markers inside strings or block comments.
#[derive(Debug, Default)]
struct FormatMarkerScanner {
    /// Whether the current scan position is inside a quoted string.
    in_string: bool,
    /// Whether the current scan position is inside a `%* ... *%` block comment.
    in_block_comment: bool,
}

impl FormatMarkerScanner {
    /// Returns whether a marker may be recognized at the start of the line.
    fn allows_marker(&self) -> bool {
        !self.in_string && !self.in_block_comment
    }

    /// Updates lexical state using one complete logical line.
    fn scan_line(&mut self, line: &[u8]) {
        let mut escaped = false;
        let mut index = 0;
        while index < line.len() {
            if self.in_string {
                match line[index] {
                    b'\\' if !escaped => escaped = true,
                    b'"' if !escaped => self.in_string = false,
                    _ => escaped = false,
                }
                index += 1;
            } else if self.in_block_comment {
                if line[index] == b'*' && line.get(index + 1) == Some(&b'%') {
                    self.in_block_comment = false;
                    index += 2;
                } else {
                    index += 1;
                }
            } else {
                match line[index] {
                    b'"' => {
                        self.in_string = true;
                        index += 1;
                    }
                    b'%' if line.get(index + 1) == Some(&b'*') => {
                        self.in_block_comment = true;
                        index += 2;
                    }
                    b'%' => break,
                    _ => index += 1,
                }
            }
        }
    }
}

/// Splits source into formattable and raw regions according to `%% fmt` markers.
fn split_format_segments(source_code: &[u8]) -> Vec<FormatSegment<'_>> {
    let mut segments = Vec::new();
    let mut scanner = FormatMarkerScanner::default();
    let mut raw_mode = false;
    let mut segment_start = 0;
    let mut line_start = 0;

    while line_start < source_code.len() {
        let line_end = next_line_end(source_code, line_start);
        let line = &source_code[line_start..line_end];
        let marker = scanner
            .allows_marker()
            .then(|| detect_format_marker(line))
            .flatten();

        match (raw_mode, marker) {
            (false, Some(FormatMarker::Off)) => {
                push_segment(
                    &mut segments,
                    FormatSegment::Formattable(&source_code[segment_start..line_start]),
                );
                raw_mode = true;
                segment_start = line_start;
            }
            (true, Some(FormatMarker::On)) => {
                push_segment(
                    &mut segments,
                    FormatSegment::Raw(&source_code[segment_start..line_end]),
                );
                raw_mode = false;
                segment_start = line_end;
            }
            _ => {}
        }

        scanner.scan_line(line);
        line_start = line_end;
    }

    if segment_start < source_code.len() {
        let segment = if raw_mode {
            FormatSegment::Raw(&source_code[segment_start..])
        } else {
            FormatSegment::Formattable(&source_code[segment_start..])
        };
        push_segment(&mut segments, segment);
    }

    segments
}

/// Adds a non-empty segment to the segment list.
fn push_segment<'a>(segments: &mut Vec<FormatSegment<'a>>, segment: FormatSegment<'a>) {
    match segment {
        FormatSegment::Formattable(bytes) | FormatSegment::Raw(bytes) if bytes.is_empty() => {}
        _ => segments.push(segment),
    }
}

/// Returns the byte index just after the current logical line ending.
fn next_line_end(source_code: &[u8], line_start: usize) -> usize {
    source_code[line_start..]
        .iter()
        .position(|byte| *byte == b'\n')
        .map_or(source_code.len(), |offset| line_start + offset + 1)
}

/// Detects supported standalone formatter control marker comments.
fn detect_format_marker(line: &[u8]) -> Option<FormatMarker> {
    let line = trim_ascii(line);
    let mut index = 0;

    if !consume_exact(line, &mut index, b"%%") || !consume_ascii_whitespace(line, &mut index) {
        return None;
    }
    skip_ascii_whitespace(line, &mut index);
    if !consume_exact(line, &mut index, b"fmt") {
        return None;
    }

    skip_ascii_whitespace(line, &mut index);
    if !consume_exact(line, &mut index, b":") {
        return None;
    }
    skip_ascii_whitespace(line, &mut index);

    if consume_exact(line, &mut index, b"off") && index == line.len() {
        Some(FormatMarker::Off)
    } else if consume_exact(line, &mut index, b"on") && index == line.len() {
        Some(FormatMarker::On)
    } else {
        None
    }
}

/// Removes leading and trailing ASCII whitespace from a byte slice.
fn trim_ascii(bytes: &[u8]) -> &[u8] {
    let start = bytes
        .iter()
        .position(|byte| !byte.is_ascii_whitespace())
        .unwrap_or(bytes.len());
    let end = bytes
        .iter()
        .rposition(|byte| !byte.is_ascii_whitespace())
        .map_or(start, |index| index + 1);
    &bytes[start..end]
}

/// Advances over an exact byte token if it is present.
fn consume_exact(bytes: &[u8], index: &mut usize, token: &[u8]) -> bool {
    if bytes
        .get(*index..)
        .is_some_and(|rest| rest.starts_with(token))
    {
        *index += token.len();
        true
    } else {
        false
    }
}

/// Consumes one ASCII whitespace byte when present.
fn consume_ascii_whitespace(bytes: &[u8], index: &mut usize) -> bool {
    if bytes.get(*index).is_some_and(u8::is_ascii_whitespace) {
        *index += 1;
        true
    } else {
        false
    }
}

/// Advances past any ASCII whitespace bytes.
fn skip_ascii_whitespace(bytes: &[u8], index: &mut usize) {
    while consume_ascii_whitespace(bytes, index) {}
}

/// Mutable traversal state for formatting a single statement.
struct StatementState {
    /// Whether the current statement is a `#show`-style directive.
    is_show: bool,
    /// Whether the current statement is an `#include` directive.
    is_include: bool,
    /// Whether the traversal has entered a rule head-like field.
    has_head_like: bool,
    /// Whether a `:-` has been encountered for the current rule.
    has_if: bool,
    /// Whether the statement contains a body.
    has_body: bool,
    /// Whether the traversal is currently inside a conjunction-like condition.
    in_conjunction: bool,
    /// Whether the traversal is inside an optional condition construct.
    in_optcondition: bool,
    /// Nesting depth inside term vectors and related term collections.
    in_termvec: usize,
    /// Whether the traversal is inside a theory atom definition.
    in_theory_atom_definition: bool,
}

impl StatementState {
    /// Creates initial traversal state for a top-level statement kind.
    fn new(statement_kind: &str) -> Self {
        Self {
            is_show: matches!(statement_kind, "show" | "show_signature" | "show_term"),
            is_include: statement_kind == "include",
            has_head_like: false,
            has_if: false,
            has_body: false,
            in_conjunction: false,
            in_optcondition: false,
            in_termvec: 0,
            in_theory_atom_definition: false,
        }
    }

    /// Records semantic context when entering a syntax node.
    fn enter_node(&mut self, field_name: Option<&str>, kind: &str) {
        if field_name == Some("head") {
            self.has_head_like = true;
        }
        if field_name == Some("body") {
            self.has_body = true;
        }

        match kind {
            "body" => self.has_body = true,
            "body_aggregate_element"
            | "head_aggregate_element"
            | "conditional_literal"
            | "optimize_element" => {
                self.in_optcondition = true;
            }
            "terms" | "theory_terms" => self.in_termvec += 1,
            "theory_atom_definition" => {
                self.in_termvec += 1;
                self.in_theory_atom_definition = true;
            }
            _ => {}
        }
    }

    /// Records semantic context when leaving a syntax node.
    fn leave_node(&mut self, kind: &str) {
        match kind {
            "terms" | "theory_terms" => self.in_termvec = self.in_termvec.saturating_sub(1),
            "theory_atom_definition" => {
                self.in_termvec = self.in_termvec.saturating_sub(1);
                self.in_theory_atom_definition = false;
            }
            "body_aggregate_element"
            | "head_aggregate_element"
            | "conditional_literal"
            | "optimize_element" => {
                self.in_optcondition = false;
            }
            "condition" => self.in_conjunction = false,
            _ => {}
        }
    }
}

/// Describes how wrapped continuation lines should be indented.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ContinuationIndent {
    /// Indent by a whole-number multiple of the configured indent width.
    Level(usize),
    /// Indent to an exact visible column width.
    Align(usize),
}

/// Prioritizes structural wrap points so higher-level body breaks can win over
/// nested tuple or term breaks when a line exceeds the soft limit.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum SoftBreakPriority {
    /// A break inside a nested term, tuple, or similarly local construct.
    Nested,
    /// A break at a higher-level rule-body boundary such as a top-level comma.
    TopLevel,
}

/// Records a possible soft-wrap location in the buffered output together with
/// its indentation and structural priority.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct SoftBreakCandidate {
    /// Byte position in the current output buffer where the wrap can occur.
    pos: usize,
    /// Indentation mode to apply after wrapping at this candidate.
    indent: ContinuationIndent,
    /// Relative preference of this wrap point against other candidates.
    priority: SoftBreakPriority,
}

/// Statement-local output buffer used while formatting one top-level statement.
#[derive(Debug, Default)]
struct OutputBuffer {
    /// Raw bytes accumulated for the current statement line fragment.
    bytes: Vec<u8>,
}

impl OutputBuffer {
    /// Creates an empty statement output buffer.
    fn new() -> Self {
        Self { bytes: Vec::new() }
    }

    /// Returns the buffered bytes.
    fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Returns the current visible character width of the buffer.
    fn visible_len(&self) -> Result<usize> {
        buffer_len(&self.bytes)
    }
}

impl Write for OutputBuffer {
    /// Appends bytes to the statement buffer.
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.bytes.write(buf)
    }

    /// Flushes the in-memory buffer.
    fn flush(&mut self) -> std::io::Result<()> {
        self.bytes.flush()
    }
}

impl Deref for OutputBuffer {
    type Target = Vec<u8>;

    /// Exposes byte-vector read operations while buffer users are migrated.
    fn deref(&self) -> &Self::Target {
        &self.bytes
    }
}

impl DerefMut for OutputBuffer {
    /// Exposes byte-vector mutation operations while buffer users are migrated.
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bytes
    }
}

/// Owns soft-wrap candidates and multiline wrap mode for one statement.
#[derive(Debug, Default)]
struct SoftWrapEngine {
    /// Candidate most recently made active for soft-limit checking.
    active: Option<SoftBreakCandidate>,
    /// Preferred candidate when a nested break should yield to a higher-level break.
    preferred: Option<SoftBreakCandidate>,
    /// Deferred `:-` break candidate used when head wrapping is enabled.
    pending_head: Option<SoftBreakCandidate>,
    /// Top-level body separator candidates recorded after a pending head break.
    top_level_body_breaks: Vec<SoftBreakCandidate>,
    /// Whether a taken head wrap should force later top-level body separators.
    head_wrapped_body_multiline: bool,
    /// Whether the rule head already took a nested soft wrap before `:-`.
    head_content_soft_wrapped: bool,
    /// Whether a nested semicolon should force the next top-level separator.
    nested_semicolon_wrap_pending: bool,
    /// Whether a boolean conditional colon should prefer an earlier body break.
    boolean_conditional_colon_prefers_body_break: bool,
}

impl SoftWrapEngine {
    /// Clears wrap candidates after an unconditional hard flush.
    fn reset_after_hard_flush(&mut self) {
        self.active = None;
        self.preferred = None;
        self.pending_head = None;
        self.nested_semicolon_wrap_pending = false;
        self.boolean_conditional_colon_prefers_body_break = false;
    }

    /// Clears transient wrap candidates after a soft wrap is resolved.
    fn reset_after_soft_flush(&mut self) {
        self.active = None;
        self.preferred = None;
        self.pending_head = None;
        self.boolean_conditional_colon_prefers_body_break = false;
    }

    /// Records a soft wrap that happened while still formatting a rule head.
    fn record_head_content_soft_wrap(&mut self, state: &StatementState) {
        if state.has_head_like && !state.has_if {
            self.head_content_soft_wrapped = true;
        }
    }

    /// Returns the top-level body break preferred by boolean conditional colons.
    fn preferred_boolean_conditional_body_break(&self) -> Option<SoftBreakCandidate> {
        self.boolean_conditional_colon_prefers_body_break
            .then(|| self.top_level_body_breaks.first().copied())
            .flatten()
    }
}

/// Formatting decision for a colon token.
enum ColonDecision {
    /// The colon only needs a following space.
    SpaceOnly,
    /// The colon should trigger an immediate hard break.
    HardBreak,
    /// The colon should register a soft continuation candidate.
    SoftContinuation {
        /// Indentation to use if the continuation wraps.
        indent: ContinuationIndent,
        /// Whether overflow after this colon should prefer an earlier body comma.
        prefer_body_break_before_head: bool,
    },
}

/// Decides how colon tokens interact with hard breaks and soft continuations.
struct ColonPolicy {
    /// Formatter options used by colon-specific decisions.
    policy: FormattingPolicy,
}

impl ColonPolicy {
    /// Creates a colon policy from the active formatter policy.
    fn new(policy: FormattingPolicy) -> Self {
        Self { policy }
    }

    /// Returns the formatting decision for a colon in the current traversal state.
    fn decide(
        self,
        node: &tree_sitter::Node<'_>,
        statement_kind: &str,
        state: &StatementState,
        buf: &OutputBuffer,
        indent_level: usize,
        preferred_soft_break: Option<SoftBreakCandidate>,
    ) -> Result<ColonDecision> {
        if state.in_theory_atom_definition {
            Ok(ColonDecision::SpaceOnly)
        } else if (self.policy.should_break_after_colon(state)
            && !is_boolean_conditional_literal_colon(node))
            || (self.policy.break_after_colon && is_show_condition_colon(node, statement_kind))
        {
            Ok(ColonDecision::HardBreak)
        } else if state.is_show {
            Ok(ColonDecision::SpaceOnly)
        } else {
            Ok(ColonDecision::SoftContinuation {
                indent: continuation_indent_after_colon(
                    buf.as_bytes(),
                    self.policy,
                    indent_level,
                    preferred_soft_break,
                )?,
                prefer_body_break_before_head: self.policy.break_after_colon
                    && is_boolean_conditional_literal_colon(node),
            })
        }
    }
}

/// Statement categories used to decide blank-line grouping behavior.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum StatementType {
    /// A fact-like statement that can share a line block with other facts.
    Fact,
    /// A directive that groups with consecutive directives of the same kind.
    Directive(DirectiveType),
    /// Any other statement that should be separated more aggressively.
    Other,
}

/// Directive categories used for top-level block grouping.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum DirectiveType {
    /// A `#show`-style directive.
    Show,
    /// An `#include` directive.
    Include,
    /// An `#external` directive.
    External,
    /// A `#const` directive.
    Const,
    /// A `#heuristic` directive.
    Heuristic,
    /// A `#defined` directive.
    Defined,
    /// A `#project` directive.
    Project,
}

/// Classifies top-level syntax nodes into formatter grouping categories.
struct StatementClassifier;

impl StatementClassifier {
    /// Returns the block grouping category for a formatted statement.
    fn classify(statement_kind: &str, state: &StatementState) -> StatementType {
        if let Some(directive_type) = Self::directive_type(statement_kind) {
            StatementType::Directive(directive_type)
        } else if statement_kind == "edge" || (state.has_head_like && !state.has_body) {
            StatementType::Fact
        } else {
            StatementType::Other
        }
    }

    /// Returns the directive grouping category for top-level directive statements.
    fn directive_type(statement_kind: &str) -> Option<DirectiveType> {
        match statement_kind {
            "show" | "show_signature" | "show_term" => Some(DirectiveType::Show),
            "include" => Some(DirectiveType::Include),
            "external" => Some(DirectiveType::External),
            "const" => Some(DirectiveType::Const),
            "heuristic" => Some(DirectiveType::Heuristic),
            "defined" => Some(DirectiveType::Defined),
            "project_atom" => Some(DirectiveType::Project),
            _ => None,
        }
    }
}

/// High-level output grouping state while formatting a full program.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum FormatterState {
    /// Currently formatting a block of statements of a specific type.
    Block(StatementType),
    /// Currently positioned after a standalone comment block.
    SomeBlock,
    /// No output has been written yet.
    No,
}

use FormatterState::*;

/// Tracks program-level output grouping while writing formatted statements.
struct ProgramFormatter<'a> {
    /// Destination writer receiving formatted program output.
    out: &'a mut dyn Write,
    /// Current grouping state of the emitted output.
    state: FormatterState,
}

impl ProgramFormatter<'_> {
    /// Starts a new statement block, inserting the required separating lines.
    fn new_block(&mut self, stmt_type: Option<StatementType>) -> Result<()> {
        match self.state {
            No => {}
            Block(StatementType::Other) | SomeBlock => {
                writeln!(self.out)?;
            }
            _ => {
                writeln!(self.out)?;
                writeln!(self.out)?;
            }
        }
        match stmt_type {
            Some(stmt_type) => self.state = Block(stmt_type),
            None => self.state = SomeBlock,
        }
        Ok(())
    }

    /// Emits a top-level comment with the correct surrounding spacing.
    fn process_comment(&mut self, buf: &[u8]) -> Result<()> {
        match self.state {
            No => {}
            SomeBlock | Block(StatementType::Other) => {
                writeln!(self.out)?;
            }
            _ => self.new_block(None)?,
        };
        let text = std::str::from_utf8(buf).unwrap();
        write!(self.out, "{}", text.trim_end())?;
        self.state = SomeBlock;
        Ok(())
    }

    /// Emits a formatted statement and updates program-level block grouping.
    fn process_statement(&mut self, stmt_type: StatementType, buf: &[u8]) -> Result<()> {
        match (self.state, stmt_type) {
            (Block(StatementType::Fact), StatementType::Fact) => write!(self.out, " ")?,
            (Block(StatementType::Directive(current)), StatementType::Directive(next))
                if current == next =>
            {
                writeln!(self.out)?
            }
            (Block(StatementType::Other), StatementType::Other) => writeln!(self.out)?,
            _ => self.new_block(Some(stmt_type))?,
        }

        let buf_str = std::str::from_utf8(buf)?;
        write!(self.out, "{}", buf_str)?;

        if stmt_type == StatementType::Other {
            writeln!(self.out)?;
        }
        Ok(())
    }

    /// Finalizes the program output with any trailing newline required by the
    /// current grouping state.
    fn finish_program(&mut self) -> Result<()> {
        if self.state != No && self.state != Block(StatementType::Other) {
            writeln!(self.out)?;
        }
        Ok(())
    }
}

/// Parses and formats raw clingo source text into `out`.
pub fn format_source(
    source_code: &[u8],
    out: &mut dyn Write,
    debug: bool,
    options: &FormatOptions,
) -> Result<()> {
    for segment in split_format_segments(source_code) {
        match segment {
            FormatSegment::Formattable(bytes) => format_source_segment(bytes, out, debug, options)?,
            FormatSegment::Raw(bytes) => out.write_all(bytes)?,
        }
    }
    Ok(())
}

/// Parses and formats one formattable source segment into `out`.
fn format_source_segment(
    source_code: &[u8],
    out: &mut dyn Write,
    debug: bool,
    options: &FormatOptions,
) -> Result<()> {
    if source_code.iter().all(u8::is_ascii_whitespace) {
        return Ok(());
    }

    let tree = parse_source(source_code)?;
    format_program(&tree, source_code, out, debug, options)
}

/// Formats an already parsed clingo syntax tree into `out`.
pub fn format_program(
    tree: &tree_sitter::Tree,
    source_code: &[u8],
    out: &mut dyn Write,
    debug_enabled: bool,
    options: &FormatOptions,
) -> Result<()> {
    let policy = FormattingPolicy::from(options);
    let mut formatter = ProgramFormatter {
        out,
        state: FormatterState::No,
    };
    let mut short_cut = false;
    let mut cursor = tree.walk();
    let has_errors = cursor.node().has_error();

    let mut indent_level = 0;
    let mut did_visit_children = false;

    loop {
        let node = cursor.node();
        if !did_visit_children {
            if log_node_error(&node, source_code) {
                did_visit_children = true;
            } else {
                match node.kind() {
                    "line_comment" | "block_comment" => {
                        let start_byte = node.start_byte();
                        let end_byte = node.end_byte();
                        formatter.process_comment(&source_code[start_byte..end_byte])?;
                    }
                    _ if is_top_level_statement(&node) => {
                        let mut buf = Vec::new();
                        let stmt_type =
                            format_statement(&node, source_code, &mut buf, debug_enabled, policy)?;

                        formatter.process_statement(stmt_type, &buf)?;
                        short_cut = true;
                    }
                    _ => {}
                }
                debug_node(&cursor, indent_level, debug_enabled);
                if short_cut {
                    did_visit_children = true;
                } else if cursor.goto_first_child() {
                    did_visit_children = false;
                    indent_level += 1;
                } else {
                    did_visit_children = true;
                }
            }
        } else {
            match node.kind() {
                "source_file" => {
                    formatter.finish_program()?;
                }
                _ if is_top_level_statement(&node) => short_cut = false,
                _ => {}
            }
            if cursor.goto_next_sibling() {
                did_visit_children = false;
            } else if cursor.goto_parent() {
                did_visit_children = true;
                indent_level -= 1;
            } else {
                break;
            }
        }
    }
    if has_errors {
        Err(anyhow::Error::msg("Error while parsing"))
    } else {
        Ok(())
    }
}

/// Formats a single top-level statement into an internal output buffer and
/// reports the statement category used for block grouping.
fn format_statement(
    node: &tree_sitter::Node<'_>,
    source_code: &[u8],
    out: &mut dyn Write,
    debug_enabled: bool,
    policy: FormattingPolicy,
) -> Result<StatementType> {
    let statement_kind = node.kind();
    let mut buf = OutputBuffer::new();
    let mut wrap = SoftWrapEngine::default();
    let mut colon_continuation_indent: Option<ContinuationIndent> = None;
    let mut flush = false;
    let mut cosmetic_ws = false;
    let mut state = StatementState::new(statement_kind);
    let mut cursor = node.walk();

    let mut indent_level = 0;
    let mut mindent_level: usize = 0;
    let mut did_visit_children = false;

    loop {
        let node = cursor.node();
        if !did_visit_children {
            if log_node_error(&node, source_code) {
                did_visit_children = true;
            } else {
                state.enter_node(cursor.field_name(), node.kind());
                match node.kind() {
                    "[" => {
                        cosmetic_ws = true;
                        state.in_termvec += 1;
                        mindent_level += 1;
                    }
                    ":-" => cosmetic_ws = true,
                    "|" | "relation" | ":" => cosmetic_ws = true,
                    "{" if matches!(statement_kind, "maximize" | "minimize" | "theory")
                        && node
                            .parent()
                            .is_some_and(|parent| parent.kind() == statement_kind) =>
                    {
                        cosmetic_ws = true;
                    }
                    "}" => {
                        if state.in_theory_atom_definition
                            || node
                                .parent()
                                .is_some_and(|parent| parent.kind() == "theory_set")
                        {
                            cosmetic_ws = true;
                        } else {
                            mindent_level = mindent_level.saturating_sub(1);
                            flush = true;
                        }
                    }
                    ")" => mindent_level = mindent_level.saturating_sub(1),
                    _ => {}
                }
                debug_node(&cursor, indent_level, debug_enabled);
                if cursor.goto_first_child() {
                    did_visit_children = false;
                    indent_level += 1;
                } else {
                    did_visit_children = true;
                }
            }
        } else {
            if flush {
                flush_current_buffer(
                    out,
                    &mut buf,
                    colon_continuation_indent.unwrap_or(ContinuationIndent::Level(mindent_level)),
                    policy.indent_width,
                )?;
                wrap.reset_after_hard_flush();
                flush = false;
            } else if let Some(candidate) = wrap.active {
                if buf.visible_len()? >= policy.soft_flush_limit {
                    if let Some(head_candidate) = wrap.pending_head {
                        if let Some(preferred_body_candidate) =
                            wrap.preferred_boolean_conditional_body_break()
                        {
                            flush_at_candidate(
                                out,
                                &mut buf,
                                preferred_body_candidate,
                                policy.indent_width,
                            )?;
                        } else {
                            flush_at_head_candidate(
                                out,
                                &mut buf,
                                head_candidate,
                                &wrap.top_level_body_breaks,
                                policy.indent_width,
                            )?;
                            wrap.head_wrapped_body_multiline = true;
                        }
                        wrap.reset_after_soft_flush();
                    } else if candidate.priority == SoftBreakPriority::Nested {
                        flush_at_candidate(
                            out,
                            &mut buf,
                            wrap.preferred.unwrap_or(candidate),
                            policy.indent_width,
                        )?;
                        wrap.record_head_content_soft_wrap(&state);
                        wrap.reset_after_soft_flush();
                    } else {
                        flush_at_candidate(out, &mut buf, candidate, policy.indent_width)?;
                        wrap.record_head_content_soft_wrap(&state);
                        wrap.reset_after_soft_flush();
                    }
                } else {
                    wrap.active = None;
                }
            } else if let Some(candidate) = wrap.pending_head {
                if buf.visible_len()? >= policy.soft_flush_limit {
                    if let Some(preferred_body_candidate) =
                        wrap.preferred_boolean_conditional_body_break()
                    {
                        flush_at_candidate(
                            out,
                            &mut buf,
                            preferred_body_candidate,
                            policy.indent_width,
                        )?;
                    } else {
                        flush_at_head_candidate(
                            out,
                            &mut buf,
                            candidate,
                            &wrap.top_level_body_breaks,
                            policy.indent_width,
                        )?;
                        wrap.head_wrapped_body_multiline = true;
                    }
                    wrap.reset_after_soft_flush();
                }
            }
            if cosmetic_ws {
                write!(buf, " ")?;
                cosmetic_ws = false
            }
            if node.child_count() == 0 {
                let start_byte = node.start_byte();
                let end_byte = node.end_byte();
                let text = std::str::from_utf8(&source_code[start_byte..end_byte]).unwrap();
                if node.kind() == "line_comment" {
                    write!(buf, "{}", text.trim_end())?;
                } else {
                    write!(buf, "{}", text)?;
                }
            }

            state.leave_node(node.kind());
            match node.kind() {
                "line_comment" => flush = true,
                "]" => {
                    mindent_level = mindent_level.saturating_sub(1);
                    state.in_termvec = state.in_termvec.saturating_sub(1);
                }
                "body" => {
                    if state.has_if {
                        mindent_level = mindent_level.saturating_sub(1);
                        state.has_if = false;
                    }
                }
                "body_aggregate_element"
                | "head_aggregate_element"
                | "conditional_literal"
                | "optimize_element" => {
                    colon_continuation_indent = None;
                    mindent_level = mindent_level.saturating_sub(1);
                }
                "(" => mindent_level += 1,
                "default_negation"
                | "double_default_negation"
                | "aggregate_function"
                | "#external"
                | "#defined"
                | "#const"
                | "#program"
                | "#project"
                | "#heuristic"
                | "#theory" => write!(buf, " ")?,
                "#include" => {
                    write!(buf, " ")?;
                    state.is_include = true;
                }
                "#show" => {
                    write!(buf, " ")?;
                    state.is_show = true;
                }
                "relation" | "|" => write!(buf, " ")?,
                ";" => {
                    if state.in_termvec > 0 {
                        wrap.active = Some(mark_soft_break(
                            &buf,
                            SoftBreakPriority::Nested,
                            ContinuationIndent::Level(mindent_level),
                        )?);
                        wrap.nested_semicolon_wrap_pending = true;
                        write!(buf, " ")?;
                    } else {
                        flush = true;
                    }
                }
                ":" => {
                    match ColonPolicy::new(policy).decide(
                        &node,
                        statement_kind,
                        &state,
                        &buf,
                        mindent_level + 1,
                        wrap.preferred,
                    )? {
                        ColonDecision::SpaceOnly => write!(buf, " ")?,
                        ColonDecision::HardBreak => {
                            colon_continuation_indent = None;
                            mindent_level += 1;
                            flush = true;
                        }
                        ColonDecision::SoftContinuation {
                            indent,
                            prefer_body_break_before_head,
                        } => {
                            colon_continuation_indent = Some(indent);
                            wrap.active =
                                Some(mark_soft_break(&buf, SoftBreakPriority::Nested, indent)?);
                            wrap.boolean_conditional_colon_prefers_body_break =
                                prefer_body_break_before_head;
                            write!(buf, " ")?;
                            mindent_level += 1;
                        }
                    }
                }
                "{" => {
                    if state.in_theory_atom_definition
                        || node
                            .parent()
                            .is_some_and(|parent| parent.kind() == "theory_set")
                    {
                        write!(buf, " ")?;
                    } else {
                        mindent_level += 1;
                        flush = true;
                    }
                }
                "," => {
                    let is_top_level_separator = state.in_termvec == 0 && !state.is_show;

                    if wrap.head_wrapped_body_multiline && is_top_level_separator {
                        flush = true;
                    } else if policy.should_break_after_body_atom(&state) && is_top_level_separator
                    {
                        if let Some(head_candidate) = wrap.pending_head {
                            let body_candidate = mark_soft_break(
                                &buf,
                                SoftBreakPriority::TopLevel,
                                colon_continuation_indent
                                    .unwrap_or(ContinuationIndent::Level(mindent_level)),
                            )?;
                            wrap.top_level_body_breaks.push(body_candidate);
                            flush_at_head_candidate(
                                out,
                                &mut buf,
                                head_candidate,
                                &wrap.top_level_body_breaks,
                                policy.indent_width,
                            )?;
                            wrap.head_wrapped_body_multiline = true;
                            wrap.reset_after_soft_flush();
                        } else {
                            flush = true;
                        }
                    } else if policy.should_break_after_body_atom(&state)
                        || (is_top_level_separator && wrap.nested_semicolon_wrap_pending)
                    {
                        flush = true;
                    } else {
                        let candidate = mark_soft_break(
                            &buf,
                            if is_top_level_separator {
                                SoftBreakPriority::TopLevel
                            } else {
                                SoftBreakPriority::Nested
                            },
                            if is_top_level_separator {
                                colon_continuation_indent
                                    .unwrap_or(ContinuationIndent::Level(mindent_level))
                            } else {
                                ContinuationIndent::Level(mindent_level)
                            },
                        )?;
                        if candidate.priority == SoftBreakPriority::TopLevel {
                            wrap.preferred = Some(candidate);
                            if state.has_if {
                                wrap.top_level_body_breaks.push(candidate);
                            }
                        }
                        wrap.active = Some(candidate);
                        write!(buf, " ")?;
                    }
                    if is_top_level_separator {
                        wrap.nested_semicolon_wrap_pending = false;
                    }
                }
                ":-" => {
                    state.has_if = true;
                    mindent_level += 1;
                    if !state.has_head_like {
                        write!(buf, " ")?;
                    } else {
                        let candidate = mark_soft_break(
                            &buf,
                            SoftBreakPriority::TopLevel,
                            ContinuationIndent::Level(mindent_level),
                        )?;
                        if policy.should_break_after_head(&state) {
                            if wrap.head_content_soft_wrapped {
                                flush = true;
                                wrap.head_wrapped_body_multiline = true;
                                wrap.active = None;
                                wrap.pending_head = None;
                            } else {
                                wrap.pending_head = Some(candidate);
                                wrap.active = Some(candidate);
                                write!(buf, " ")?;
                            }
                        } else {
                            wrap.active = Some(candidate);
                            write!(buf, " ")?;
                        }
                    }
                }
                _ => {}
            }
            if cursor.goto_next_sibling() {
                did_visit_children = false;
            } else if cursor.goto_parent() {
                did_visit_children = true;
                indent_level -= 1;
            } else {
                break;
            }
        }
    }
    if let Some(candidate) = wrap.active {
        if buf.visible_len()? >= policy.soft_flush_limit {
            if let Some(head_candidate) = wrap.pending_head {
                if let Some(preferred_body_candidate) =
                    wrap.preferred_boolean_conditional_body_break()
                {
                    flush_at_candidate(
                        out,
                        &mut buf,
                        preferred_body_candidate,
                        policy.indent_width,
                    )?;
                } else {
                    flush_at_head_candidate(
                        out,
                        &mut buf,
                        head_candidate,
                        &wrap.top_level_body_breaks,
                        policy.indent_width,
                    )?;
                }
            } else if candidate.priority == SoftBreakPriority::Nested {
                flush_at_candidate(
                    out,
                    &mut buf,
                    wrap.preferred.unwrap_or(candidate),
                    policy.indent_width,
                )?;
                wrap.record_head_content_soft_wrap(&state);
            } else {
                flush_at_candidate(out, &mut buf, candidate, policy.indent_width)?;
                wrap.record_head_content_soft_wrap(&state);
            }
        }
    } else if let Some(candidate) = wrap.pending_head {
        if buf.visible_len()? >= policy.soft_flush_limit {
            if let Some(preferred_body_candidate) = wrap.preferred_boolean_conditional_body_break()
            {
                flush_at_candidate(out, &mut buf, preferred_body_candidate, policy.indent_width)?;
            } else {
                flush_at_head_candidate(
                    out,
                    &mut buf,
                    candidate,
                    &wrap.top_level_body_breaks,
                    policy.indent_width,
                )?;
            }
        }
    }

    let buf_str = std::str::from_utf8(buf.as_bytes())?;
    write!(out, "{}", buf_str)?;
    Ok(StatementClassifier::classify(statement_kind, &state))
}

/// Returns whether `node` is the colon introducing a conditional `#show` body.
fn is_show_condition_colon(node: &tree_sitter::Node<'_>, statement_kind: &str) -> bool {
    matches!(statement_kind, "show" | "show_signature" | "show_term")
        && node
            .parent()
            .is_some_and(|parent| parent.kind() == statement_kind)
}

/// Returns whether `node` is the colon inside a boolean conditional literal.
fn is_boolean_conditional_literal_colon(node: &tree_sitter::Node<'_>) -> bool {
    node.parent()
        .filter(|parent| parent.kind() == "conditional_literal")
        .and_then(|parent| parent.child_by_field_name("literal"))
        .and_then(|literal| literal.child(0))
        .is_some_and(|child| child.kind() == "boolean_constant")
}

/// Creates a soft-break candidate at the current end of the buffered output.
fn mark_soft_break(
    buf: &[u8],
    priority: SoftBreakPriority,
    indent: ContinuationIndent,
) -> Result<SoftBreakCandidate> {
    Ok(SoftBreakCandidate {
        pos: buffer_len(buf)?,
        indent,
        priority,
    })
}

/// Flushes the full current buffer and starts a new indented output line.
fn flush_current_buffer(
    out: &mut dyn Write,
    buf: &mut Vec<u8>,
    indent: ContinuationIndent,
    indent_width: usize,
) -> Result<()> {
    let buf_str = std::str::from_utf8(buf)?;
    write!(out, "{}", buf_str)?;
    buf.clear();
    writeln!(out)?;
    write_continuation_indent(buf, indent, indent_width)?;
    Ok(())
}

/// Flushes the buffer at a selected soft-break candidate and keeps the
/// remaining text on the next indented line.
fn flush_at_candidate(
    out: &mut dyn Write,
    buf: &mut Vec<u8>,
    candidate: SoftBreakCandidate,
    indent_width: usize,
) -> Result<()> {
    let mut remainder = buf.split_off(candidate.pos);
    if remainder.first() == Some(&b' ') {
        remainder.remove(0);
    }

    let prefix = std::str::from_utf8(buf)?;
    write!(out, "{}", prefix)?;
    writeln!(out)?;

    buf.clear();
    write_continuation_indent(buf, candidate.indent, indent_width)?;
    buf.extend_from_slice(&remainder);
    Ok(())
}

/// Flushes at a selected head soft-break and rewrites already buffered
/// top-level body commas into multiline body breaks.
fn flush_at_head_candidate(
    out: &mut dyn Write,
    buf: &mut Vec<u8>,
    candidate: SoftBreakCandidate,
    body_breaks: &[SoftBreakCandidate],
    indent_width: usize,
) -> Result<()> {
    let mut remainder = buf.split_off(candidate.pos);
    let trimmed_leading_space = usize::from(remainder.first() == Some(&b' '));
    if trimmed_leading_space == 1 {
        remainder.remove(0);
    }

    let prefix = std::str::from_utf8(buf)?;
    write!(out, "{}", prefix)?;
    writeln!(out)?;

    let mut split_points: Vec<(usize, ContinuationIndent)> = body_breaks
        .iter()
        .filter_map(|body_break| {
            (body_break.pos > candidate.pos)
                .then_some(body_break.pos - candidate.pos)
                .and_then(|relative| relative.checked_sub(trimmed_leading_space))
                .filter(|relative| *relative <= remainder.len())
                .map(|relative| (relative, body_break.indent))
        })
        .collect();
    split_points.sort_unstable_by_key(|(relative, _)| *relative);
    split_points.dedup_by_key(|(relative, _)| *relative);

    buf.clear();
    let mut start = 0;
    let mut current_indent = candidate.indent;
    for (split_point, next_indent) in split_points {
        let segment = &remainder[start..split_point];
        write_continuation_indent(buf, current_indent, indent_width)?;
        buf.extend_from_slice(segment);
        let line = std::str::from_utf8(buf)?;
        write!(out, "{}", line)?;
        writeln!(out)?;
        buf.clear();

        start = split_point;
        if remainder.get(start) == Some(&b' ') {
            start += 1;
        }
        current_indent = next_indent;
    }

    write_continuation_indent(buf, current_indent, indent_width)?;
    buf.extend_from_slice(&remainder[start..]);
    Ok(())
}

/// Chooses the indentation to use for a continuation created after a colon.
fn continuation_indent_after_colon(
    buf: &[u8],
    policy: FormattingPolicy,
    indent_level: usize,
    preferred_soft_break: Option<SoftBreakCandidate>,
) -> Result<ContinuationIndent> {
    if policy.align_continuation_after_colon {
        if let Some(candidate) = preferred_soft_break
            .filter(|candidate| candidate.priority == SoftBreakPriority::TopLevel)
        {
            let mut remainder = &buf[candidate.pos..];
            if remainder.first() == Some(&b' ') {
                remainder = &remainder[1..];
            }
            Ok(ContinuationIndent::Align(
                rendered_indent_width(candidate.indent, policy.indent_width)
                    + buffer_len(remainder)?
                    + 1,
            ))
        } else {
            Ok(ContinuationIndent::Align(buffer_len(buf)? + 1))
        }
    } else {
        Ok(ContinuationIndent::Level(indent_level))
    }
}

/// Converts a continuation indentation mode into its visible column width.
fn rendered_indent_width(indent: ContinuationIndent, indent_width: usize) -> usize {
    match indent {
        ContinuationIndent::Level(level) => indent_width * level,
        ContinuationIndent::Align(width) => width,
    }
}

/// Writes the indentation represented by `indent` into `buf`.
fn write_continuation_indent(
    buf: &mut Vec<u8>,
    indent: ContinuationIndent,
    indent_width: usize,
) -> Result<()> {
    match indent {
        ContinuationIndent::Level(level) => {
            let indent = " ".repeat(indent_width * level);
            write!(buf, "{indent}")?;
        }
        ContinuationIndent::Align(width) => {
            let indent = " ".repeat(width);
            write!(buf, "{indent}")?;
        }
    }
    Ok(())
}

/// Returns the visible character length of the current buffered output.
fn buffer_len(buf: &[u8]) -> Result<usize> {
    Ok(std::str::from_utf8(buf)?.len())
}

/// Returns whether `node` is a named top-level statement in the source file.
fn is_top_level_statement(node: &tree_sitter::Node<'_>) -> bool {
    node.parent()
        .is_some_and(|parent| parent.kind() == "source_file")
        && node.is_named()
        && !matches!(node.kind(), "line_comment" | "block_comment")
}

/// Logs parse errors or missing nodes encountered during traversal.
fn log_node_error(node: &tree_sitter::Node<'_>, source_code: &[u8]) -> bool {
    if node.is_missing() {
        let start = node.start_position();
        if node.is_named() {
            warn!(
                "MISSING {} at [{}, {}]",
                node.kind(),
                start.row,
                start.column
            );
        } else {
            warn!(
                "MISSING \"{}\" at [{}, {}]",
                node.kind().replace('\n', "\\n"),
                start.row,
                start.column
            );
        }
        true
    } else if node.is_error() {
        let start = node.start_position();
        let end = node.end_position();
        let text = std::str::from_utf8(&source_code[node.start_byte()..node.end_byte()]).unwrap();

        warn!(
            "SYNTAX ERROR at [{}, {}] - [{}, {}]",
            start.row, start.column, end.row, end.column
        );
        warn!("Unexpected: {text}");
        true
    } else {
        false
    }
}

/// Logs a tree-sitter node in a readable indented form when debug output is
/// enabled.
fn debug_node(cursor: &tree_sitter::TreeCursor<'_>, indent_level: usize, debug_enabled: bool) {
    if !debug_enabled {
        return;
    }

    let node = cursor.node();
    let indent = "  ".repeat(indent_level);
    let start = node.start_position();
    let end = node.end_position();
    if let Some(field_name) = cursor.field_name() {
        debug!("{}: ", field_name);
    }

    debug!(
        "{}({} [{}, {}] - [{}, {}]",
        indent,
        node.kind(),
        start.row,
        start.column,
        end.row,
        end.column
    );
}
