use anyhow::{Context, Result};
use clap::Parser;
use log::{debug, error};
use std::fs;
use std::{
    io::{self, Write},
    path::PathBuf,
};

const MAX_LENGTH: usize = 40;

/// Format clingo code
#[derive(Parser, Debug)]
#[clap(version, author)]
struct Opt {
    /// Input file in clingo format
    #[clap(name = "FILE", parse(from_os_str))]
    file: PathBuf,

    /// Enable debug output
    #[clap(long)]
    debug: bool,
}

pub enum Reader<'a> {
    File(io::BufReader<fs::File>),
    Stdin(io::StdinLock<'a>),
}
impl<'a> io::Read for Reader<'a> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self {
            Self::File(reader) => reader.read(buf),
            Self::Stdin(guard) => guard.read(buf),
        }
    }
}
impl<'a> io::BufRead for Reader<'a> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        match self {
            Self::File(reader) => reader.fill_buf(),
            Self::Stdin(guard) => guard.fill_buf(),
        }
    }
    fn consume(&mut self, amt: usize) {
        match self {
            Self::File(reader) => reader.consume(amt),
            Self::Stdin(guard) => guard.consume(amt),
        }
    }
}

fn main() {
    stderrlog::new()
        .module(module_path!())
        .verbosity(3)
        .init()
        .unwrap();
    if let Err(err) = run() {
        error!("{:?}", err);
        std::process::exit(1);
    }
}

fn flush(buf: &mut String, ident_level: usize) -> Result<()> {
    let mut stdout = std::io::stdout();
    writeln!(&mut stdout, "{buf}")?;
    buf.clear();
    for _i in 0..ident_level {
        buf.push_str("    ");
    }
    Ok(())
}
fn plush(buf: &mut String) -> Result<()> {
    let mut stdout = std::io::stdout();
    write!(&mut stdout, "{buf}")?;
    buf.clear();
    Ok(())
}
fn hush(buf: &mut String, ident_level: usize) -> Result<()> {
    let mut stdout = std::io::stdout();
    for _i in 0..ident_level {
        write!(&mut stdout, "    ")?;
    }
    writeln!(&mut stdout, "{buf}")?;
    buf.clear();
    Ok(())
}
fn lush(buf: &mut String, ident_level: usize) -> Result<()> {
    let mut stdout = std::io::stdout();
    for _i in 0..ident_level {
        write!(&mut stdout, "    ")?;
    }
    write!(&mut stdout, "{buf}")?;
    buf.clear();
    Ok(())
}

fn run() -> Result<()> {
    let opt = Opt::parse();

    let path = opt.file.to_str().unwrap();
    let source_code =
        fs::read(path).with_context(|| format!("Error reading source file {}", path))?;

    let mut stdout = std::io::stdout();

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_clingo::language())
        .expect("Error loading clingo grammar");

    let tree = parser.parse(&source_code, None).unwrap();
    let mut cursor = tree.walk();

    let mut first = true;
    let mut buf = String::new();
    let mut indent_level = 0;
    let mut mindent_level = 0;
    let mut did_visit_children = false;

    let mut in_statement = false;
    let mut in_conjunction = false;
    let mut in_noptcondition = false;
    let mut in_ntermvec = 0;
    let mut has_head = false;
    let mut has_body = false;

    loop {
        let node = cursor.node();
        let is_named = node.is_named();
        if did_visit_children {
            // what happens after the element
            if is_named {
                match node.kind() {
                    "comment" => {
                        if in_statement {
                            hush(&mut buf, mindent_level)?;
                        } else {
                            flush(&mut buf, mindent_level)?;
                        }
                        first = true;
                    }
                    "statement" => {
                        if !has_head || has_body {
                            writeln!(&mut stdout, "{buf}")?;
                        } else {
                            // no newline after facts.
                            write!(&mut stdout, "{buf}")?;
                        }
                        buf.clear();

                        //reset properties
                        in_statement = false;
                        has_head = false;
                        has_body = false;
                    }
                    "head" => {
                        has_head = true;
                    }
                    "ntermvec" => {
                        in_ntermvec -= 1;
                    }
                    "NOT" | "aggregatefunction" => buf.push(' '),
                    "IF" => {
                        mindent_level += 1; // decrease after bodydot
                        if !has_head {
                            buf.push(' ');
                        } else {
                            flush(&mut buf, mindent_level)?;
                        }
                    }
                    "bodydot" => {
                        mindent_level -= 1;
                    }
                    "lubodyaggregate" => {}
                    "bodyaggrelem" => {
                        // mindent_level -= 1;
                    }
                    "noptcondition" => {
                        in_noptcondition = false;
                        mindent_level -= 1;
                    }
                    "conjunction" => {
                        in_conjunction = false;
                        mindent_level -= 1;
                    }
                    "COMMA" => {
                        if in_ntermvec == 0 {
                            flush(&mut buf, mindent_level)?;
                        } else if buf.len() >= MAX_LENGTH {
                            flush(&mut buf, mindent_level)?;
                        } else {
                            buf.push(' ');
                        }
                    }
                    "SEM" => {
                        flush(&mut buf, mindent_level)?;
                    }
                    "COLON" => {
                        if in_conjunction {
                            mindent_level += 1;
                        }
                        if in_noptcondition {
                            mindent_level += 1;
                        }
                        flush(&mut buf, mindent_level)?;
                    }
                    "LBRACE" => {
                        mindent_level += 1;
                        flush(&mut buf, mindent_level)?;
                    }
                    "LPAREN" => {
                        mindent_level += 1;
                    }
                    "SHOW" | "cmp" => buf.push(' '),
                    _ => {}
                }
            }
            if cursor.goto_next_sibling() {
                did_visit_children = false;
            } else if cursor.goto_parent() {
                did_visit_children = true;
                indent_level -= 1;
            } else {
                break;
            }
        } else {
            // what happens before the element
            if is_named {
                match node.kind() {
                    "comment" => {
                        if in_statement {
                            plush(&mut buf)?;
                        } else {
                            if !first {
                                writeln!(&mut stdout)?;
                            } else {
                                first = false
                            }
                            lush(&mut buf, mindent_level)?;
                        }
                    }
                    "statement" => {
                        if !first {
                            writeln!(&mut stdout)?;
                        } else {
                            first = false
                        }
                        in_statement = true;
                    }
                    "bodydot" => {
                        has_body = true;
                    }
                    "noptcondition" => {
                        in_noptcondition = true;
                        //incease mindent_level after COLON
                    }
                    "conjunction" => {
                        in_conjunction = true;
                        //incease mindent_level after COLON
                    }
                    "ntermvec" => {
                        in_ntermvec += 1;
                    }
                    "lubodyaggregate" => {}
                    "IF" => {
                        buf.push(' ');
                    }
                    "COLON" | "cmp" => buf.push(' '),
                    "RBRACE" => {
                        mindent_level -= 1;
                        flush(&mut buf, mindent_level)?;
                    }
                    "RPAREN" => {
                        mindent_level -= 1;
                    }
                    _ => {}
                }

                if node.child_count() == 0 {
                    let start_byte = node.start_byte();
                    let end_byte = node.end_byte();
                    let text = std::str::from_utf8(&source_code[start_byte..end_byte]).unwrap();

                    buf.push_str(text);
                    if opt.debug {
                        debug!("{} ", text);
                    }
                }

                if opt.debug {
                    let indent = "  ".repeat(indent_level);
                    let start = node.start_position();
                    let end = node.end_position();
                    if let Some(field_name) = cursor.field_name() {
                        eprint!("{}: ", field_name);
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
            } else if node.child_count() == 0 {
                let start_byte = node.start_byte();
                let end_byte = node.end_byte();
                let text = std::str::from_utf8(&source_code[start_byte..end_byte]).unwrap();

                buf.push_str(text);

                if opt.debug {
                    debug!("{}", text);
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
            }

            if cursor.goto_first_child() {
                did_visit_children = false;
                indent_level += 1;
            } else {
                did_visit_children = true;
            }
        }
    }
    cursor.reset(tree.root_node());

    let mut first_error = None;
    loop {
        let node = cursor.node();
        if node.has_error() {
            if node.is_error() || node.is_missing() {
                first_error = Some(node);
                break;
            } else if !cursor.goto_first_child() {
                break;
            }
        } else if !cursor.goto_next_sibling() {
            break;
        }
    }

    if first_error.is_some() {
        error!("{}", path);
        if let Some(node) = first_error {
            let start = node.start_position();
            let end = node.end_position();
            error!("\t(");
            if node.is_missing() {
                if node.is_named() {
                    error!("MISSING {}", node.kind());
                } else {
                    error!("MISSING \"{}\"", node.kind().replace("\n", "\\n"));
                }
            } else {
                error!("{}", node.kind());
            }
            error!(
                "[{}, {}] - [{}, {}])",
                start.row, start.column, end.row, end.column
            );
        }
    }

    Ok(())
}
