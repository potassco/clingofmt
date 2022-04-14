use anyhow::{Context, Result};
use clap::Parser;
use log::{debug, error, warn};
use std::fs;
use std::{
    io::{self, Write},
    path::PathBuf,
};

const MAX_LENGTH: usize = 60;

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

fn flush(out: &mut dyn Write, buf: &mut String) -> Result<()> {
    write!(out, "{buf}")?;
    buf.clear();
    Ok(())
}
fn flushln(out: &mut dyn Write, buf: &mut String) -> Result<()> {
    writeln!(out, "{buf}")?;
    buf.clear();
    Ok(())
}
fn flushln_indent(out: &mut dyn Write, buf: &mut String, indent_level: usize) -> Result<()> {
    flushln(out, buf)?;
    indent(out, indent_level)
}

fn indent(out: &mut dyn Write, ident_level: usize) -> Result<()> {
    for _i in 0..ident_level {
        write!(out, "    ")?;
    }
    Ok(())
}
fn run() -> Result<()> {
    let opt = Opt::parse();

    let path = opt.file.to_str().unwrap();
    let source_code =
        fs::read(path).with_context(|| format!("Error reading source file {}", path))?;

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_clingo::language())
        .expect("Error loading clingo grammar");
    let tree = parser.parse(&source_code, None).unwrap();

    let mut buf = std::io::stdout();

    // error_pass(&tree, path)?;
    pass_one(&tree, &source_code, &mut buf, opt.debug)
}

fn pass_one(
    tree: &tree_sitter::Tree,
    source_code: &[u8],
    out: &mut dyn Write,
    debug: bool,
) -> Result<()> {
    let mut cursor = tree.walk();

    let mut nl = false;
    let mut buf = String::new();
    let mut indent_level = 0;
    let mut mindent_level = 0;
    let mut did_visit_children = false;

    let mut in_statement = false;
    let mut in_conjunction = false;
    let mut in_optcondition = false;
    let mut in_termvec = 0;
    let mut in_theory_atom_definition = false;
    let mut has_headlike = false;
    let mut has_body = false;
    let mut is_error = false;

    loop {
        let node = cursor.node();
        let is_named = node.is_named();
        if did_visit_children {
            // what happens after the element
            if is_named {
                match node.kind() {
                    "ERROR" => {
                        is_error = false;
                        flush(out, &mut buf)?;
                    }
                    "statement" => {
                        if !has_headlike || has_body {
                            writeln!(out, "{buf}")?;
                        } else {
                            // no newline after facts.
                            write!(out, "{buf}")?;
                        }
                        buf.clear();

                        //reset properties
                        in_statement = false;
                        has_headlike = false;
                        has_body = false;
                    }
                    "head" | "EDGE" => {
                        has_headlike = true;
                    }
                    "termvec" | "binaryargvec" => {
                        in_termvec -= 1;
                    }
                    "theory_atom_definition" => {
                        in_termvec -= 1;
                        in_theory_atom_definition = false;
                    }
                    "RBRACK" => {
                        mindent_level -= 1;
                        in_termvec -= 1;
                    }
                    "NOT" | "aggregatefunction" | "theory_identifier" => buf.push(' '),
                    "bodydot" => {
                        mindent_level -= 1;
                    }
                    "optcondition" | "optimizecond" => {
                        in_optcondition = false;
                        mindent_level -= 1;
                    }
                    "conjunction" => {
                        in_conjunction = false;
                        mindent_level -= 1;
                    }
                    "LPAREN" => {
                        mindent_level += 1;
                    }
                    "EXTERNAL" | "DEFINED" | "CONST" | "SHOW" | "BLOCK" | "EQ" | "cmp"
                    | "INCLUDE" | "PROJECT" | "HEURISTIC" | "THEORY" | "VBAR" | "MAXIMIZE"
                    | "MINIMIZE" => buf.push(' '),
                    _ => {}
                }
                if node.child_count() == 0 {
                    match node.kind() {
                        "comment" => {
                            if in_statement && nl {
                                writeln!(out)?;
                                indent(out, mindent_level)?;
                            }
                            flushln_indent(out, &mut buf, mindent_level)?;

                            nl = false;
                        }
                        "SEM" => {
                            flushln_indent(out, &mut buf, mindent_level)?;
                            nl = false;
                        }
                        "COLON" => {
                            if in_theory_atom_definition {
                                buf.push(' ');
                            } else {
                                if in_conjunction {
                                    mindent_level += 1;
                                }
                                if in_optcondition {
                                    mindent_level += 1;
                                }
                                flushln_indent(out, &mut buf, mindent_level)?;
                                nl = false;
                            }
                        }
                        "LBRACE" => {
                            if in_theory_atom_definition {
                                buf.push(' ');
                            } else {
                                mindent_level += 1;
                                flushln_indent(out, &mut buf, mindent_level)?;
                                nl = false;
                            }
                        }
                        "COMMA" => {
                            if in_termvec == 0 || buf.len() >= MAX_LENGTH {
                                flushln_indent(out, &mut buf, mindent_level)?;
                                nl = false;
                            } else {
                                buf.push(' ');
                            }
                        }
                        "IF" => {
                            mindent_level += 1; // decrease after bodydot
                            if !has_headlike {
                                buf.push(' ');
                            } else {
                                flushln_indent(out, &mut buf, mindent_level)?;
                                nl = false;
                            }
                        }
                        _ => {
                            nl = true;
                        }
                    };
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
                    "ERROR" => {
                        is_error = true;
                        flushln_indent(out, &mut buf, mindent_level)?;
                    }
                    "comment" => {
                        if in_statement {
                            flush(out, &mut buf)?;
                        } else {
                            if nl {
                                writeln!(out)?;
                            }
                            indent(out, mindent_level)?;
                            flush(out, &mut buf)?;
                        }
                    }
                    "statement" => {
                        if nl {
                            writeln!(out)?;
                        }
                        in_statement = true;
                    }
                    "bodydot" => {
                        has_body = true;
                    }
                    "optcondition" | "optimizecond" => {
                        in_optcondition = true;
                        //incease mindent_level after COLON
                    }
                    "conjunction" => {
                        in_conjunction = true;
                        //incease mindent_level after COLON
                    }
                    "termvec" | "binaryargvec" => {
                        in_termvec += 1;
                    }
                    "theory_atom_definition" => {
                        in_termvec += 1;
                        in_theory_atom_definition = true;
                    }
                    "LBRACK" => {
                        buf.push(' ');
                        in_termvec += 1;
                        mindent_level += 1;
                    }
                    "IF" => {
                        buf.push(' ');
                    }
                    "COLON" | "EQ" | "cmp" => buf.push(' '),
                    "RBRACE" => {
                        if in_theory_atom_definition {
                            buf.push(' ');
                        } else {
                            mindent_level -= 1;
                            flushln_indent(out, &mut buf, mindent_level)?;
                        }
                    }
                    "RPAREN" => {
                        mindent_level -= 1;
                    }
                    _ => {}
                }

                if node.child_count() == 0 || is_error {
                    let start_byte = node.start_byte();
                    let end_byte = node.end_byte();
                    let text = std::str::from_utf8(&source_code[start_byte..end_byte]).unwrap();

                    buf.push_str(text);
                    if debug {
                        debug!("{} ", text);
                    }
                }

                if is_error {
                    let start = node.start_position();
                    let end = node.end_position();
                    let start_byte = node.start_byte();
                    let end_byte = node.end_byte();
                    let text = std::str::from_utf8(&source_code[start_byte..end_byte]).unwrap();

                    warn!(
                        "SYNTAX ERROR at [{}, {}] - [{}, {}]",
                        start.row, start.column, end.row, end.column
                    );
                    warn!("{text}");
                    if node.is_missing() {
                        if node.is_named() {
                            warn!("MISSING {}", node.kind());
                        } else {
                            warn!("MISSING \"{}\"", node.kind().replace('\n', "\\n"));
                        }
                    }
                }
                if debug && !is_error {
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
            } else if node.child_count() == 0 {
                let start_byte = node.start_byte();
                let end_byte = node.end_byte();
                let text = std::str::from_utf8(&source_code[start_byte..end_byte]).unwrap();

                buf.push_str(text);

                if debug {
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
            if is_error {
                did_visit_children = true;
            } else if cursor.goto_first_child() {
                did_visit_children = false;
                indent_level += 1;
            } else {
                did_visit_children = true;
            }
        }
    }

    Ok(())
}

// fn error_pass(tree: &tree_sitter::Tree, path: &str) -> Result<()> {
//     let mut cursor = tree.walk();

//     let mut first_error = None;
//     loop {
//         let node = cursor.node();
//         if node.has_error() {
//             if node.is_error() || node.is_missing() {
//                 first_error = Some(node);
//                 break;
//             } else if !cursor.goto_first_child() {
//                 break;
//             }
//         } else if !cursor.goto_next_sibling() {
//             break;
//         }
//     }
//
//     if first_error.is_some() {
//         error!("{}", path);
//         if let Some(node) = first_error {
//             let start = node.start_position();
//             let end = node.end_position();
//             error!("\t(");
//             if node.is_missing() {
//                 if node.is_named() {
//                     error!("MISSING {}", node.kind());
//                 } else {
//                     error!("MISSING \"{}\"", node.kind().replace("\n", "\\n"));
//                 }
//             } else {
//                 error!("{}", node.kind());
//             }
//             error!(
//                 "[{}, {}] - [{}, {}])",
//                 start.row, start.column, end.row, end.column
//             );
//         }
//     }
//     Ok(())
// }
