use anyhow::{Context, Result};
use log::error;
use std::fs;
use std::{
    io::{self, Write},
    path::PathBuf,
};
use structopt::StructOpt;

/// Format clingo code
#[derive(StructOpt, Debug)]
#[structopt(name = "clingofmt")]
struct Opt {
    /// Input file in clingo format
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,
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
        .verbosity(2)
        .init()
        .unwrap();
    if let Err(err) = run() {
        error!("{:?}", err);
        std::process::exit(1);
    }
}
fn run() -> Result<()> {
    let opt = Opt::from_args();

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

    let mut needs_space = false;
    let mut needs_newline = false;
    let mut in_body = false;
    let mut in_body_agg = false;
    let mut in_condition = false;
    let mut in_literal = false;
    let mut indent_level = 0;
    let mut did_visit_children = false;
    loop {
        let node = cursor.node();
        let is_named = node.is_named();
        if did_visit_children {
            // what happens after the element
            if is_named {
                match node.kind() {
                    "statement" | "comment" => {
                        writeln!(&mut stdout)?;
                    }
                    "head" | "NOT" | "aggregatefunction" => needs_space = true,
                    "IF" => {
                        needs_space = true;
                    }
                    "literal" => {
                        in_literal = false;
                    }
                    "bodydot" => {
                        in_body = false;
                    }
                    "lubodyaggregate" => {
                        in_body_agg = false;
                    }
                    "noptcondition" => {
                        in_condition = false;
                    }
                    "COLON" | "LBRACE" => needs_space = true,
                    "DOT" => {
                        if in_body {
                            needs_newline = true;
                        }
                    }
                    "SHOW" => needs_space = true,
                    _ => {}
                }
                if needs_newline {
                    writeln!(&mut stdout)?;
                } else if needs_space {
                    write!(&mut stdout, " ")?;
                }
                needs_newline = false;
                needs_space = false;
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
                    "bodydot" => {
                        in_body = true;
                    }
                    "noptcondition" => {
                        in_condition = true;
                    }
                    "literal" => {
                        if in_body && !in_literal {
                            write!(&mut stdout, "\n    ")?;
                        }
                        if in_body_agg {
                            write!(&mut stdout, "    ")?;
                        }
                        if in_condition {
                            write!(&mut stdout, "    ")?;
                        }
                        in_literal = true;
                    }
                    "lubodyaggregate" => {
                        if in_body && !in_literal {
                            write!(&mut stdout, "\n    ")?;
                        }
                        // in_literal = true;
                        in_body_agg = true;
                    }
                    "COLON" => write!(&mut stdout, " ")?,
                    "RBRACE" => {
                        if in_body {
                            write!(&mut stdout, "\n    ")?;
                        }
                    }
                    _ => {}
                }

                if node.child_count() == 0 {
                    let start_byte = node.start_byte();
                    let end_byte = node.end_byte();
                    let text = std::str::from_utf8(&source_code[start_byte..end_byte]).unwrap();

                    // eprint!("{} ", text);
                    write!(&mut stdout, "{}", text)?;
                }

                // for _ in 0..indent_level {
                //     eprint!("  ");
                // }
                // let start = node.start_position();
                // let end = node.end_position();
                // if let Some(field_name) = cursor.field_name() {
                //     eprint!("{}: ", field_name);
                // }

                // eprintln!(
                // "({} [{}, {}] - [{}, {}]",
                // node.kind(),
                // start.row,
                // start.column,
                // end.row,
                // end.column
                // );
            } else if node.child_count() == 0 {
                let start_byte = node.start_byte();
                let end_byte = node.end_byte();
                let text = std::str::from_utf8(&source_code[start_byte..end_byte]).unwrap();

                // eprint!("{}", text);
                write!(&mut stdout, "{}", text)?;

                // for _ in 0..indent_level {
                //     eprint!("  ");
                // }

                // let start = node.start_position();
                // let end = node.end_position();
                // if let Some(field_name) = cursor.field_name() {
                //     eprint!("{}: ", field_name);
                // }
                // eprintln!(
                //     "({} [{}, {}] - [{}, {}]",
                //     node.kind(),
                //     start.row,
                //     start.column,
                //     end.row,
                //     end.column
                // );
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
    println!();

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
        write!(&mut stdout, "{}", path)?;
        if let Some(node) = first_error {
            let start = node.start_position();
            let end = node.end_position();
            write!(&mut stdout, "\t(")?;
            if node.is_missing() {
                if node.is_named() {
                    write!(&mut stdout, "MISSING {}", node.kind())?;
                } else {
                    write!(
                        &mut stdout,
                        "MISSING \"{}\"",
                        node.kind().replace("\n", "\\n")
                    )?;
                }
            } else {
                write!(&mut stdout, "{}", node.kind())?;
            }
            write!(
                &mut stdout,
                " [{}, {}] - [{}, {}])",
                start.row, start.column, end.row, end.column
            )?;
        }
        writeln!(&mut stdout)?;
    }

    // return Ok(first_error.is_some());

    Ok(())
}
