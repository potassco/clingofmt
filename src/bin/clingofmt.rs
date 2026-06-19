#![warn(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]
//! Command-line entry point for formatting clingo source files or stdin.

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use clingofmt::{format_source, Config};
use log::error;
use std::fs;
use std::{
    io::{self, Read, Write},
    path::{Path, PathBuf},
};

/// Format clingo code
#[derive(Parser, Debug)]
#[clap(version, author)]
struct Opt {
    /// Input file in clingo format; reads from stdin when omitted
    #[clap(name = "FILE")]
    file: Option<PathBuf>,

    /// Enable debug output
    #[clap(long)]
    debug: bool,

    /// Write formatted output back to FILE instead of stdout
    #[clap(short = 'i', long = "inplace")]
    inplace: bool,
}

/// Input reader that abstracts over file and stdin sources.
pub enum Reader<'a> {
    /// Reads formatter input from an on-disk file.
    File(io::BufReader<fs::File>),
    /// Reads formatter input from the process standard input stream.
    Stdin(io::StdinLock<'a>),
}
impl io::Read for Reader<'_> {
    /// Reads more bytes from the active input source into `buf`.
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self {
            Self::File(reader) => reader.read(buf),
            Self::Stdin(guard) => guard.read(buf),
        }
    }
}
impl io::BufRead for Reader<'_> {
    /// Returns the currently buffered unread bytes from the active input source.
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        match self {
            Self::File(reader) => reader.fill_buf(),
            Self::Stdin(guard) => guard.fill_buf(),
        }
    }
    /// Marks `amt` buffered bytes as consumed on the active input source.
    fn consume(&mut self, amt: usize) {
        match self {
            Self::File(reader) => reader.consume(amt),
            Self::Stdin(guard) => guard.consume(amt),
        }
    }
}

/// Initializes logging and exits with a non-zero status on formatter errors.
fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Trace)
        .format_timestamp(None)
        .init();
    if let Err(err) = run() {
        error!("{:?}", err);
        std::process::exit(1);
    }
}

/// Executes the command-line formatter workflow.
fn run() -> Result<()> {
    let opt = Opt::parse();
    if opt.inplace && opt.file.is_none() {
        return Err(anyhow!("--inplace requires a FILE argument"));
    }

    let cfg: Config = confy::load_path(".clingofmt")?;
    let stdin = io::stdin();
    let (mut reader, input_name) = input_reader(opt.file.as_deref(), &stdin)?;
    let mut source_code = Vec::new();
    reader
        .read_to_end(&mut source_code)
        .with_context(|| format!("Error reading {input_name}"))?;

    let mut buf = Vec::new();
    format_source(&source_code, &mut buf, opt.debug, cfg.format_options())?;

    if opt.inplace {
        let path = opt
            .file
            .as_deref()
            .expect("inplace mode was validated to require a file");
        fs::write(path, &buf)
            .with_context(|| format!("Error writing formatted output to {}", path.display()))?;
    } else {
        let mut out = std::io::stdout();
        let buf_str = std::str::from_utf8(&buf)?;
        write!(out, "{buf_str}")?;
    }
    Ok(())
}

/// Opens the configured formatter input and returns it together with a label
/// suitable for diagnostics.
fn input_reader<'a>(file: Option<&Path>, stdin: &'a io::Stdin) -> Result<(Reader<'a>, String)> {
    match file {
        Some(path) => Ok((
            Reader::File(io::BufReader::new(fs::File::open(path).with_context(
                || format!("Error opening source file {}", path.display()),
            )?)),
            path.display().to_string(),
        )),
        None => Ok((Reader::Stdin(stdin.lock()), String::from("stdin"))),
    }
}
