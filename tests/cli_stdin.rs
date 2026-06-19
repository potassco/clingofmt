#![warn(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]
//! Integration tests for CLI file and stdin input handling.

use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    time::{SystemTime, UNIX_EPOCH},
};

/// Temporary directory used by CLI integration tests.
struct TestDir {
    /// Filesystem path to the temporary directory.
    path: PathBuf,
}

impl TestDir {
    /// Creates a new unique temporary directory for a test case.
    fn new() -> Self {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let path =
            std::env::temp_dir().join(format!("clingofmt-test-{}-{}", std::process::id(), nanos));
        fs::create_dir_all(&path).unwrap();
        Self { path }
    }

    /// Returns the directory path backing this test fixture.
    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TestDir {
    /// Removes the temporary directory when the fixture is dropped.
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

#[test]
/// Verifies that the CLI formats an explicitly named input file.
fn formats_file_input() {
    let dir = TestDir::new();
    fs::write(dir.path().join("input.lp"), "a:-b.").unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_clingofmt"))
        .current_dir(dir.path())
        .arg("input.lp")
        .output()
        .unwrap();

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "a :- b.\n");
    assert_eq!(
        fs::read_to_string(dir.path().join("input.lp")).unwrap(),
        "a:-b."
    );
}

#[test]
/// Verifies that the long inplace flag rewrites the named input file.
fn formats_file_inplace_with_long_flag() {
    let dir = TestDir::new();
    let input = dir.path().join("input.lp");
    fs::write(&input, "a:-b.").unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_clingofmt"))
        .current_dir(dir.path())
        .arg("--inplace")
        .arg("input.lp")
        .output()
        .unwrap();

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "");
    assert_eq!(fs::read_to_string(input).unwrap(), "a :- b.\n");
}

#[test]
/// Verifies that the short inplace flag rewrites the named input file.
fn formats_file_inplace_with_short_flag() {
    let dir = TestDir::new();
    let input = dir.path().join("input.lp");
    fs::write(&input, "a:-b.").unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_clingofmt"))
        .current_dir(dir.path())
        .arg("-i")
        .arg("input.lp")
        .output()
        .unwrap();

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "");
    assert_eq!(fs::read_to_string(input).unwrap(), "a :- b.\n");
}

#[test]
/// Verifies that inplace mode fails when stdin would otherwise be used.
fn inplace_requires_file_argument() {
    let dir = TestDir::new();

    let output = Command::new(env!("CARGO_BIN_EXE_clingofmt"))
        .current_dir(dir.path())
        .arg("--inplace")
        .stdin(Stdio::piped())
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "");
    assert!(String::from_utf8(output.stderr)
        .unwrap()
        .contains("--inplace requires a FILE argument"));
}

#[test]
/// Verifies that the CLI formats stdin when no file argument is provided.
fn formats_stdin_when_file_is_omitted() {
    let dir = TestDir::new();
    fs::write(
        dir.path().join(".clingofmt"),
        "break_after_head = false\nbreak_after_body_atom = false\nbreak_after_colon = false\nsoft_flush_limit = 60\n",
    )
    .unwrap();

    let mut child = Command::new(env!("CARGO_BIN_EXE_clingofmt"))
        .current_dir(dir.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    child.stdin.as_mut().unwrap().write_all(b"a:-b,c.").unwrap();
    let output = child.wait_with_output().unwrap();

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "a :- b, c.\n");
}
