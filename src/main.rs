use std::{
    fs,
    io::{self, Read, stdout},
};

use bugreport::{
    bugreport,
    collector::{CompileTimeInformation, EnvironmentVariables, OperatingSystem, SoftwareVersion},
    format::Markdown,
};
use clap::{ArgMatches, Command, command};
use miette::{Context, IntoDiagnostic, miette};
use rlox::{ast::Stmt, int::Interpreter, parser::Parser, resolver::Resolver};

#[macro_use]
extern crate clap;

const FILE_CMD: &str = "f";
const STDIN_CMD: &str = "i";
const BUGREPORT_CMD: &str = "bugreport";

const PATH: &str = "PATH";

fn main() -> miette::Result<()> {
    let app = build_cli();
    let matches = app.get_matches();

    match matches.subcommand() {
        Some((FILE_CMD, cmd)) => scan_file(cmd),
        Some((STDIN_CMD, cmd)) => scan_stdin(cmd),
        Some((BUGREPORT_CMD, cmd)) => {
            print_bugreport(cmd);
            Ok(())
        }
        _ => Ok(()),
    }
}

/// Scans file specified.
///
/// # Errors
///
/// This function will return an error if scanning failed.
fn scan_file(cmd: &ArgMatches) -> miette::Result<()> {
    let path = cmd
        .get_one::<String>(PATH)
        .ok_or(miette! {"Path required"})?;
    let content = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err(format!("Failed to read: {path}"))?;
    scan(content)?;
    Ok(())
}

/// Scans data from standard input.
///
/// # Errors
///
/// This function will return an error if scanning failed.
fn scan_stdin(_cmd: &ArgMatches) -> miette::Result<()> {
    let mut content = String::new();
    io::stdin()
        .read_to_string(&mut content)
        .into_diagnostic()
        .wrap_err("Failed to read stdin content")?;
    scan(content)?;
    Ok(())
}

fn scan(content: String) -> miette::Result<()> {
    let mut parser = Parser::new(&content);
    let interpreter = Interpreter::new(stdout());
    let resolver = Resolver::new(interpreter);
    let stmts: Vec<rlox::Result<Stmt>> = parser.collect();
    resolver.interpret(&stmts).map_err(|err| match err {
        rlox::LoxError::Error(e) => e.with_source_code(content),
        rlox::LoxError::Return(val) => miette!("Unexpected return value: {val}"),
    })
}

fn print_bugreport(_matches: &ArgMatches) {
    bugreport!()
        .info(SoftwareVersion::default())
        .info(OperatingSystem::default())
        .info(EnvironmentVariables::list(&["SHELL", "TERM"]))
        .info(CompileTimeInformation::default())
        .print::<Markdown>();
}

fn build_cli() -> Command {
    #![allow(non_upper_case_globals)]
    command!(crate_name!())
        .arg_required_else_help(true)
        .version(crate_version!())
        .author(crate_authors!("\n"))
        .about(crate_description!())
        .subcommand(file_cmd())
        .subcommand(stdin_cmd())
        .subcommand(bugreport_cmd())
}

fn file_cmd() -> Command {
    Command::new(FILE_CMD)
        .aliases(["file"])
        .about("Interpret file specified")
        .arg(
            arg!([PATH])
                .help("Sets file path to interpret")
                .required(true),
        )
}

fn stdin_cmd() -> Command {
    Command::new(STDIN_CMD)
        .aliases(["stdin"])
        .about("Use data from standard input")
}

fn bugreport_cmd() -> Command {
    Command::new(BUGREPORT_CMD)
        .about("Collect information about the system and the environment that users can send along with a bug report")
}
