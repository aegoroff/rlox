use std::{
    fs,
    io::{self, Read},
};

use bugreport::{
    bugreport,
    collector::{CompileTimeInformation, EnvironmentVariables, OperatingSystem, SoftwareVersion},
    format::Markdown,
};
use clap::{ArgMatches, Command, command};
use miette::{Context, IntoDiagnostic, miette};
use rlox::lexer::Lexer;

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

fn scan_file(cmd: &ArgMatches) -> miette::Result<()> {
    let path = cmd
        .get_one::<String>(PATH)
        .ok_or(miette! {"Path required"})?;
    let content = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err(format!("Failed to read: {path}"))?;
    scan(content)
}

fn scan_stdin(_cmd: &ArgMatches) -> miette::Result<()> {
    let mut content = String::new();
    io::stdin()
        .read_to_string(&mut content)
        .into_diagnostic()
        .wrap_err("Failed to read stdin content")?;
    scan(content)
}

fn scan(content: String) -> miette::Result<()> {
    let scanner = Lexer::new(&content);
    let mut errors = vec![];
    for t in scanner {
        match t {
            Ok(t) => println!("{t}"),
            Err(e) => {
                if let Some(l) = e.labels() {
                    errors.extend(l);
                }
            }
        }
    }
    if !errors.is_empty() {
        let report = miette!(labels = errors, "errors occured");
        println!("{:?}", report.with_source_code(content))
    }
    Ok(())
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
