use std::{
    fs,
    io::{self, Read, stdout},
};

use bugreport::{
    bugreport,
    collector::{CompileTimeInformation, EnvironmentVariables, OperatingSystem, SoftwareVersion},
    format::Markdown,
};
use clap::{Arg, ArgAction, ArgMatches, Command, command};
use compiler::vm::VirtualMachine;
use interpreter::{ast::Stmt, int::Interpreter, parser::Parser, resolver::Resolver};
use miette::{Context, IntoDiagnostic, miette};

#[macro_use]
extern crate clap;

const INTERPRET_CMD: &str = "n";
const COMPILE_CMD: &str = "c";
const BUGREPORT_CMD: &str = "bugreport";
const PRINTCODE_OPT: &str = "printcode";
const PATH_OPT: &str = "PATH";

fn main() -> miette::Result<()> {
    let app = build_cli();
    let matches = app.get_matches();

    match matches.subcommand() {
        Some((INTERPRET_CMD, cmd)) => interpretation(cmd),
        Some((COMPILE_CMD, cmd)) => compilation(cmd),
        Some((BUGREPORT_CMD, cmd)) => {
            print_bugreport(cmd);
            Ok(())
        }
        _ => Ok(()),
    }
}

fn interpretation(cmd: &ArgMatches) -> miette::Result<()> {
    let path = cmd.get_one::<String>(PATH_OPT);
    if let Some(p) = path {
        from_file(p, interpret)
    } else {
        from_stdin(interpret)
    }
}

fn compilation(cmd: &ArgMatches) -> miette::Result<()> {
    let printcode = cmd.get_flag(PRINTCODE_OPT);
    let path = cmd.get_one::<String>(PATH_OPT);
    if let Some(p) = path {
        from_file(p, |s| compile(s, printcode))
    } else {
        from_stdin(|s| compile(s, printcode))
    }
}

/// Scans file specified.
///
/// # Errors
///
/// This function will return an error if scanning failed.
fn from_file<F: Fn(String) -> miette::Result<()>>(path: &String, action: F) -> miette::Result<()> {
    let content = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err(format!("Failed to read: {path}"))?;
    action(content)?;
    Ok(())
}

/// Scans data from standard input.
///
/// # Errors
///
/// This function will return an error if scanning failed.
fn from_stdin<F: Fn(String) -> miette::Result<()>>(action: F) -> miette::Result<()> {
    let mut content = String::new();
    io::stdin()
        .read_to_string(&mut content)
        .into_diagnostic()
        .wrap_err("Failed to read stdin content")?;
    action(content)?;
    Ok(())
}

fn interpret(content: String) -> miette::Result<()> {
    let mut parser = Parser::new(&content);
    let interpreter = Interpreter::new(stdout());
    let resolver = Resolver::new(interpreter);
    let stmts: Vec<interpreter::Result<Stmt>> = parser.collect();
    resolver.interpret(&stmts).map_err(|err| match err {
        interpreter::LoxError::Error(e) => e.with_source_code(content),
        interpreter::LoxError::Return(val) => miette!("Unexpected return value: {val}"),
    })
}

fn compile(content: String, printcode: bool) -> miette::Result<()> {
    let mut vm = VirtualMachine::new(stdout());
    vm.init();

    vm.interpret(&content, printcode)
        .map_err(|err| err.with_source_code(content))
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
        .subcommand(interpret_cmd())
        .subcommand(compile_cmd())
        .subcommand(bugreport_cmd())
}

fn compile_cmd() -> Command {
    Command::new(COMPILE_CMD)
        .aliases(["compile"])
        .about("Use bytecode compiler")
        .arg(printcode_arg())
        .arg(path_arg())
}

fn interpret_cmd() -> Command {
    Command::new(INTERPRET_CMD)
        .aliases(["interpret"])
        .about("Use interpreter")
        .arg(path_arg())
}

fn bugreport_cmd() -> Command {
    Command::new(BUGREPORT_CMD)
        .about("Collect information about the system and the environment that users can send along with a bug report")
}

fn printcode_arg() -> Arg {
    Arg::new(PRINTCODE_OPT)
        .long(PRINTCODE_OPT)
        .required(false)
        .action(ArgAction::SetTrue)
        .help("Output generated code")
}

fn path_arg() -> Arg {
    Arg::new(PATH_OPT)
        .required(false)
        .help("Code file path. If not provided, read from stdin.")
}
