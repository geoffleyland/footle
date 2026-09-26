use std::{fs, path::PathBuf};

use anyhow::{bail, Context, Result};
use git_version::git_version;

mod env;
mod ast;
mod core;
mod lex;
mod vir;
mod codegen;
mod runtime;

#[cfg(feature = "dogfood")]
mod dogfood;

#[cfg(feature = "dogfood")]
const FOOTLE_FILE_EXTENSION: &str = "ftl";


//-------------------------------------------------------------------------------------------------

fn main() {
    show_version();
    let args = pico_args::Arguments::from_env();

    std::process::exit(match run(args) {
        Ok(()) => 0,
        Err(err) => {
            if let Some(diags) = err.downcast_ref::<runtime::Diagnostics>() {
                eprint!("{diags}");          // already has its own styled "error" labels
            } else {
                eprintln!("\x1b[1;31merror\x1b[0m\x1b[1m: {err:#}\x1b[0m");
            }
            1
        }
    });
}


#[cfg(not(feature = "dogfood"))]
fn run(mut args: pico_args::Arguments) -> Result<()> {
    if args.contains(["-h", "--help"]) {
        show_help();
        return Ok(());
    }

    let Some(file_or_dir): Option<PathBuf> = args.opt_free_from_str()? else { return Ok(()); };
    if !file_or_dir.try_exists()? {
        bail!("couldn't read {}: No such file or directory", file_or_dir.display());
    }
    run_file(&file_or_dir, &parse_arguments(args)?, &mut runtime::Silent)?;
    Ok(())
}

#[cfg(not(feature = "dogfood"))]
fn show_help() {
    let name: &str = env!("CARGO_PKG_NAME");
    eprintln!("\
Usage: {name} [<file> [<arguments...>]]
       {name} -h

Options:
  -h, --help     display usage information
");
}


#[cfg(feature = "dogfood")]
fn run(mut args: pico_args::Arguments) -> Result<()> {
    if args.contains(["-h", "--help"]) {
        show_help();
        return Ok(());
    }

    let verbose = args.contains(["-v", "--verbose"]);
    let test = args.contains(["-t", "--test"]);
    let Some(file_or_dir): Option<PathBuf> = args.opt_free_from_str()? else { return Ok(()); };
    if !file_or_dir.try_exists()? {
        bail!("couldn't read {}: No such file or directory", file_or_dir.display());
    }
    if test {
        dogfood::run_tests(&file_or_dir)?;
    } else {
        let arguments = parse_arguments(args)?;
        if verbose {
//            dogfood::run_file_verbose(&file_or_dir, &arguments)?;
            run_file(&file_or_dir, &arguments, &mut dogfood::Printer::new(2, 40, true))?;
        } else {
            run_file(&file_or_dir, &arguments, &mut runtime::Silent)?;
        }
    }

    Ok(())
}


#[cfg(feature = "dogfood")]
fn show_help() {
    let name: &str = env!("CARGO_PKG_NAME");
    eprintln!("\
Usage: {name} [-v] [<file> [<arguments...>]]
       {name} -t <file_or_dir>
       {name} -h

Options:
  -t, --test     check all the files in dogfood mode
  -v, --verbose  show more output when running a file
  -h, --help     display usage information
");
}


fn show_version() {
    let name: &str = env!("CARGO_PKG_NAME");
    let version: &str = env!("CARGO_PKG_VERSION");
    let profile = if cfg!(debug_assertions) { "debug" } else { "release" };
    let git_version: &str = git_version!();
    eprintln!("{name} {version} ({profile}, {git_version})");
}


fn parse_arguments(args: pico_args::Arguments) -> Result<Vec<runtime::Value>> {
    args.finish().iter()
        .map(|s| s.to_str().and_then(|s| s.parse().ok())
            .ok_or_else(|| anyhow::anyhow!("invalid argument '{}'", s.display())))
        .collect()
}


//-------------------------------------------------------------------------------------------------

/// Compile and run a single file.
///
/// Read in the file specified, process it and show any output.
fn run_file<O: runtime::Observer>(
    file_path:          &PathBuf,
    arguments:          &[runtime::Value],
    observer:           &mut O
) -> Result<()> {
    let file_name = file_path.display().to_string();
    let source =
        fs::read_to_string(file_path)
            .with_context(|| format!("couldn't read '{file_name}'"))?;

    let mut block = runtime::load(&file_name, source, observer)?;
    let results = block.call(arguments, observer)?;

    println!("{}", core::join_format(&results, " "));

    Ok(())
}


//-------------------------------------------------------------------------------------------------
