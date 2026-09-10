use std::{fs, path::PathBuf};

use anyhow::{bail, Context, Result};
use git_version::git_version;

mod env;
mod ast;
mod core;
mod lex;
mod vir;
mod codegen;

mod dogfood;

use env::Env;

const FOOTLE_FILE_EXTENSION: &str = "ftl";


//-------------------------------------------------------------------------------------------------

fn main() {
    show_version();
    let mut args = pico_args::Arguments::from_env();

    std::process::exit(match run(&mut args) {
        Ok(()) => 0,
        Err(err) => {
            eprintln!("error: {err:#}");
            1
        }
    });
}


fn run(args: &mut pico_args::Arguments) -> Result<()> {
    if args.contains(["-h", "--help"]) {
        show_help();
        return Ok(());
    }

    let verbose = args.contains(["-v", "--verbose"]);
    let test = args.contains(["-t", "--test"]);
    let Some(file_or_dir): Option<PathBuf> = args.opt_free_from_str()? else { return Ok(()); };
    if !file_or_dir.try_exists()? {
        bail!("no such file or directory: {}", file_or_dir.display());
    }
    if test {
        dogfood::run_tests(&file_or_dir)?;
    } else {
        let remaining = args.clone().finish();
        let arguments: Vec<f64> = remaining.iter()
            .map(|s| s.to_str().and_then(|s| s.parse().ok())
                .ok_or_else(|| anyhow::anyhow!("invalid numeric argument '{}'", s.display())))
            .collect::<Result<Vec<f64>>>()?;

        if verbose {
            dogfood::run_file_verbose(&file_or_dir, &arguments)?;
        } else {
            run_file(&file_or_dir, &arguments)?;
        }
    }

    Ok(())
}


fn show_version() {
    let name: &str = env!("CARGO_PKG_NAME");
    let version: &str = env!("CARGO_PKG_VERSION");
    let git_version: &str = git_version!();
    eprintln!("{name} {version} ({git_version})");
}


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


//-------------------------------------------------------------------------------------------------

/// Compile and run a single file.
///
/// Read in the file specified, process it and show any output.
fn run_file(file_path: &PathBuf, arguments: &[f64]) -> Result<()> {
    let file_name = file_path.display().to_string();
    let source =
        fs::read_to_string(file_path)
            .with_context(|| format!("couldn't read '{file_name}'"))?;

    let (stmts, errors, source_map) = ast::parse(&file_name, source.as_str());
    report_errors(&errors, &file_name, &source_map)?;

    let env = Env::new();
    let (vir_block, vir_errors) = vir::run(&env, &stmts);
    report_errors(&vir_errors, &file_name, &source_map)?;

    let func = codegen::run(&vir_block);
    let results = func.call(arguments)?;
    println!("{}", results.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(" "));

    Ok(())
}


fn report_errors(errors: &[core::ParseError], file_name: &str, source_map: &core::SourceMap<&str>
    ) -> Result<()> {
    if !errors.is_empty() {
        eprintln!("\nErrors from '{file_name}':");
        for e in errors {
            eprint!("{}", e.show_in_source(source_map));
        }
        bail!("Syntax errors")
    }
    Ok(())
}


//-------------------------------------------------------------------------------------------------
