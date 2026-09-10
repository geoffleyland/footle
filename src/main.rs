use std::{
    collections::HashMap,
    fs,
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Result};
use git_version::git_version;

mod env;
mod ast;
mod core;
mod lex;
mod vir;
mod codegen;

use core::Styleable;
use env::Env;

const FOOTLE_FILE_EXTENSION: &str = "txt";


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
        run_tests(&file_or_dir)?;
    } else {
        let remaining = args.clone().finish();
        let arguments: Vec<f64> = remaining.iter()
            .map(|s| s.to_str().and_then(|s| s.parse().ok())
                .ok_or_else(|| anyhow::anyhow!("invalid numeric argument '{}'", s.display())))
            .collect::<Result<Vec<f64>>>()?;

        if verbose {
            run_file_verbose(&file_or_dir, &arguments)?;
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

/// Compile and run a single file noisily
///
/// Read in the file specified, process it, and tell everyone about it.
fn run_file_verbose(file_path: &PathBuf, arguments: &[f64]) -> Result<()> {
    let file_name = file_path.display().to_string();
    eprintln!("Opening '{file_name}'");
    let source =
        fs::read_to_string(file_path)
            .with_context(|| format!("couldn't read '{file_name}'"))?;
    eprintln!("Contents of '{file_name}':\n  {}", source.lines().collect::<Vec<_>>().join("\n  "));

    let (stmts, errors, source_map) = ast::parse(&file_name, source.as_str());
    let style = core::SourceStyle::new(2, 40, true, &source_map);

    report_errors(&errors, &file_name, &source_map)?;
    eprintln!("\nStatements from '{file_name}':");
    for stmt in &stmts { eprintln!("{}", stmt.styled(1, &style)); }

    let env = Env::new();
    let (vir_block, vir_errors) = vir::run(&env, &stmts);
    report_errors(&vir_errors, &file_name, &source_map)?;
    eprintln!("\nVIR instructions from '{file_name}':");
    eprintln!("{}", vir_block.styled(1, &style));

    let schedule = codegen::schedule(&vir_block);
    eprintln!("\nScheduled instructions from '{file_name}':");
    eprintln!("{}", schedule.styled(1, &style));

    let assembler = codegen::assemble(&vir_block);
    eprintln!("\nAssembly instructions from '{file_name}':");
    eprintln!("{}", assembler.styled(1, &style));

    let func = codegen::run(&vir_block);

    eprintln!("\nDisassembly from '{file_name}':");
    for line in codegen::disassemble(&func) { eprintln!("  {line}"); }

    let results = func.call(arguments)?;
    println!("\nResult from '{file_name}':");
    println!("  f({}) = ({})",
        arguments.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(", "),
        results.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(", "));

    Ok(())
}


//-------------------------------------------------------------------------------------------------

fn run_tests(dir_path: &PathBuf) -> Result<()> {
    let mut paths = vec![];
    if dir_path.is_dir() {
        find_tests(dir_path, &mut paths)?;
        println!("Testing {} files in '{}' ...", paths.len(), dir_path.display());
    } else {
        if let Some(e) = dir_path.extension() && e == FOOTLE_FILE_EXTENSION {
            paths.push(dir_path.clone());
        }
        if paths.is_empty() {
            bail!("No files.");
        }
        println!("Testing '{}' ...", dir_path.display());
    }

    let max_filename_width = paths.iter().map(|p|
        p.strip_prefix(dir_path).unwrap_or(p).as_os_str().len()).max().unwrap_or(0);
    let filename_width = (max_filename_width / 4 + 1) * 4;

    let mut tests = 0;
    let mut fails = 0;
    for p in paths {
        tests += 1;
        print!("  {:filename_width$}", p.strip_prefix(dir_path).unwrap_or(&p).display());
        std::io::stdout().flush()?;
        match run_test(&p) {
            Ok(()) => {
                println!("\x1b[1;32m\u{2713}\x1b[0m");
            }
            Err(e) => {
                println!("\x1b[1;31m\u{2718}\x1b[0m");
                eprintln!("{e:#}");
                fails += 1;
            }
        }
    }

    if fails > 0 {
        println!("\ntest result: \x1b[31mFAILED\x1b[0m. {} passed; {fails} failed", tests - fails);
        bail!("tests failed")
    }

    println!("\ntest result: \x1b[32mok\x1b[0m. {tests} passed; 0 failed");
    Ok(())
}


fn find_tests(dir: &Path, paths: &mut Vec<PathBuf>) -> Result<()> {
    for entry in dir.read_dir()? {
        let entry = entry
            .with_context(||format!("Problem reading {}", dir.display()))?;
        let path = entry.path();
        if path.is_dir() {
            find_tests(&path, paths)?;
        } else if let Some(e) = path.extension() && e == FOOTLE_FILE_EXTENSION {
            paths.push(path);
        }
    }
    Ok(())
}


/// Run a test file.
///
/// A magical, own-dogfood eating tester.
/// Read the code, which is broken into sections:
///  * source - whatever comes at the start - the program
///  * errors - inside an #( expected errors .... #) block comment - expected errors, if any
///  * statements - inside #( expected statements ... #) - expected pretty-printed statements
///  * schedule - inside #( expected schedule ... #) - expected scheduler output - no longer
///    written as machine-readable code, though.
///  * assembler - inside #( expected assembler ... #) - assembler output - maybe one day readable
///    by a proper assembler?
///  * result - inside #( expected result ... #) - if the code is a single-argument function with
///    one result, the result of calling f(42.0)
///
/// The clever thing is that all the excess stuff is block comments, so the files are still
/// legitimate programs.
/// First, parse the code.  If there are errors, check they match the expected errors. If there are
/// no errors (and that's what we wanted), check the statements match expectations. If that worked,
/// take the output from the parser (actually, the expected output, but we already checked they're
/// the same), and run it back through the parser, checking that we get the same result as before.
fn run_test(path: &Path) -> Result<()> {
    let expected = read_test_file(path)?;

    for key in ["source", "statements", "vir"] {
        let source = expected.get(key).unwrap_or(&vec![]).join("\n");
        test_lines(&path.to_string_lossy(), key, &source, &expected)?;
    }

    Ok(())
}


fn read_test_file(path: &Path) -> Result<HashMap<String, Vec<String>>> {
    let file =
        fs::File::open(path)
            .with_context(|| format!("couldn't open '{}'", path.display()))?;

    // Read the file, putting all the bits into the right buffers.
    let mut expected = HashMap::<String, Vec<String>>::new();
    let mut mode = "source".to_string();

    for line in BufReader::new(file).lines().map_while(Result::ok) {
        if let Some(raw_mode) = line.strip_prefix("#( expected") {
            mode = raw_mode.trim().to_lowercase();
            expected.entry(mode.clone()).or_default();
        } else if !line.is_empty() && (mode == "source" || !line.starts_with("#)")) {
            expected.entry(mode.clone()).or_default().push(line);
        } else if mode != "source" && line.starts_with("#)") {
            mode = "source".to_string();
        }
    }
    Ok(expected)
}


fn test_lines(
    file_name: &str,
    section: &str,
    source: &str,
    expected: &HashMap<String, Vec<String>>,
) -> Result<()> {
    let (stmts, errors, _) = ast::parse(file_name, source);

    let mut checking = section == "source";
    if checking {
        let error_strings: Vec<_> = errors.iter().map(|e| format!("{e}")).collect();
        compare_lines(&error_strings, expected.get("errors").unwrap_or(&vec![]), section, "errors")?;
        if expected.contains_key("errors") {
            return Ok(());
        }
    }

    checking |= section == "statements";
    if checking && expected.contains_key("statements") {
        let string_stmts = stmts_to_strings(&stmts);
        compare_lines(&string_stmts, &expected["statements"], section, "statements")?;
    }

    let env = Env::new();
    let (vir_block, vir_errors) = vir::run(&env, &stmts);
    checking |= section == "vir";
    if checking {
        if section == "source" { // only check errors the first time around
            let error_strings: Vec<_> = vir_errors.iter().map(|e| format!("{e}")).collect();
            compare_lines(&error_strings, expected.get("vir-errors").unwrap_or(&vec![]), section, "vir-errors")?;
        }
        if expected.contains_key("vir-errors") {
            return Ok(());
        }
    }

    if checking && expected.contains_key("vir") {
        compare_lines(&block_to_strings(&vir_block), &expected["vir"], section, "vir")?;
    }

    // Once we get to the scheduling and assembler passes, we only do that for the source pass
    // (since we're already proving that the other passes all give the same output, and because
    // we can't feed the output of these passes back into the compiler), and we only do it if the
    // expected output is present (because the compiler is being implemented bit by bit and if
    // we run something NYI, we get an NYI and a panic.)
    if expected.contains_key("schedule") && section == "source" {
        let schedule = codegen::schedule(&vir_block);
        compare_lines(&block_to_strings(&schedule), &expected["schedule"], section, "schedule")?;
    }

    if expected.contains_key("assembler") && section == "source" {
        let assembler = codegen::assemble(&vir_block);
        compare_lines(&block_to_strings(&assembler), &expected["assembler"], section, "assembler")?;
    }

    if (expected.contains_key("assembler") || expected.contains_key("results")) && section == "source" {
        let func = codegen::run(&vir_block);

        if expected.contains_key("assembler") {
            let disassembled = &codegen::disassemble(&func);
            let expected_disassembled = &expected["assembler"][0..disassembled.len()];
            compare_lines(disassembled, expected_disassembled, section, "disassembler")?;
        }

        if expected.contains_key("results") {
            test_results(&func, &expected["results"], section)?;
        }
    }

    Ok(())
}


fn test_results(func: &codegen::CompiledFn, expected: &[String], section: &str) -> Result<()> {
    let mut actual_strings = vec![];
    for line in expected {
        let Some((inputs_str, _)) = line.split_once("->") else {
            bail!("    invalid result line: {line:?}");
        };
        let inputs = inputs_str.split_whitespace()
            .map(str::parse::<f64>)
            .collect::<Result<Vec<_>, _>>()
            .with_context(|| format!("    invalid input in {line:?}"))?;

        let actual_outputs = func.call(&inputs)
            .with_context(|| format!("    invalid input in {line:?}"))?;

        actual_strings.push(format!("{} -> {}",
            inputs.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(" "),
            actual_outputs.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(" ")));
    }
    compare_lines(&actual_strings, expected, section, "results")?;

    Ok(())
}


fn compare_lines(
    actual: &[String],
    expected: &[String],
    input: &str,
    output: &str,
) -> Result<()> {
    if actual.len() != expected.len()
        || actual.iter().zip(expected).any(|(a, e)| a.trim() != e.trim()) {
        bail!(
            "Mismatch between expected and obtained {output} from {input}.  Test output:\n\
            #( expected {output}\n\n  {}\n\n#)",
            actual.join("\n  "))
    }
    Ok(())
}


/// Turn a list of statements into a list of strings
///
/// Statements can be multi-line (e.g. block assignments), so we format each one and split on
/// newlines to get a flat list of non-empty lines for comparison.
fn stmts_to_strings<S: core::Styleable>(stmts: &[S]) -> Vec<String> {
    let style = core::IndentedStyle::new(2);
    stmts
        .iter()
        .flat_map(|stmt| {
            format!("{}", stmt.styled(0, &style))
                .lines()
                .filter(|l| !l.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .collect()
}


/// Turn anything Styleable into a list of strings
///
/// The thing can return multiple lines so we format the whole lot and split on
/// newlines to get a flat list of non-empty lines for comparison.
fn block_to_strings<S: core::Styleable>(block: &S) -> Vec<String> {
    let style = core::IndentedStyle::new(2);
    format!("{}", block.styled(0, &style))
        .lines()
        .filter(|l| !l.is_empty())
        .map(str::to_string)
        .collect::<Vec<_>>()
}


//-------------------------------------------------------------------------------------------------
