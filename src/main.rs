use std::{
    collections::HashMap,
    error::Error,
    fs,
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
};

use argh::FromArgs;
use git_version::git_version;

mod ast;
mod core;
mod lex;
mod vir;
mod codegen;

use core::Styleable;


//-------------------------------------------------------------------------------------------------
// Command-line arguments

#[derive(FromArgs)]
/// footle.  Nearly a parser?
struct Args {
    /// run in "test" mode, comparing output to comments in the file
    #[argh(switch, short = 't')]
    test:        bool,
    /// file to test or directory (recursively) containing files to run/test
    #[argh(positional)]
    file_or_dir: Option<String>,
}


//-------------------------------------------------------------------------------------------------

fn main() {
    let args: Args = argh::from_env();

    write_version();

    let result = args.file_or_dir.as_ref().map_or_else(
        || Ok(()),
        |file_or_dir| {
            if args.test { run_tests(file_or_dir) } else { run_file(file_or_dir) }
        },
    );

    std::process::exit(match result {
        Ok(()) => 0,
        Err(err) => {
            eprintln!("error: {err}");
            1
        }
    })
}


fn write_version() {
    let name: &str = env!("CARGO_PKG_NAME");
    let version: &str = env!("CARGO_PKG_VERSION");
    let git_version: &str = git_version!();
    eprintln!("{name} {version} ({git_version})");
}


//-------------------------------------------------------------------------------------------------

/// Compile and run a single file
///
/// Read in the file specified, and process it!
fn run_file(file_name: &str) -> Result<(), Box<dyn Error>> {
    eprintln!("Opening '{file_name}'");
    let source =
        fs::read_to_string(file_name).map_err(|e| format!("couldn't open '{file_name}': {e}"))?;
    println!("Contents of '{file_name}':\n  {}", source.lines().collect::<Vec<_>>().join("\n  "));

    let (stmts, errors, source_map) = ast::parse(file_name, source.as_str());
    let style = core::SourceStyle::new(2, 40, true, &source_map);

    if errors.is_empty() {
        println!("\nStatements from '{file_name}':");
        for stmt in &stmts {
            println!("{}", stmt.styled(1, &style));
        }
    } else {
        println!("\nErrors from '{file_name}':");
        for e in errors {
            print!("{}", e.show_in_source(&source_map));
        }
        return Err("Syntax errors".into())
    }

    let (vir_block, vir_errors) = vir::run(&stmts);
    if vir_errors.is_empty() {
        let instrs = vir::instructions(&vir_block);
        println!("\nVIR instructions from '{file_name}':");
        for instr in &instrs {
            println!("{}", instr.styled(1, &style));
        }
    } else {
        println!("\nErrors from '{file_name}':");
        for e in vir_errors {
            print!("{}", e.show_in_source(&source_map));
        }
        return Err("Syntax errors".into())
    }

    let schedule = codegen::schedule(&vir_block);
    println!("\nScheduled instructions from '{file_name}':");
    println!("{}", schedule.styled(1, &style));

    let assembler = codegen::assemble(&vir_block);
    println!("\nAssembly instructions from '{file_name}':");
    println!("{}", assembler.styled(1, &style));

    let func = codegen::run(&vir_block);

    println!("\nDisassembly from '{file_name}':");
    for line in codegen::disassemble(&func) { println!("  {line}"); }

    let mut results = vec![0.0];
    func.call(&[42.0], &mut results);
    let result = results[0];
    println!("\nResult from '{file_name}':");
    println!("  f(42.0) = {result:?}");

    Ok(())
}


//-------------------------------------------------------------------------------------------------

fn run_tests(dir_name: &str) -> Result<(), Box<dyn Error>> {
    let mut paths = vec![];
    let path = Path::new(dir_name);
    if path.is_dir() {
        find_tests(path, &mut paths)?;
        println!("Testing {} files in '{dir_name}' ...", paths.len());
    } else {
        if let Some(e) = path.extension()
            && e == "txt"
        {
            paths.push(path.to_path_buf());
        }
        if paths.is_empty() {
            return Err("No files.".into());
        }
        println!("Testing '{}' ...", path.display());
    }

    let min_width = paths.iter().map(|p| p.as_os_str().len()).max().unwrap_or(0);
    let width = ((min_width) / 4 + 2) * 4;

    let mut tests = 0;
    let mut fails = 0;
    for p in paths {
        tests += 1;
        print!("  {:width$}", p.display());
        std::io::stdout().flush()?;
        match run_test(&p) {
            Ok(()) => {
                println!("\x1b[1;32m\u{2713}\x1b[0m");
            }
            Err(e) => {
                println!("\x1b[1;31m\u{2718}\x1b[0m\n{e}");
                fails += 1;
            }
        }
    }

    if fails > 0 {
        println!("\ntest result: \x1b[31mFAILED\x1b[0m. {} passed; {fails} failed", tests - fails);
        Err("tests failed".into())
    } else {
        println!("\ntest result: \x1b[32mok\x1b[0m. {tests} passed; 0 failed");
        Ok(())
    }
}


fn find_tests(dir: &Path, paths: &mut Vec<PathBuf>) -> Result<(), Box<dyn Error>> {
    for entry in dir.read_dir()? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            find_tests(&path, paths)?;
        } else if let Some(e) = path.extension()
            && e == "txt"
        {
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
fn run_test(path: &Path) -> Result<(), Box<dyn Error>> {
    let expected = read_test_file(path)?;

    for key in ["source", "statements", "vir"] {
        let source = expected.get(key).unwrap_or(&vec![]).join("\n");
        test_lines(&path.to_string_lossy(), key, &source, &expected)?;
    }

    Ok(())
}


fn read_test_file(path: &Path) -> Result<HashMap<String, Vec<String>>, Box<dyn Error>> {
    let file =
        fs::File::open(path).map_err(|e| format!("couldn't open '{}': {}", path.display(), e))?;

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
) -> Result<(), Box<dyn Error>> {
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

    let (vir_stmts, vir_errors) = vir::run(&stmts);
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
        let vir_instrs = vir::instructions(&vir_stmts);
        let string_instrs = stmts_to_strings(&vir_instrs);
        compare_lines(&string_instrs, &expected["vir"], section, "vir")?;
    }

    // Once we get to the scheduling and assembler passes, we only do that for the source pass
    // (since we're already proving that the other passes all give the same output, and because
    // we can't feed the output of these passes back into the compiler), and we only do it if the
    // expected output is present (because the compiler is being implemented bit by bit and if
    // we run something NYI, we get an NYI and a panic.)
    if expected.contains_key("schedule") && section == "source" {
        let schedule = codegen::schedule(&vir_stmts);
        let schedule_strings = stmts_to_strings(&[schedule]);
        compare_lines(&schedule_strings, &expected["schedule"], section, "schedule")?;
    }

    if expected.contains_key("assembler") && section == "source" {
        let assembler = codegen::assemble(&vir_stmts);
        let assembler_strings = stmts_to_strings(&[assembler]);
        compare_lines(&assembler_strings, &expected["assembler"], section, "assembler")?;
    }

    if (expected.contains_key("disassembler") || expected.contains_key("results")) && section == "source" {
        let func = codegen::run(&vir_stmts);

        if expected.contains_key("disassembler") {
            compare_lines(&codegen::disassemble(&func), &expected["disassembler"], section, "disassembler")?;
        }

        if expected.contains_key("results") {
            test_results(&func, &expected["results"], section)?;
        }
    }

    Ok(())
}


fn test_results(func: &codegen::CompiledFn, expected: &[String], section: &str) -> Result<(), Box<dyn Error>> {
    let mut actual_strings = vec![];
    for line in expected {
        let Some((inputs_str, outputs_str)) = line.split_once("->") else {
            return Err(format!("invalid result line: {line:?}").into());
        };
        let inputs = inputs_str.split_whitespace()
            .map(str::parse::<f64>)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| format!("invalid input in {line:?}: {e}"))?;
        let expected_outputs = outputs_str.split_whitespace()
            .map(str::parse::<f64>)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| format!("invalid output in {line:?}: {e}"))?;

        let mut actual_outputs = vec![0.0f64; expected_outputs.len()];
        func.call(&inputs, &mut actual_outputs);

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
) -> Result<(), Box<dyn Error>> {
    if actual.len() != expected.len()
        || actual.iter().zip(expected).any(|(a, e)| a.trim() != e.trim())
    {
        Err(format!(
            "Mismatch between expected and obtained {output} from {input}.  Test output:\n\
            #( expected {output}\n\n  {}\n\n#)",
            actual.join("\n  ")).into())
    } else {
        Ok(())
    }
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


//-------------------------------------------------------------------------------------------------
