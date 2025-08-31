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
/// Read in the file specified, and parse it!
fn run_file(file_name: &str) -> Result<(), Box<dyn Error>> {
    eprintln!("Opening '{file_name}'");
    let source =
        fs::read_to_string(file_name).map_err(|e| format!("couldn't open '{file_name}': {e}"))?;
    println!("Contents of '{file_name}':\n  {}", source.lines().collect::<Vec<_>>().join("\n  "));

    let (stmts, errors, source_map) = ast::parse(file_name, source.as_str());

    if errors.is_empty() {
        println!("\nStatements from '{file_name}':");
        for stmt in &stmts {
            println!("{stmt}");
        }
    } else {
        println!("\nErrors from '{file_name}':");
        for e in errors {
            print!("{}", e.show_in_source(&source_map));
        }
    }

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
///  * errors - inside an #( expected errors .... #) block comment - expected
///    errors, if any
///  * statements - inside #( expected statements ... #) - expected
///    pretty-printed statements
///
/// The clever thing is that all the excess stuff is block comments, so the
/// files are still legitimate programs.
/// First, parse the code.  If there are errors, check they match the expected
/// errors. If there are no errors (and that's what we wanted), check the
/// statements match expectations. If that worked, take the output from the
/// parser (actually, the expected output, but we already checked they're the
/// same), and run it back through the parser, checking that we get the
/// same result as before.
fn run_test(path: &Path) -> Result<(), Box<dyn Error>> {
    let expected = read_test_file(path)?;

    for key in ["source", "statements"] {
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
        } else if !line.is_empty() && (mode == "source" || !line.starts_with("#)")) {
            expected.entry(mode.clone()).or_default().push(line);
        }
    }
    Ok(expected)
}


fn test_lines(
    file_name: &str,
    input: &str,
    source: &str,
    expected: &HashMap<String, Vec<String>>,
) -> Result<(), Box<dyn Error>> {
    let (stmts, errors, _) = ast::parse(file_name, source);
    let error_strings: Vec<_> = errors.iter().map(|e| format!("{e}")).collect();

    let mut checking = input == "source";
    if checking {
        compare_lines(&error_strings, expected.get("errors").unwrap_or(&vec![]), input, "errors")?;
        if expected.contains_key("errors") {
            return Ok(());
        }
    }

    checking |= input == "statements";
    if checking && expected.contains_key("statements") {
        let string_statements = statements_to_strings(&stmts);
        compare_lines(&string_statements, &expected["statements"], input, "statements")?;
    }

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


fn statements_to_strings(stmts: &[ast::Stmt]) -> Vec<String> {
    let style = core::IndentedStyle::new(2);
    stmts
        .iter()
        .map(|stmt| format!("{}", stmt.styled(1, &style)))
        .collect::<Vec<String>>()
        .join("\n")
        .split('\n')
        .map(std::string::ToString::to_string)
        .filter(|s| !s.is_empty())
        .collect()
}


//-------------------------------------------------------------------------------------------------
