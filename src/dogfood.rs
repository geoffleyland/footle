use std::{
    collections::HashMap,
    fs,
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{bail, Context, Result};

use crate::{FOOTLE_FILE_EXTENSION, core, codegen};
use crate::core::Styleable;
use crate::ast;
use crate::vir;
use crate::runtime;


//-------------------------------------------------------------------------------------------------

struct Printer {
    tab:                    u16,
    width:                  u16,
    highlight:              bool,
    style:                  Option<core::SourceStyle>
}

impl Printer {
    fn new(tab: u16, width: u16, highlight: bool) -> Self {
        Self{tab, width, highlight, style: None}
    }

    fn file_name(&self) -> &str { self.style.as_ref().unwrap().file_name() }
    fn source(&self) -> &str { self.style.as_ref().unwrap().source() }
    fn style(&self) -> &core::SourceStyle { self.style.as_ref().unwrap() }
}


impl runtime::Observer for Printer {
    fn source_map(&mut self, map: Arc<core::SourceMap>) {
        self.style = Some(core::SourceStyle::new(self.tab, self.width, self.highlight, map));

        eprintln!("\nContents of '{}':", self.file_name());
        eprintln!("  {}", self.source().lines().collect::<Vec<_>>().join("\n  "));
    }
    fn stmts(&mut self, stmts: &[ast::Stmt]) {
        eprintln!("\nStatements from '{}':", self.file_name());
        for stmt in stmts { eprintln!("{}", stmt.styled(1, self.style())); }
    }
    fn vir(&mut self, vir: &vir::Block) {
        eprintln!("\nValue IR from '{}':", self.file_name());
        eprintln!("{}", vir.styled(1, self.style()));
    }
    fn schedule(&mut self, block: &codegen::scheduler::Block) {
        eprintln!("\nScheduled instructions from '{}':", self.file_name());
        eprintln!("{}", block.styled(1, self.style()));
    }
    fn assembly(&mut self, block: &codegen::assembler::Block) {
        eprintln!("\nAssembly from '{}':", self.file_name());
        eprintln!("{}", block.styled(1, self.style()));
    }
    fn func(&mut self, func: &codegen::CompiledFn) {
        eprintln!("\nDisassembly from '{}':", self.file_name());
        for line in codegen::disassemble(func) { eprintln!("  {line}"); }
    }
}


/// Compile and run a single file noisily
///
/// Read in the file specified, process it, and tell everyone about it.
pub fn run_file_verbose(file_path: &PathBuf, arguments: &[runtime::Value]) -> Result<()> {
    let file_name = file_path.display().to_string();
    let source =
        fs::read_to_string(file_path)
            .with_context(|| format!("couldn't read '{file_name}'"))?;

    let mut printer = Printer::new(2, 40, true);

    let mut block = runtime::load_observed(&file_name, source, &mut printer)?;
    let results = block.call_observed(arguments, &mut printer)?;

    println!("\nResult from '{file_name}':");
    println!("  f({}) = ({})",
        arguments.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(", "),
        results.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(", "));

    Ok(())
}


//-------------------------------------------------------------------------------------------------

pub fn run_tests(dir_path: &PathBuf) -> Result<()> {
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


struct DogfoodEater {
    expected:               HashMap<String, Vec<String>>,
    section:                String,
    mismatches:             Vec<String>,
}

impl DogfoodEater {
    fn new(expected: HashMap<String, Vec<String>>) -> Self {
        Self{expected, section: String::new(), mismatches: vec![] }
    }

    fn test(&mut self, key: &str, extra_passes: &[&str], required: bool, lines: &[String]) {
        let expected = match self.expected.get(key) {
            Some(expected) => Some(expected.as_slice()),
            None if required => Some(&[][..]),
            None => None,
        };
        if (self.section == "source" || (extra_passes.contains(&self.section.as_str()))) &&
            let Some(expected) = expected &&
            let Err(e) = compare_lines(lines, expected, &self.section, key) {
            self.mismatches.push(format!("{e:#}"));
        }
        self.expected.insert(key.into(), lines.to_vec());
    }

    fn close(&self) -> Result<()> {
        if self.mismatches.is_empty() { Ok(()) } else {
            bail!(self.mismatches.join("\n"));
        }
    }
}

impl runtime::Observer for DogfoodEater {
    fn stmts(&mut self, stmts: &[ast::Stmt]) {
        self.test("statements", &["statements"], false, &stmts_to_strings(stmts));
    }
    fn vir(&mut self, vir: &vir::Block) {
        self.test("vir", &["statements", "vir"], false, &block_to_strings(vir));
    }
    fn schedule(&mut self, block: &codegen::scheduler::Block) {
        self.test("schedule", &[], true, &block_to_strings(block));
    }
    fn assembly(&mut self, block: &codegen::assembler::Block) {
        let assembly = block_to_strings(block);
        self.test("assembly", &[], true, &assembly);
        self.expected.insert("disassembly".into(), assembly);
    }
    fn func(&mut self, func: &codegen::CompiledFn) {
        let disassembly = codegen::disassemble(func);
        let expected_disassembly = &self.expected["disassembly"][0..disassembly.len()];
        if let Err(e) = compare_lines(&disassembly, expected_disassembly, &self.section, "disassembly") {
            self.mismatches.push(format!("{e:#}"));
        }
    }
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
///  * assembly- inside #( expected assembly ... #) - assembler output - maybe one day readable
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
    let mut eater = DogfoodEater::new(expected);

    for key in ["source", "statements", "vir"] {
        eater.section = key.into();
        let stop = if let Some(source) = eater.expected.get(key) {
            test_lines(&path.to_string_lossy(), key, &source.join("\n"), &mut eater)?
        } else { false };
        eater.close()?;
        if stop { break }
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
    file_name:              &str,
    section:                &str,
    source:                 &str,
    eater:                  &mut DogfoodEater,
) -> Result<bool> {
    let mut block = match runtime::load_observed(file_name, source.into(), eater) {
        Err(diagnostics) => {
            let error_strings: Vec<_> = diagnostics.errors.iter().map(|e| format!("{e}")).collect();
            compare_lines(&error_strings, eater.expected.get("errors").unwrap_or(&vec![]), section, "errors")?;
            if eater.expected.contains_key("errors") { return Ok(true); }
            unreachable!();
        }
        Ok(block) => {
            if eater.expected.contains_key("errors") { bail!("did not get expected errors") }
            block
        },
    };

    // Once we get to the scheduling and assembler passes, we only do that for the source pass
    // (since we're already proving that the other passes all give the same output, and because
    // we can't feed the output of these passes back into the compiler), and we only do it if the
    // expected output is present (because the compiler is being implemented bit by bit and if
    // we run something NYI, we get an NYI and a panic.)
    if section == "source" && eater.expected.contains_key("results") {
        let all_arguments = parse_expected_results(&eater.expected["results"])?;
        let mut obtained_lines = vec![];
        for arguments in all_arguments {
            let results = block.call_observed(&arguments, eater)?;

            obtained_lines.push(format!("{} -> {}",
                arguments.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(" "),
                results.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(" ")));
        }
        if let Err(e) = compare_lines(&obtained_lines, &eater.expected["results"], section, "results") {
            eater.mismatches.push(format!("{e:#}"));
        }
        eater.close()?;
    }
    Ok(false)
}


fn parse_expected_results(expected: &[String]) -> Result<Vec<Vec<runtime::Value>>> {
    let mut all_arguments = vec![];
    for line in expected {
        let Some((arguments_str, _)) = line.split_once("->") else {
            bail!("    invalid result line: {line:?}");
        };
        let arguments = arguments_str.split_whitespace()
            .map(str::parse::<runtime::Value>)
            .collect::<Result<Vec<_>, _>>()
            .with_context(|| format!("    invalid argument in {line:?}"))?;
        all_arguments.push(arguments);
    }
    Ok(all_arguments)
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


//-----------------------------------------------fo--------------------------------------------------
