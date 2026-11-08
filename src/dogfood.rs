use std::{
    collections::BTreeMap,
    fmt::Write as fmtWrite,
    fs,
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{bail, Context, Result};
use similar::{TextDiff, ChangeTag};

use crate::{FOOTLE_FILE_EXTENSION, core};
use crate::codegen::{self, assembler, scheduler, Signature, Pool};
use crate::core::{Styleable, join_format};
use crate::ast;
use crate::vir::{self, TypeInfo};
use crate::runtime;


//-------------------------------------------------------------------------------------------------

pub struct Printer {
    tab:                    u16,
    width:                  u16,
    highlight:              bool,
    style:                  Option<core::SourceStyle>
}

impl Printer {
    pub fn new(tab: u16, width: u16, highlight: bool) -> Self {
        Self{tab, width, highlight, style: None}
    }

    fn style(&self) -> &core::SourceStyle { self.style.as_ref().unwrap() }
    fn file_name(&self) -> &str { self.style().file_name() }
}


impl runtime::Observer for Printer {
    fn source_map(&mut self, map: Arc<core::SourceMap>) {
        self.style = Some(core::SourceStyle::new(self.tab, self.width, self.highlight, map));

        eprintln!("\nContents of '{}':", self.file_name());
        eprintln!("  {}", self.style().source().lines().collect::<Vec<_>>().join("\n  "));
    }
    fn stmts(&mut self, stmts: &[ast::Stmt]) {
        eprintln!("\nStatements from '{}':", self.file_name());
        for stmt in stmts { eprintln!("{}", stmt.styled(1, self.style())); }
    }
    fn vir(&mut self, vir: &vir::Block) {
        eprintln!("\nValue IR from '{}':", self.file_name());
        eprintln!("{}", vir.styled(1, self.style()));
    }
    fn schedule(&mut self, block: &scheduler::Block, signature: &Signature, pool: &Pool) {
        eprintln!("\nScheduled instructions from '{}':", self.file_name());
        eprintln!("{}", (block, signature, pool).styled(1, self.style()));
    }
    fn assembly(&mut self, block: &assembler::Block, pool: &Pool) {
        eprintln!("\nAssembly from '{}':", self.file_name());
        eprintln!("{}", (block, pool).styled(1, self.style()));
    }
    fn func(&mut self, func: &codegen::CompiledFn) {
        eprintln!("\nDisassembly from '{}':", self.file_name());
        for line in codegen::disassemble(func) { eprintln!("  {line}"); }
    }
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
    expected:               BTreeMap<Section, Vec<String>>,
    unchecked:              Vec<Section>,
    input:                  Section,
    had_mismatches:         bool,
    safe_to_run:            bool,
    signature:              Vec<TypeInfo>,
    assembly:               Vec<String>,
}

impl DogfoodEater {
    fn new(expected: BTreeMap<Section, Vec<String>>, unchecked: Vec<Section>) -> Self {
        Self{ expected, unchecked, input: Section::Source, had_mismatches: false,
            safe_to_run: true, signature: vec![], assembly: vec![] }
    }

    fn test(&mut self, section: &Section, extra_inputs: &[Section], required: bool, lines: &[String]) {
        let expected = match self.expected.get(section) {
            Some(expected)      => Some(expected.as_slice()),
            None if required    => Some(&[][..]),
            None => None,
        };
        if (self.input == Section::Source || (extra_inputs.contains(&self.input))) &&
            let Some(expected) = expected {
            self.unchecked.retain(|e| e != section);
            if let Err(e) = compare_lines(lines, expected, &self.input, section) {
                eprintln!("\n{e:#}");
                self.had_mismatches = true;
            }
        }
        self.expected.insert(section.clone(), lines.to_vec());
    }

    fn close(&self) -> Result<()> {
        if self.had_mismatches { bail!("")}
        Ok(())
    }

    fn schedule_section()       -> Section { Section::Schedule(Arch::HOST) }
    fn assembly_section(&self)  -> Section { Section::Assembly(Arch::HOST, self.signature.clone()) }
}

impl runtime::Observer for DogfoodEater {
    fn stmts(&mut self, stmts: &[ast::Stmt]) {
        self.test(&Section::Statements, &[Section::Statements], false,
            &stmts.iter().flat_map(block_to_strings).collect::<Vec<_>>());
    }
    fn vir(&mut self, vir: &vir::Block) {
        self.test(&Section::Vir, &[Section::Statements, Section::Vir], false, &block_to_strings(vir));
    }
    fn signature(&mut self, signature: &[vir::TypeInfo]) {
        self.signature = signature.to_vec();
    }
    fn schedule(&mut self, block: &scheduler::Block, signature: &Signature, pool: &Pool) {
        self.test(&Self::schedule_section(), &[], true, &block_to_strings(&(block, signature, pool)));
    }
    fn assembly(&mut self, block: &assembler::Block, pool: &Pool) {
        let assembly = block_to_strings(&(block, pool));
        self.test(&self.assembly_section(), &[], true, &assembly);
        self.assembly = assembly;
    }
    fn func(&mut self, func: &codegen::CompiledFn) {
        let disassembly = codegen::disassemble(func);
        let expected_disassembly = &self.assembly[0..func.instr_words()];
        let check = compare_lines(&disassembly, expected_disassembly, &self.input, "disassembly");
        self.safe_to_run = check.is_ok();
        if let Err(e) = check {
            eprintln!("\n{e:#}");
            self.had_mismatches = true;
        }
    }
    fn safe_to_run(&self) -> bool { self.safe_to_run }
}


/// Run a test file.
///
/// A magical, own-dogfood eating tester.
/// Read the code, which is structured as the source of the program followed by expected
/// pretty-printed output from each compiler pass, each inside #( expected <section> ... #) block
/// comments.  The output of each pass is checked against the corresponding expectation.
/// The sections are:
///  * errors - if there's any errors we expect, otherwise:
///  * statements - expected pretty-printed statements, which should be re-readable as footle source;
///  * vir - "value intermediate representation", an SSA form, readable as footle source;
///  * schedule <arch> - expected scheduler output - no longer written as footle source, though;
///  * assembly <arch> <argument types...> - assembler output - in the same format as the
///    disassembler, so we check the generated assembly against what we expect AND what we get from
///    disassembling the final binary;
///  * results - we call the code with the provided arguments and check the results match the
///    expected output
///
/// The clever thing is that all the excess stuff is block comments, so the files are still
/// legitimate footle programs.
///
/// The first two passes (statements and vir) pretty-print to valid footle, so output from these
/// passes is fed back into the front of the compiler, and we check that we get the same output
/// the second (and third!) time around.
///
/// The remaining codegen passes are only checked once, though, on the first pass - if running the
/// compiler with the source, statements and vir as inputs all produce the same vir, then what
/// follows should be the same in all cases.
fn run_test(path: &Path) -> Result<()> {
    let (expected, sections) = read_test_file(path)?;
    let mut eater = DogfoodEater::new(expected, sections);

    for input in [Section::Source, Section::Statements, Section::Vir] {
        eater.input = input.clone();
        let stop = if let Some(source) = eater.expected.get(&input) {
            test_lines(&path.to_string_lossy(), &input, &source.join("\n"), &mut eater)?
        } else { false };
        eater.close()?;
        if stop { break }
    }
    if !eater.unchecked.is_empty() {
        bail!("unchecked sections: {}", join_format(&eater.unchecked, ", "));
    }
    Ok(())
}


#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, derive_more::Display, derive_more::FromStr)]
enum Arch {
    #[display("aarch64")] AArch64,
    #[display("riscv64")] RiscV64
}

impl Arch {
    #[cfg(target_arch = "aarch64")] const HOST: Self = Self::AArch64;
    #[cfg(target_arch = "riscv64")] const HOST: Self = Self::RiscV64;
}


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Section { Source, Errors, Statements, Vir, Schedule(Arch), Assembly(Arch, Vec<TypeInfo>), Results}

impl Section {
    fn is_for_host(&self) -> bool {
        use Section::*;
        match self {
            Source | Errors | Statements | Vir | Results    => true,
            Schedule(arch) | Assembly(arch, ..)             => *arch == Arch::HOST
        }
    }
}

impl std::str::FromStr for Section {
    type Err = anyhow::Error;
    fn from_str(header: &str) -> anyhow::Result<Self> {
        let mut words = header.split_whitespace();
        match words.next().context("missing expected output section name")?.to_lowercase().as_str() {
            "errors"                    => Ok(Self::Errors),
            "statements"                => Ok(Self::Statements),
            "vir"                       => Ok(Self::Vir),
            "results"                   => Ok(Self::Results),
            "schedule" => {
                Ok(Self::Schedule(words.next()
                    .context("missing expected output section architecture")?
                    .parse::<Arch>()?))
            }
            "assembly" => {
                let arch = words.next()
                    .context("missing expected output section architecture")?
                    .parse::<Arch>()?;
                let signature = words.map(str::parse).collect::<Result<Vec<TypeInfo>, _>>()?;
                Ok(Self::Assembly(arch, signature))
            }
            _                           => bail!("unknown expected output section: '{header}'")
        }
    }
}

impl std::fmt::Display for Section {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Source                => write!(f, "source"),
            Self::Errors                => write!(f, "errors"),
            Self::Statements            => write!(f, "statements"),
            Self::Vir                   => write!(f, "vir"),
            Self::Results               => write!(f, "results"),
            Self::Schedule(arch)        => write!(f, "schedule {arch}"),
            Self::Assembly(arch, signature) => {
                write!(f, "assembly {arch}")?;
                for s in signature { write!(f, " {s}")?; }
                Ok(())
            }
        }
    }
}


#[allow(clippy::type_complexity)]
fn read_test_file(path: &Path) -> Result<(BTreeMap<Section, Vec<String>>, Vec<Section>)> {
    let file =
        fs::File::open(path)
            .with_context(|| format!("couldn't open '{}'", path.display()))?;

    // Read the file, putting all the bits into the right buffers.
    let mut expected = BTreeMap::<Section, Vec<String>>::new();
    let mut sections = vec![];
    let mut reading = Some(Section::Source);

    for line in BufReader::new(file).lines().map_while(Result::ok) {
        if let Some(header) = line.strip_prefix("#( expected") {
            let section = header.parse::<Section>()?;
            if expected.insert(section.clone(), vec![]).is_some() {
                bail!("Repeated expected section '{section}'");
            }
            if section.is_for_host() { sections.push(section.clone()) }
            reading = Some(section);
        } else if line.starts_with("#)") && reading != Some(Section::Source) {
            reading = None;
        } else if reading == Some(Section::Source) {
            expected.entry(Section::Source).or_default().push(line);
        } else if let Some(section) = &reading && !line.is_empty() {
            let line = line.strip_prefix("  ").unwrap_or(&line);
            expected.entry(section.clone()).or_default().push(line.to_string());
        } else if reading.is_none() && !line.is_empty() {
            bail!("text betwen expected sections: '{line}'")
        }
    }
    Ok((expected, sections))
}


fn test_lines(
    file_name:              &str,
    input:                  &Section,
    source:                 &str,
    eater:                  &mut DogfoodEater,
) -> Result<bool> {
    let mut block = match runtime::load(file_name, source.into(), eater) {
        Err(diagnostics) => {
            let error_strings: Vec<_> = diagnostics.errors.iter().map(|e| format!("{e}")).collect();
            eater.unchecked.retain(|e| *e != Section::Errors);
            compare_lines(&error_strings, eater.expected.get(&Section::Errors).unwrap_or(&vec![]), input, "errors")?;
            return Ok(true);
        }
        Ok(block) => {
            if eater.expected.contains_key(&Section::Errors) { bail!("did not get expected errors") }
            block
        },
    };

    // Once we get to codegen and calling (with block.call), we only run for the original source
    // (since we've already proven that the other passes all give the same output, and because we
    // can't feed the output of the codegen passes back into the compiler), and we only do it if
    // the expected results are present (because the compiler is being implemented bit by bit and
    // if we run something NYI, we get an NYI and a panic.)
    if *input == Section::Source && eater.expected.contains_key(&Section::Results) {
        let all_args = parse_expected_results(&eater.expected[&Section::Results])?;
        eater.unchecked.retain(|e| *e != Section::Results);
        let mut obtained_lines = vec![];
        for args in all_args {
            let outcome = match block.call(&args, eater) {
                Ok(results)     => join_format(&results, " "),
                Err(e) => {
                    let error_text = e.downcast_ref::<runtime::Diagnostics>().map_or_else(
                        || format!("{e:#}"),
                        |d| join_format(&d.errors, "; "));
                    format!("error: {error_text}")
                }
            };

            let line = format!("{} -> {outcome}", join_format(&args, " "));
            // Trim the start of the line - If there are no arguments, start at the "->"
            obtained_lines.push(line.trim_start().to_string());
        }
        if let Err(e) = compare_lines(&obtained_lines, &eater.expected[&Section::Results], input, "results") {
            eprintln!("\n{e:#}");
            eater.had_mismatches = true;
        }
    }
    Ok(false)
}


fn parse_expected_results(expected: &[String]) -> Result<Vec<Vec<runtime::Value>>> {
    let mut all_args = vec![];
    for line in expected {
        let Some((args_str, _)) = line.split_once("->") else {
            bail!("    invalid result line: {line:?}");
        };
        let args = args_str.split_whitespace()
            .map(str::parse::<runtime::Value>)
            .collect::<Result<Vec<_>, _>>()
            .with_context(|| format!("    invalid argument in {line:?}"))?;
        all_args.push(args);
    }
    Ok(all_args)
}


fn compare_lines(
    obtained:           &[String],
    expected:           &[String],
    input:              &Section,
    output:             impl std::fmt::Display) -> Result<()> {
    if obtained != expected {
        let diff_text = format_diff(expected, obtained);
        bail!(
            "Mismatch between expected and obtained {output} from {input}:\n\n\
            {diff_text}\nTest output:\n#( expected {output}\n\n  {}\n\n#)",
            obtained.join("\n  "))
    }
    Ok(())
}


/// Colour the differences between two lists of lines, with a little context.
///
/// Labels aren't part of the alignment, or inserting one instruction would mark every later line
/// as changed.
fn format_diff(expected: &[String], obtained: &[String]) -> String {
    let (expected_labels, expected_bodies): (Vec<_>, Vec<_>) =
        expected.iter().map(|line| split_label(line)).unzip();
    let (obtained_labels, obtained_bodies): (Vec<_>, Vec<_>) =
        obtained.iter().map(|line| split_label(line)).unzip();

    let diff = TextDiff::from_slices(&expected_bodies, &obtained_bodies);
    let mut text = String::new();
    for group in diff.grouped_ops(2) {
        if !text.is_empty() { text += "\x1b[2m  ...\x1b[0m\n"; }
        for op in group {
            for change in diff.iter_inline_changes(&op) {
                let (sign, colour) = match change.tag() {
                    ChangeTag::Delete   => ('-', "31"),
                    ChangeTag::Insert   => ('+', "32"),
                    ChangeTag::Equal    => (' ', "2"),
                };
                let label = change.new_index().map(|i| obtained_labels[i])
                    .or_else(|| change.old_index().map(|i| expected_labels[i]))
                    .unwrap_or_default();
                let _ = write!(text, "\x1b[{colour}m{sign} {label}");
                for (emphasized, part) in change.iter_strings_lossy() {
                    let style = if emphasized { "0;7" } else { "0" };
                    let _ = write!(text, "\x1b[{style};{colour}m{part}");
                }
                text += "\x1b[0m\n";
            }
        }
    }
    text
}


/// Split `0x1000: ldr x9, #0x1038` or `I3: ret I2` into its label (with the `: `) and the rest.
fn split_label(line: &str) -> (&str, &str) {
    match line.split_once(": ") {
        Some((label, _)) if label.starts_with("0x")
            || label.strip_prefix(['I', 'K']).is_some_and(|number| number.parse::<u32>().is_ok())
                => line.split_at(label.len() + ": ".len()),
        _       => ("", line),
    }
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
