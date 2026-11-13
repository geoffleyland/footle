use std::{
    collections::{BTreeSet, BTreeMap},
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

pub fn run_tests(path: &PathBuf, do_bless: bool) -> Result<()> {
    let mut paths = vec![];
    if path.is_dir() {
        find_tests(path, &mut paths)?;
        println!("Testing {} files in '{}' ...", paths.len(), path.display());
    } else {
        if let Some(e) = path.extension() && e == FOOTLE_FILE_EXTENSION {
            paths.push(path.clone());
        }
        if paths.is_empty() {
            bail!("No files.");
        }
        println!("Testing '{}' ...", path.display());
    }

    let max_filename_width = paths.iter().map(|p|
        p.strip_prefix(path).unwrap_or(p).as_os_str().len()).max().unwrap_or(0);
    let filename_width = (max_filename_width / 4 + 1) * 4;

    let mut tests = 0;
    let mut fails = 0;
    let mut bless_ok = true;
    let mut blessable = vec![];
    for p in paths {
        tests += 1;
        print!("  {:filename_width$}", p.strip_prefix(path).unwrap_or(&p).display());
        std::io::stdout().flush()?;
        match run_test(&p) {
            Ok((Outcome::Pass, _)) => {
                println!("\x1b[1;32m\u{2713}\x1b[0m");
            }
            Ok((Outcome::Blessable, messages)) => {
                println!("\x1b[1;33m\u{2248}\x1b[0m");
                for m in messages { println!("{m}"); }
                fails += 1;
                blessable.push(p);
            }
            Ok((Outcome::Fail, messages)) => {
                println!("\x1b[1;31m\u{2718}\x1b[0m");
                for m in messages { println!("{m}"); }
                fails += 1;
                bless_ok = false;
            }
            Err(e) => {
                println!("\x1b[1;31m\u{2718}\x1b[0m");
                eprintln!("{e:#}");
                fails += 1;
                bless_ok = false;
            }
        }
    }


    if fails > 0 {
        println!("\ntest result: \x1b[31mFAILED\x1b[0m. {} passed; {fails} failed", tests - fails);
    } else {
        println!("\ntest result: \x1b[32mok\x1b[0m. {tests} passed; 0 failed");
    }

    if do_bless && bless_ok && !blessable.is_empty() {
        println!("blessing {} file(s)", blessable.len());
        for path in blessable {
            fs::rename(path.with_added_extension("bless"), path)?;
        }
        return Ok(())
    }

    match (do_bless, bless_ok, blessable.is_empty()) {
        (true, false, _) => {
            println!("not blessing: {} failure(s) (\x1b[1;31m\u{2718}\x1b[0m) can't be blessed",
                fails - blessable.len());
        }
        (true,  _, true) => {
            println!("nothing to bless");
        }
        (false, true, false) => {
            println!("{fails} file(s) (\x1b[1;33m\u{2248}\x1b[0m) can be blessed: run `./admin bless`");
        }
        _ => {}
    }

    if fails > 0 { bail!("tests failed") }
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


type Sections = BTreeMap<Section, Vec<String>>;

#[derive(Default)]
struct DogfoodEater {
    obtained:               BTreeMap<Section, Sections>,
    input:                  Section,
    signature:              Vec<TypeInfo>,
}

impl DogfoodEater {
    fn obtained_lines(&self, input: &Section, section: &Section) -> &[String] {
        self.obtained.get(input).and_then(|sections| sections.get(section)).map_or(&[], Vec::as_slice)
    }

    fn insert_obtained(&mut self, section: &Section, lines: impl IntoIterator<Item = String>) {
        self.obtained.entry(self.input.clone()).or_default()
            .insert(section.clone(), lines.into_iter().collect());
    }

    fn schedule_section()           -> Section { Section::Schedule(Arch::HOST) }
    fn assembly_section(&self)      -> Section { Section::Assembly(Arch::HOST, self.signature.clone()) }
    fn disassembly_section(&self)   -> Section { Section::Disassembly(Arch::HOST, self.signature.clone()) }
}

impl runtime::Observer for DogfoodEater {
    fn stmts(&mut self, stmts: &[ast::Stmt]) {
        self.insert_obtained(&Section::Statements, stmts.iter().flat_map(to_lines));
    }
    fn vir(&mut self, vir: &vir::Block) {
        self.insert_obtained(&Section::Vir, to_lines(vir));
    }
    fn signature(&mut self, signature: &[vir::TypeInfo]) {
        self.signature = signature.to_vec();
    }
    fn schedule(&mut self, block: &scheduler::Block, signature: &Signature, pool: &Pool) {
        self.insert_obtained(&Self::schedule_section(), to_lines(&(block, signature, pool)));
    }
    fn assembly(&mut self, block: &assembler::Block, pool: &Pool) {
        self.insert_obtained(&self.assembly_section(), to_lines(&(block, pool)));
    }
    fn func(&mut self, func: &codegen::CompiledFn) {
        let mut disassembly = codegen::disassemble(func);
        disassembly.resize(func.instr_words(), "(not decoded)".to_string());
        self.insert_obtained(&self.disassembly_section(), disassembly);
    }
    fn safe_to_run(&self) -> bool {
        let sections = &self.obtained[&Section::Source];
        sections[&self.assembly_section()].starts_with(&sections[&self.disassembly_section()])
    }
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
fn run_test(path: &Path) -> Result<(Outcome, Vec<String>)> {
    let mut expected = read_test_file(path)?;
    expected.entry(Section::Errors).or_default();
    let mut eater = DogfoodEater::default();

    for input_section in [Section::Source, Section::Statements, Section::Vir] {
        let input_text = match input_section {
            Section::Source => expected.get(&input_section),
            _               => eater.obtained.get(&Section::Source)
                                .and_then(|sections| sections.get(&input_section)),
        };
        let Some(input_text) = input_text.map(|lines| lines.join("\n")) else { continue };
        eater.input = input_section.clone();
        compile_input(&path.to_string_lossy(), &input_text, &mut eater, &expected)?;

        if !eater.obtained_lines(&input_section, &Section::Errors).is_empty() { break }
    }

    copy_assembly_to_expected_disassembly(&eater, &mut expected);
    Ok(match report_mismatches(&eater, &expected) {
        (Outcome::Blessable, _) if path.extension().is_some_and(|e| e == "bless") => (Outcome::Fail, vec![]),
        (Outcome::Blessable, messages) => {
            let mut sections = expected;
            sections.extend(eater.obtained.remove(&Section::Source).unwrap_or_default());
            write_whole_file(path, sections)?;
            match run_test(&path.with_added_extension("bless")) {
                Ok((Outcome::Pass, _))  => (Outcome::Blessable, messages),
                _                       => (Outcome::Fail, messages)
            }
        }
        other                           => other,
    })
}


fn copy_assembly_to_expected_disassembly(eater: &DogfoodEater, expected: &mut Sections) {
    if let Some(sections) = eater.obtained.get(&Section::Source) {
        for (section, disassembly) in sections {
            if let Section::Disassembly(arch, signature) = section {
                let listing = &sections[&Section::Assembly(*arch, signature.clone())];
                expected.insert(section.clone(), listing[..disassembly.len()].to_vec());
            }
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Outcome { Pass, Blessable, Fail }

fn report_mismatches(eater: &DogfoodEater, expected: &Sections) -> (Outcome, Vec<String>) {
    let source_output = eater.obtained.get(&Section::Source);
    let compile_failed = !eater.obtained_lines(&Section::Source, &Section::Errors).is_empty();
    let sections_to_compare: BTreeSet<&Section> = if compile_failed {
        BTreeSet::from([&Section::Errors])
    } else {
        expected.keys()
            .chain(source_output.into_iter()
                .flat_map(|sections| sections.keys())
                .filter(|s| s.is_compulsory()))
            .collect()
    };

    let mut outcome = Outcome::Pass;
    let mut messages = vec![];
    for (input, obtained_sections) in &eater.obtained {
        let expected_sections = if *input == Section::Source { expected } else { source_output.unwrap() };
        for section in &sections_to_compare {
            let Some(reference) = compare_with(input, section) else { continue };
            let obtained_lines = obtained_sections.get(section).map_or(&[][..], Vec::as_slice);
            let expected_lines = expected_sections.get(&reference).map_or(&[][..], Vec::as_slice);
            if let Err(e) = compare_lines(obtained_lines, expected_lines, input, section) {
                messages.push(format!("\n{e:#}"));
                outcome = outcome.max(section.mismatch_outcome(expected_lines, obtained_lines));
            }
        }
    }
    (outcome, messages)
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


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
enum Section { #[default] Source, Statements, Vir, Errors, Schedule(Arch),
    Assembly(Arch, Vec<TypeInfo>), Disassembly(Arch, Vec<TypeInfo>), Results}

impl Section {
    fn is_for_host(&self) -> bool {
        use Section::*;
        match self {
            Source | Errors | Statements | Vir | Results    => true,
            Schedule(arch) | Assembly(arch, ..) |
                Disassembly(arch, ..)                       => *arch == Arch::HOST
        }
    }
    fn is_compulsory(&self) -> bool {
        matches!(self, Self::Schedule(..) | Self::Assembly(..))
    }
    fn mismatch_outcome(&self, expected: &[String], obtained: &[String]) -> Outcome {
        match self {
            Self::Results | Self::Disassembly(..)               => Outcome::Fail,
            Self::Errors if expected.is_empty() || obtained.is_empty()
                                                                => Outcome::Fail,
            _                                                   => Outcome::Blessable,
        }
    }
}

fn compare_with(input: &Section, section: &Section) -> Option<Section> {
    use Section::*;
    match (input, section) {
        (_, _) if !section.is_for_host()                        => None,
        (Source, Source) | (Vir, Statements)                    => None,
        // FIXME: This is the correct line for Vir once we've fixed up the statement writer
        // (Vir, Statements)                                       => Some(Vir),
        (Source, _) |
            (Statements, Statements | Vir | Errors) |
            (Vir, Vir | Errors)                                 => Some(section.clone()),
        _                                                       => None,
    }
}


impl std::str::FromStr for Section {
    type Err = anyhow::Error;
    fn from_str(header: &str) -> anyhow::Result<Self> {
        let header = header.trim();
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
            Self::Disassembly(arch, signature) => {
                write!(f, "disassembly {arch}")?;
                for s in signature { write!(f, " {s}")?; }
                Ok(())
            }
        }
    }
}


fn read_test_file(path: &Path) -> Result<Sections> {
    let file =
        fs::File::open(path)
            .with_context(|| format!("couldn't open '{}'", path.display()))?;

    // Read the file, putting all the bits into the right buffers.
    let mut expected = BTreeMap::<Section, Vec<String>>::new();
    let mut reading = Some(Section::Source);

    for line in BufReader::new(file).lines().map_while(Result::ok) {
        if let Some(header) = line.strip_prefix("#( expected") {
            let section = header.parse::<Section>()?;
            if expected.insert(section.clone(), vec![]).is_some() {
                bail!("Repeated expected section '{section}'");
            }
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
    Ok(expected)
}


fn compile_input(
    file_name:              &str,
    source:                 &str,
    eater:                  &mut DogfoodEater,
    expected:               &Sections,
) -> Result<()> {
    let Ok(mut block) = runtime::load(file_name, source.into(), eater)
        .inspect_err(|diagnostics| eater.insert_obtained(&Section::Errors, diagnostics.errors.iter().map(ToString::to_string)))
    else { return Ok(()) };

    // Once we get to codegen and calling (with block.call), we only run for the original source
    // (since we've already proven that the other passes all give the same output, and because we
    // can't feed the output of the codegen passes back into the compiler), and we only do it if
    // the expected results are present (because the compiler is being implemented bit by bit and
    // if we run something NYI, we get an NYI and a panic.)
    if eater.input == Section::Source && expected.contains_key(&Section::Results) {
        let all_args = parse_arguments(&expected[&Section::Results])?;
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
        eater.insert_obtained(&Section::Results, obtained_lines);
    }
    Ok(())
}


fn parse_arguments(expected: &[String]) -> Result<Vec<Vec<runtime::Value>>> {
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
    section:            impl std::fmt::Display) -> Result<()> {
    if obtained != expected {
        let diff_text = format_diff(expected, obtained);
        bail!(
            "Mismatch between expected and obtained {section} from {input}:\n\n\
            {diff_text}\n")
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
fn to_lines<S: core::Styleable>(block: &S) -> Vec<String> {
    let style = core::IndentedStyle::new(2);
    format!("{}", block.styled(0, &style))
        .lines()
        .filter(|l| !l.is_empty())
        .map(str::to_string)
        .collect::<Vec<_>>()
}


//-------------------------------------------------------------------------------------------------

fn write_whole_file(path: &Path, sections: BTreeMap<Section, Vec<String>>) -> Result<()> {
    let temp_path = path.with_added_extension("bless");
    let mut f = fs::File::create(&temp_path)?;

    let mut was_expected = false;

    for (section, lines) in sections {
        if section == Section::Source {
            for l in &lines { writeln!(f, "{l}")?; }
        } else if !matches!(section, Section::Disassembly(..)) && !lines.is_empty() {
            if was_expected { writeln!(f)?; }
            write!(f, "#( expected {section}\n\n")?;
            for l in lines { writeln!(f, "  {l}")?; }
            writeln!(f, "\n#)")?;
            was_expected = true;
        }
    }
    Ok(())
}


//-------------------------------------------------------------------------------------------------
