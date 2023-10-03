mod parser;

use std::path;
use std::fs;
use std::io::{Read, Write};
use std::path::Path;
use std::time::Instant;

use parser::Program;

const HELP: &str = r#"scefiruarier
assembler for scenite firuary

USAGE:
  scefiruarier [{-h|--help}] [--dry-run] [--output OUTPUT] [INPUT]

FLAGS:
  -h, --help            Prints this message and exists
  --dry-run             Compiles the source file however doesnt save it

OPTIONS:
  --output OUTPUT       Specifies the output path for the compiled binary (--
                        for stdout) [default: <INPUT FILENAME>.sfe]

ARGS:
  <INPUT>               Path to the source file (-- for stdin)
"#;

#[derive(Debug)]
struct CompilerArgs {
    input: Option<path::PathBuf>,
    output: Option<Option<path::PathBuf>>,
    dry_run: bool,
}

fn save(path: impl AsRef<Path>, binary: &[u8]) {
    fs::write(path, binary).expect("failed to write the binary")
}

fn main() {
    let args = parse_args().unwrap_or_else(|e| {
            eprintln!("Error: {e}.");
            std::process::exit(1);
        });

    let source_code = String::from_utf8(match args.input.as_ref() {
        Some(path) => fs::read(path).expect("was not able to read source file"),
        None => {
            let mut buf = Vec::new();
            std::io::stdin().read(&mut buf).expect("failed to read stdin");
            buf
        },
    }).expect("invalid utf-8 file or smth");

    let compile_start_time = Instant::now();
    let compiled_binary = Program::try_from(source_code.as_ref()).expect("fuuuck you :b");
    let compilation_took = compile_start_time.elapsed();

    match args.output.as_ref() {
        Some(output) => match output.as_ref() {
            Some(path) => save(path, compiled_binary.get_binary()),
            None => {
                let mut stdout = std::io::stdout();
                stdout.write(compiled_binary.get_binary()).expect("failed to print the binary");
                stdout.flush().expect("couldnt flush the stdout");
            },
        },
        None => {
            save(match args.input.as_ref() {
                None => format!("your code but cooked for useless cpu at 180°C in a horrible parser for {} seconds.sfe", compilation_took.as_secs()),
                Some(input_file) => format!("{}.sfe", Path::new(input_file).file_stem().expect("there is no filename (stem)?").to_str().expect("it is not a string..?")),
            }, compiled_binary.get_binary());
        }
    };
}


fn parse_args() -> Result<CompilerArgs, pico_args::Error> {
    let mut pargs = pico_args::Arguments::from_env();

    if pargs.contains(["-h", "--help"]) {
        print!("{HELP}");
        std::process::exit(0);
    };

    Ok(CompilerArgs {
        dry_run: pargs.contains("--dry-run"),
        output: pargs.opt_value_from_fn("--output", parse_path)?,
        input: pargs.free_from_fn(parse_path)?,
    })
}

fn parse_path(s: &str) -> Result<Option<path::PathBuf>, &'static str> {
    if s == "--" {
        Ok(None)
    } else {
        Ok(Some(s.into()))
    }
}
