#![allow(clippy::manual_strip)]
#[allow(unused_imports)]
use std::fs;
use std::{error::Error, fmt, io::Read, path::{Path}, str::FromStr};

/// Translate shaders to different formats.
#[derive(argh::FromArgs, Debug, Clone)]
struct Args {
  /// bitmask of the ValidationFlags to be used, use 0 to disable validation
  #[argh(option)]
  validate: Option<u8>,

  /// what policy to use for index bounds checking for arrays, vectors, and
  /// matrices.
  ///
  /// May be `Restrict` (force all indices in-bounds), `ReadZeroSkipWrite`
  /// (out-of-bounds indices read zeros, and don't write at all), or
  /// `Unchecked` (generate the simplest code, and whatever happens, happens)
  ///
  /// `Unchecked` is the default.
  #[argh(option)]
  index_bounds_check_policy: Option<BoundsCheckPolicyArg>,

  /// what policy to use for index bounds checking for arrays, vectors, and
  /// matrices, when they are stored in globals in the `storage` or `uniform`
  /// storage classes.
  ///
  /// Possible values are the same as for `index-bounds-check-policy`. If
  /// omitted, defaults to the index bounds check policy.
  #[argh(option)]
  buffer_bounds_check_policy: Option<BoundsCheckPolicyArg>,

  /// what policy to use for texture loads bounds checking.
  ///
  /// Possible values are the same as for `index-bounds-check-policy`. If
  /// omitted, defaults to the index bounds check policy.
  #[argh(option)]
  image_load_bounds_check_policy: Option<BoundsCheckPolicyArg>,

  /// what policy to use for texture stores bounds checking.
  ///
  /// Possible values are the same as for `index-bounds-check-policy`. If
  /// omitted, defaults to the index bounds check policy.
  #[argh(option)]
  image_store_bounds_check_policy: Option<BoundsCheckPolicyArg>,

  /// directory to dump the SPIR-V block context dump to
  // #[argh(option)]
  // block_ctx_dir: Option<String>,

  /// specify file path to process STDIN as
  #[argh(option)]
  stdin_file_path: Option<String>,

  /// generate debug symbols, only works for spv-out for now
  // #[argh(switch, short = 'g')]
  // generate_debug_symbols: bool,

  /// compact the module's IR and revalidate.
  ///
  /// Output files will reflect the compacted IR. If you want to see the IR as
  /// it was before compaction, use the `--before-compaction` option.
  #[argh(switch)]
  compact: bool,

  /// write the module's IR before compaction to the given file.
  ///
  /// This implies `--compact`. Like any other output file, the filename
  /// extension determines the form in which the module is written.
  #[argh(option)]
  before_compaction: Option<String>,

  /// show version
  #[argh(switch)]
  version: bool,

  /// the input and output files.
  ///
  /// First positional argument is the input file. If not specified, the
  /// input will be read from stdin. In the case, --stdin-file-path must also
  /// be specified.
  ///
  /// The rest arguments are the output files. If not specified, only
  /// validation will be performed.
  #[argh(positional)]
  files: Vec<String>,
}

/// Newtype so we can implement [`FromStr`] for `BoundsCheckPolicy`.
#[derive(Debug, Clone, Copy)]
struct BoundsCheckPolicyArg(naga::proc::BoundsCheckPolicy);

impl FromStr for BoundsCheckPolicyArg {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    use naga::proc::BoundsCheckPolicy;
    Ok(Self(match s.to_lowercase().as_str() {
      "restrict" => BoundsCheckPolicy::Restrict,
      "readzeroskipwrite" => BoundsCheckPolicy::ReadZeroSkipWrite,
      "unchecked" => BoundsCheckPolicy::Unchecked,
      _ => {
        return Err(format!(
          "Invalid value for --index-bounds-check-policy: {s}"
        ))
      }
    }))
  }
}


#[derive(Default)]
struct Parameters<'a> {
  validation_flags: naga::valid::ValidationFlags,
  bounds_check_policies: naga::proc::BoundsCheckPolicies,
  keep_coordinate_space: bool,
  spv_in: naga::front::spv::Options,
  spv_out: naga::back::spv::Options<'a>,
}

trait PrettyResult {
  type Target;
  fn unwrap_pretty(self) -> Self::Target;
}

fn print_err(error: &dyn Error) {
  eprint!("{error}");

  let mut e = error.source();
  if e.is_some() {
    eprintln!(": ");
  } else {
    eprintln!();
  }

  while let Some(source) = e {
    eprintln!("\t{source}");
    e = source.source();
  }
}

impl<T, E: Error> PrettyResult for Result<T, E> {
  type Target = T;
  fn unwrap_pretty(self) -> T {
    match self {
      Result::Ok(value) => value,
      Result::Err(error) => {
        print_err(&error);
        std::process::exit(1);
      }
    }
  }
}

fn main() {
  if let Err(e) = run() {
    print_err(e.as_ref());
    std::process::exit(1);
  }
}

/// Error type for the CLI
#[derive(Debug, Clone)]
struct CliError(&'static str);
impl fmt::Display for CliError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}
impl std::error::Error for CliError {}




fn run() -> Result<(), Box<dyn std::error::Error>> {
  env_logger::init();

  // Initialize default parameters
  //TODO: read the parameters from RON?
  let mut params = Parameters::default();

  // Parse commandline arguments
  let args: Args = argh::from_env();
  if args.version {
    println!("{}", env!("CARGO_PKG_VERSION"));
    return Ok(());
  }
  let (input_path, input) = if let Some(path) = args.files.first() {
    let path = Path::new(path);
    (path, fs::read(path)?)
  } else if let Some(path) = &args.stdin_file_path {
    let mut input = vec![];
    std::io::stdin().lock().read_to_end(&mut input)?;
    (Path::new(path), input)
  } else {
    return Err(CliError("Input file path is not specified").into());
  };
  let output_paths = args.files.get(1..).unwrap_or(&[]);

  // Update parameters from commandline arguments
  if let Some(bits) = args.validate {
    params.validation_flags = naga::valid::ValidationFlags::from_bits(bits)
      .ok_or(CliError("Invalid validation flags"))?;
  }
  if let Some(policy) = args.index_bounds_check_policy {
    params.bounds_check_policies.index = policy.0;
  }
  params.bounds_check_policies.buffer = match args.buffer_bounds_check_policy {
    Some(arg) => arg.0,
    None => params.bounds_check_policies.index,
  };
  params.bounds_check_policies.image_load = match args.image_load_bounds_check_policy {
    Some(arg) => arg.0,
    None => params.bounds_check_policies.index,
  };
  params.bounds_check_policies.image_store = match args.image_store_bounds_check_policy {
    Some(arg) => arg.0,
    None => params.bounds_check_policies.index,
  };

  params.spv_in = naga::front::spv::Options {
    adjust_coordinate_space: false,
    strict_capabilities: false,
    block_ctx_dump_prefix: None,
    // block_ctx_dump_prefix: args.block_ctx_dir.map(std::path::PathBuf::from),
  };

  params.spv_out.bounds_check_policies = params.bounds_check_policies;
  params.spv_out.flags.set(
    naga::back::spv::WriterFlags::ADJUST_COORDINATE_SPACE,
    !params.keep_coordinate_space,
  );

  let provider = wgslx_cli::FileProvider::new(); 

  let (mut module, input_text) = match Path::new(&input_path)
    .extension()
    .ok_or(CliError("Input filename has no extension"))?
    .to_str()
    .ok_or(CliError("Input filename not valid unicode"))?
  {
    // For now, only wgslx is supported
    "wgslx" => {
      let input = String::from_utf8(input)?;
      let id = provider.visit(input_path).expect("Unable to parse file"); 
      let result = naga::front::wgsl::parse_module(&provider, id);

      match result {
        Ok(v) => (v, Some(input)),
        Err(ref e) => {
          e.emit_to_stderr_with_provider(&provider);
          return Err(CliError("Could not parse WGSL").into());
        }
      }
    }
    _ => return Err(CliError("Unknown input file extension").into()),
  };

  // Include debugging information if requested.
  // if args.generate_debug_symbols {
  //   if let Some(ref input_text) = input_text {
  //     params
  //       .spv_out
  //       .flags
  //       .set(naga::back::spv::WriterFlags::DEBUG, true);
  //     params.spv_out.debug_info = Some(naga::back::spv::DebugInfo {
  //       source_code: input_text,
  //       file_name: input_path,
  //     })
  //   } else {
  //     eprintln!(
  //       "warning: `--generate-debug-symbols` was passed, \
  //        but input is not human-readable: {}",
  //       input_path.display()
  //     );
  //   }
  // }

  // Decide which capabilities our output formats can support.
  let validation_caps =
    output_paths
    .iter()
    .fold(naga::valid::Capabilities::all(), |caps, path| {
      use naga::valid::Capabilities as C;
      let missing = match Path::new(path).extension().and_then(|ex| ex.to_str()) {
        Some("wgsl") => C::CLIP_DISTANCE | C::CULL_DISTANCE,
        Some("metal") => C::CULL_DISTANCE,
        _ => C::empty(),
      };
      caps & !missing
    });

  // Validate the IR before compaction.
  let info = match naga::valid::Validator::new(params.validation_flags, validation_caps)
    .validate(&module)
  {
    Ok(info) => Some(info),
    Err(error) => {
      // Validation failure is not fatal. Just report the error.
      if let Some(input) = &input_text {
        let filename = input_path.file_name().and_then(std::ffi::OsStr::to_str);
        emit_annotated_error(&error, filename.unwrap_or("input"), input);
      }
      print_err(&error);
      None
    }
  };

  // Compact the module, if requested.
  let info = if args.compact || args.before_compaction.is_some() {
    // Compact only if validation succeeded. Otherwise, compaction may panic.
    if info.is_some() {
      // Write out the module state before compaction, if requested.
      if let Some(ref before_compaction) = args.before_compaction {
        write_output(&module, &info, &params, before_compaction)?;
      }

      naga::compact::compact(&mut module);

      // Re-validate the IR after compaction.
      match naga::valid::Validator::new(params.validation_flags, validation_caps)
        .validate(&module)
      {
        Ok(info) => Some(info),
        Err(error) => {
          // Validation failure is not fatal. Just report the error.
          eprintln!("Error validating compacted module:");
          if let Some(input) = &input_text {
            let filename = input_path.file_name().and_then(std::ffi::OsStr::to_str);
            emit_annotated_error(&error, filename.unwrap_or("input"), input);
          }
          print_err(&error);
          None
        }
      }
    } else {
      eprintln!("Skipping compaction due to validation failure.");
      None
    }
  } else {
    info
  };

  // If no output was requested, then report validation results and stop here.
  //
  // If the user asked for output, don't stop: some output formats (".txt",
  // ".dot", ".bin") can be generated even without a `ModuleInfo`.
  if output_paths.is_empty() {
    if info.is_some() {
      println!("Validation successful");
      return Ok(());
    } else {
      std::process::exit(-1);
    }
  }

  for output_path in output_paths {
    write_output(&module, &info, &params, output_path)?;
  }

  Ok(())
}

fn write_output(
  module: &naga::Module,
  info: &Option<naga::valid::ModuleInfo>,
  _params: &Parameters,
  output_path: &str,
) -> Result<(), Box<dyn std::error::Error>> {
  match Path::new(&output_path)
    .extension()
    .ok_or(CliError("Output filename has no extension"))?
    .to_str()
    .ok_or(CliError("Output filename not valid unicode"))?
  {
    // For now, only WGSL out is supported
    "wgsl" => {
      use naga::back::wgsl;

      let wgsl = wgsl::write_string(
        module,
        info.as_ref().ok_or(CliError(
          "Generating wgsl output requires validation to \
           succeed, and it failed in a previous step",
        ))?,
        wgsl::WriterFlags::empty(),
      )
        .unwrap_pretty();
      fs::write(output_path, wgsl)?;
    }
    other => {
      println!("Unknown output extension: {other}");
    }
  }

  Ok(())
}

use codespan_reporting::{
  diagnostic::{Diagnostic, Label},
  files::{SimpleFile},
  term::{
    self,
    termcolor::{ColorChoice, StandardStream},
  },
};
use naga::{front::wgsl::source_provider::{SourceProvider}, WithSpan};

pub fn emit_glsl_parser_error(errors: Vec<naga::front::glsl::Error>, filename: &str, source: &str) {
  let files = SimpleFile::new(filename, source);
  let config = codespan_reporting::term::Config::default();
  let writer = StandardStream::stderr(ColorChoice::Auto);

  for err in errors {
    let mut diagnostic = Diagnostic::error().with_message(err.kind.to_string());

    if let Some(range) = err.meta.to_range() {
      diagnostic = diagnostic.with_labels(vec![Label::primary((), range)]);
    }

    term::emit(&mut writer.lock(), &config, &files, &diagnostic).expect("cannot write error");
  }
}

pub fn emit_annotated_error<E: Error>(ann_err: &WithSpan<E>, filename: &str, source: &str) {
  let files = SimpleFile::new(filename, source);
  let config = codespan_reporting::term::Config::default();
  let writer = StandardStream::stderr(ColorChoice::Auto);

  let diagnostic = Diagnostic::error().with_labels(
    ann_err
      .spans()
      .map(|(span, desc)| {
        Label::primary((), span.to_range().unwrap()).with_message(desc.to_owned())
      })
      .collect(),
  );

  term::emit(&mut writer.lock(), &config, &files, &diagnostic).expect("cannot write error");
}
