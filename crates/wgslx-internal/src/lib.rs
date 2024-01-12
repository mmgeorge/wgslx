

mod file_provider;

use core::fmt;
use std::path::Path;

use codespan_reporting::diagnostic;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use naga::compact::compact;
pub use naga::proc;
pub use naga::valid;
pub use naga::front;
pub use naga::back;
pub use naga::compact;
pub use naga::Module; 
pub use naga::WithSpan; 

pub use naga::front::wgsl::source_provider::SourceProvider; 
pub use naga::front::wgsl::parse_module; 

pub use file_provider::FileProvider;
pub use naga::valid::ModuleInfo;

/// Error type for the CLI
#[derive(Debug, Clone)]
pub struct Error(&'static str);

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl std::error::Error for Error {}


pub fn emit_annotated_error<E: std::error::Error>(ann_err: &WithSpan<E>, filename: &str, source: &str) {
  let files = SimpleFile::new(filename, source);
  let config = term::Config::default();
  let writer = term::termcolor::StandardStream::stderr(term::termcolor::ColorChoice::Auto);

  let diagnostic = diagnostic::Diagnostic::error().with_labels(
    ann_err
      .spans()
      .map(|(span, desc)| {
        diagnostic::Label::primary((), span.to_range().unwrap()).with_message(desc.to_owned())
      })
      .collect(),
  );

  term::emit(&mut writer.lock(), &config, &files, &diagnostic).expect("cannot write error");
}

pub fn compile_module(input_path: impl AsRef<Path>, compact: bool) -> Result<(Module, Option<ModuleInfo>), Error> {
  let provider = FileProvider::new(); 

  let mut module = match input_path.as_ref()
    .extension()
    .ok_or(Error("Input filename has no extension"))?
    .to_str()
    .ok_or(Error("Input filename not valid unicode"))?
  {
    // For now, only wgslx is supported
    "wgslx" => {
      // let input = String::from_utf8(input)?;
      let id = provider.visit(&input_path).expect("Unable to parse file"); 

      let result = parse_module(&provider, id); 

      match result {
        Ok(v) => Ok(v),
        Err(ref e) => {
          e.emit_to_stderr_with_provider(&provider);
          return Err(Error("Could not parse WGSL").into());
        }
      }
    }
    _ => return Err(Error("Unknown input file extension").into()),
  }?;

  let mut validator = valid::Validator::new(valid::ValidationFlags::all(), valid::Capabilities::all());
  let mut info = match validator.validate(&module) {
    Ok(info) => Some(info),
    Err(error) => {
      // Validation failure is not fatal. Just report the error.
      let _filename = input_path.as_ref().file_name().and_then(std::ffi::OsStr::to_str);
      // emit_annotated_error(&error, filename.unwrap_or("input"), input);
      // print_err(&error);
      None
    }
  };

  // Compact module if requested
  if compact {
    naga::compact::compact(&mut module);

    let mut validator = valid::Validator::new(valid::ValidationFlags::all(), valid::Capabilities::all());

    // Revalidate compacted module
    info = match validator.validate(&module) {
      Ok(info) => Some(info),
      Err(error) => {
        // Validation failure is not fatal. Just report the error.
        let _filename = input_path.as_ref().file_name().and_then(std::ffi::OsStr::to_str);
        // emit_annotated_error(&error, filename.unwrap_or("input"), input);
        // print_err(&error);
        None
      }
    };
    
  }

  Ok((module, info))
}
