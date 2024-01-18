
mod module_source_provider;

use core::fmt;
use std::fs;
use std::path::Path;

use codespan_reporting::diagnostic;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
pub use module_source_provider::ModuleSourceProvider;
use naga::back::wgsl;
pub use naga::proc;
pub use naga::valid;
pub use naga::front;
pub use naga::back;
pub use naga::compact;
pub use naga::Module; 
pub use naga::WithSpan; 

pub use naga::front::wgsl::source_provider::SourceProvider; 
pub use naga::front::wgsl::parse_module; 

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

pub fn compile_module(root_path: impl AsRef<Path>, compact: bool) -> Result<(Module, Option<ModuleInfo>), Error> {
  let provider = ModuleSourceProvider::new(&root_path);

  let lib_file = provider.visit("lib.wgslx")
    .ok_or(Error("Unable to find lib.wgslx. Does the file exist?"))?;

  let mut module = parse_module(&provider, lib_file).map_err(|error| {
    error.emit_to_stderr_with_provider(&provider); 
    Error("Parsing failed.")
  })?; 

  let mut validator = valid::Validator::new(valid::ValidationFlags::all(), valid::Capabilities::all());
  let info = validator.validate(&module).map_err(|error| {
    error.emit_to_stderr_with_provider(&provider); 
    Error("Validation failed.")
  })?; 

  // No compaction required, early out
  if !compact {
    return Ok((module, Some(info)))
  }

  naga::compact::compact(&mut module);

  // Revalidate compacted module
  let mut validator = valid::Validator::new(valid::ValidationFlags::all(), valid::Capabilities::all());
  let info = validator.validate(&module).map_err(|error| {
    error.emit_to_stderr_with_provider(&provider); 
    Error("Compaction failed. Compacted module failed validation")
  })?;

  Ok((module, Some(info)))
}

pub fn write_compiled_output_to_string(
  module: &naga::Module,
  info: &Option<ModuleInfo>,
) -> Result<String, Box<dyn std::error::Error>> {
  Ok(
    wgsl::write_string(
      module,
      info.as_ref().ok_or(Error(
        "Generating wgsl output requires validation to \
         succeed, and it failed in a previous step",
      ))?,
      wgsl::WriterFlags::empty(),
    )?)
}


pub fn write_compiled_output_to_file(
  module: &naga::Module,
  info: &Option<ModuleInfo>, 
  output_path: impl AsRef<Path>,
) -> Result<(), Box<dyn std::error::Error>> {
  match Path::new(&output_path.as_ref())
    .extension()
    .ok_or(Error("Output filename has no extension"))?
    .to_str()
    .ok_or(Error("Output filename not valid unicode"))?
  {
    // For now, only WGSL out is supported
    "wgsl" => {
      let wgsl = write_compiled_output_to_string(module, info)?;

      fs::write(output_path, wgsl)?;
    }
    other => {
      println!("Unknown output extension: {other}");
    }
  }

  Ok(())
}
