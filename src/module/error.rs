use tower_lsp::lsp_types::DiagnosticSeverity;

use super::diagnostic::Diagnostic;


#[derive(Debug)]
pub enum Error {
  Parse(naga::front::wgsl::ParseError) ,
  Validation(naga::WithSpan<naga::valid::ValidationError>), 
}

impl From<naga::front::wgsl::ParseError> for Error {
  fn from(value: naga::front::wgsl::ParseError) -> Self {
    Self::Parse(value)
  }
}

impl From<naga::WithSpan<naga::valid::ValidationError>> for Error {
  fn from(value: naga::WithSpan<naga::valid::ValidationError>) -> Self {
    Self::Validation(value)
  }
}

impl From<Error> for Vec<Diagnostic> {
  fn from(val: Error) -> Self {
    eprintln!("got error {:#?}", val); 
    let mut out = Vec::new();

    match val {
      Error::Parse(error) => {
        // Related diagnostics not supported by lsp-mode, pass as info instead
        for (span, label) in error.labels() {
          out.push(Diagnostic {
            severity: DiagnosticSeverity::INFORMATION, 
            message: label.to_string(),
            span, 
          }); 
        }

        // Put the main error and the root span
        if let Some((span, _label)) = error.labels().next() {
          let message = format!("[Parse] {}. {}", error.message(), error.notes()); 

          out.push(Diagnostic {
            severity: DiagnosticSeverity::ERROR, 
            message, 
            span, 
          });           
        }
      }
      Error::Validation(error) => {
        // Naga validation errors include a vector of spans (with labels, such as "naga::Expression [8]")
        // for marking code, as well as the actual error message which to view in it's entirety requires traversing
        // errors sources
        //
        // For instance, for a function Foo with an expression pow(1, 2.), naga will return a
        // 
        //   ValidationError::Function = Function [6] 'Foo' is invalid:
        //     source: FunctionError::Expression =  Expression [8] is invalid
        //       source: ExpressionError::InvalidArgumentType = Argument [0] to Pow as expression [4] has an invalid type.
        // 
        // We ignore everything but the inner error, and associate it with the last span
        if let Some((span, _label)) = error.spans().last() {
          // Get the most inner error
          let mut source: &dyn std::error::Error = &error.as_inner();
          while let Some(next) = std::error::Error::source(source) {
            source = next;
          }

          out.push(Diagnostic {
            // Should we also include the debug print of the rror?
            message: source.to_string(),
            severity: DiagnosticSeverity::ERROR,
            span: *span, 
          })
        }
      }
    };
    
    out
  }
}
