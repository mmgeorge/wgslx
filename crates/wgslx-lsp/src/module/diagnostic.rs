use internal::ModuleSourceProvider;
use tower_lsp::lsp_types; 

pub struct Diagnostic {
  pub severity: lsp_types::DiagnosticSeverity,
  pub message: String,
  pub span: naga::Span,
}

impl Diagnostic {
  pub fn to_lsp(self, sources: &ModuleSourceProvider) -> lsp_types::Diagnostic {
    let source = sources.span_source(&self.span).unwrap(); 
    let location = self.span.location(&source);
    let start = lsp_types::Position { line: location.line_number - 1, character: location.line_position - 1 };
    let end = lsp_types::Position { line: location.line_number - 1, character: location.line_position - 1 + location.length };
    let range = lsp_types::Range::new(start, end); 

    lsp_types::Diagnostic {
      severity: Some(self.severity),
      message: self.message,
      range, 
      ..Default::default()
    }}
  
}
