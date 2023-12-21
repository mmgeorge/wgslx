use naga::front::wgsl::source_provider::{FileId, Files};
use tower_lsp::lsp_types;

#[derive(Clone, Copy)]
pub struct SearchPosition {
  pub inner: lsp_types::Position, 
  pub file_id: FileId, 
  pub location: usize
}

impl SearchPosition {
  pub fn new(sources: &crate::FileSources, inner: lsp_types::Position, file_id: FileId) -> Self {
    let location = Self::location(sources, inner, file_id); 
    
    Self { inner, file_id, location }
  }

  pub fn inside(&self, span: &naga::Span) -> bool {
    Some(self.file_id) == span.file_id && self.location >= span.start as usize && self.location <= span.end as usize
  }

  pub fn location(sources: &crate::FileSources, pos: lsp_types::Position, file_id: FileId) -> usize {
    let source = sources.source(file_id).unwrap(); 
    let line_offset = if pos.line > 0 {
      source.match_indices('\n').nth(pos.line as usize - 1).unwrap().0
    }
    else {
      0
    }; 

    // + 1 to skip over newline
    line_offset + pos.character as usize + 1
  }
}
