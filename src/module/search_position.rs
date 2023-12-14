use naga::front::wgsl::source_provider::{FileId, Files};
use tower_lsp::lsp_types;

#[derive(Clone, Copy)]
pub struct SearchPosition {
  pub inner: lsp_types::Position, 
  pub file_id: FileId
}

impl SearchPosition {
  pub fn new(inner: lsp_types::Position, file_id: FileId) -> Self { Self { inner, file_id } }

  pub fn start(&self, sources: &crate::FileSources) -> usize {
    let source = sources.source(self.file_id).unwrap(); 
    let line_offset = if self.inner.line > 0 {
      source.match_indices('\n').nth(self.inner.line as usize - 1).unwrap().0
    }
    else {
      0
    }; 

    // + 1 to skip over newline
    line_offset + self.inner.character as usize + 1
  }

  pub fn as_inner(&self) -> &lsp_types::Position {
    &self.inner
  }
  
}
