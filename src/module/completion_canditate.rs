use tower_lsp::lsp_types::{self, CompletionItemKind};


pub enum CompletionKind {
  Function,
  Property,
  Variable,
  Struct,
  Type,
  Constant
}


pub struct CompletionCandiate {
  label: String,
  detail: String,
  kind: CompletionKind
}

impl CompletionCandiate {
  pub fn new(label: String, detail: String, kind: CompletionKind) -> Self {
    Self { label, detail, kind }
  }
}


impl Into<lsp_types::CompletionItem> for CompletionCandiate {
  fn into(self) -> lsp_types::CompletionItem {
    // lsp_types::CompletionItem::new_simple(self.label, self.detail)
    lsp_types::CompletionItem {
      label: self.label,
      detail: Some(self.detail),
      kind: Some(match self.kind {
        CompletionKind::Function => CompletionItemKind::FUNCTION,
        CompletionKind::Property => CompletionItemKind::PROPERTY,
        CompletionKind::Variable => CompletionItemKind::VARIABLE,
        CompletionKind::Constant => CompletionItemKind::CONSTANT,
        CompletionKind::Struct => CompletionItemKind::STRUCT,
        CompletionKind::Type => CompletionItemKind::TYPE_PARAMETER,
    }), 
      ..Default::default()
    }
  }
}
