use std::{cell::{RefCell}, sync::Arc};
use tower_lsp::lsp_types;


#[derive(Debug)]
pub struct ChangeState {
  inner: Arc<RefCell::<ChangeStateInner>>
}

unsafe impl Send for ChangeState {}
unsafe impl Sync for ChangeState {}

impl ChangeState {
  pub fn new() -> Self {
    Self {
      inner: Arc::new(ChangeStateInner::new().into())
    }
  }

  pub fn insert(&self, changes: lsp_types::DidChangeTextDocumentParams) {
    (*self.inner).borrow_mut().insert(changes); 
  }

  pub fn clear(&self) {
    (*self.inner).borrow_mut().clear(); 
  }
}


#[derive(Debug)]
struct ChangeStateInner {
  
}

impl ChangeStateInner {
  fn new() -> Self {
    Self {}
  }

  pub fn insert(&mut self, changes: lsp_types::DidChangeTextDocumentParams) {
    
  }

  pub fn clear(&mut self) {
    
  }
}
