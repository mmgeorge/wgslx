use std::{cell::{RefCell}};
use tower_lsp::lsp_types;


#[derive(Debug)]
pub struct ChangeState {
  inner: RefCell::<ChangeStateInner>
}

unsafe impl Send for ChangeState {}
unsafe impl Sync for ChangeState {}

impl ChangeState {
  pub fn new() -> Self {
    Self {
      inner: ChangeStateInner::new().into()
    }
  }

  pub fn insert(&self, changes: lsp_types::DidChangeTextDocumentParams) {
    self.inner.borrow_mut().insert(changes); 
  }

  pub fn clear(&self) {
    self.inner.borrow_mut().clear(); 
  }
}


#[derive(Debug)]
struct ChangeStateInner {}

impl ChangeStateInner {
  fn new() -> Self {
    Self {}
  }

  pub fn insert(&mut self, _changes: lsp_types::DidChangeTextDocumentParams) {
    todo!()
  }

  pub fn clear(&mut self) {
    todo!()
  }
}
