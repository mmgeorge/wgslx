use naga::{front::wgsl::{source_provider::{FileId, SourceProvider, Files}, parse_module}, valid::{Validator, Capabilities, ValidationFlags}, Function, Type, NamedExpression, Expression};
use crate::file_sources::FileSources;

use self::{error::Error, naga_type::NagaType, diagnostic::Diagnostic, search_position::SearchPosition, definition::Definition};

pub mod error;
pub mod naga_type;
pub mod diagnostic;
pub mod search_position;

pub mod definition;
pub mod format; 

pub struct Module {
  // sources: &'a FileSources,
  inner: naga::Module
}

impl Module {

  pub fn from(sources: &FileSources, id: FileId) -> Result<Module, Error> {
    let module = parse_module(sources, id)?;

    Ok(Self { inner: module })
  }

  pub fn as_inner(&self) -> &naga::Module {
    &self.inner
  }

  pub fn diagnostics(provider: &FileSources, id: FileId) -> Vec<Diagnostic> {
    if let Err(error) = Self::validate(provider, id) {
      return error.into(); 
    }

    return vec![]
  }

  fn validate(provider: &FileSources, id: FileId) -> Result<(), Error> {
    let module = parse_module(provider, id)?;
    let mut validator = Validator::new(ValidationFlags::all(), Capabilities::all());

    validator.validate(&module)?; 

    Ok(())
  }

  fn arena_iter<T>(arena: &naga::Arena<T>) -> impl Iterator<Item = (&T, naga::Span)> {
    arena.iter()
      .map(|(handle, var)| (var, arena.get_span(handle)))
  }

  fn find_closest_at<T>(arena: &naga::Arena<T>, pos: SearchPosition) -> Option<(naga::Handle<T>, &T)> {
    let item = arena.iter()
      .map(|(handle, item)| (handle, item, arena.get_span(handle)))
      .filter(move |(.., span)| pos.inside(span))
      .reduce(|prev, item| {
        let (.., span) = prev;
        let (.., next_span) = item; 
        let prev_size = span.end - span.start; 
        let size = next_span.end - next_span.start; 

        if size < prev_size {
          item
        } else {
          prev
        }
      });

    item.map(|(handle, item, ..)| (handle, item))
  }

  pub fn find_definition_at<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition) -> Option<Definition<'a>> {
    // First check expressions at the module level
    if let Some((.., expr)) = Self::find_closest_at(&self.inner.const_expressions, pos) {
      return Definition::try_from_expression(&self.inner, None, &expr, false)
    }

    // Otherwise descend into the function that intersects the span 
    if let Some((.., func)) = Self::find_closest_at(&self.inner.functions, pos) {
      return self.find_definition_at_function(sources, pos, func)
    }

    None
  }

  pub fn find_declaration_at<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition) -> Option<Definition<'a>> {
    if let Some((handle, ..)) = Self::find_closest_at(&self.inner.global_variables, pos) {
      return Some(Definition::GlobalVariable(handle))
    }

    if let Some((handle, ..)) = Self::find_closest_at(&self.inner.constants, pos) {
      return Some(Definition::Constant(handle))
    }

    // Otherwise descend into the function that intersects the span 
    if let Some((.., func)) = Self::find_closest_at(&self.inner.functions, pos) {
      return self.find_declaration_at_function(sources, pos, func)
    }

    None
  }

  pub fn find_type_at<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition) -> Option<&'a naga::Type> {
    let definition = self.find_definition_at(sources, pos)?;
    let ty = &definition.get_type(&self.inner);

    Some(&self.inner.types[*ty])
  }

  pub fn find_definition_at_function_result<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition, func: &'a naga::Function) -> Option<Definition<'a>> {
    let result = &func.result.clone()?;

    if pos.inside(&result.ty_span) {
      return Some(Definition::Type(result.ty)); 
    }

    None
  }

  pub fn find_definition_at_function<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition, func: &'a naga::Function) -> Option<Definition<'a>> {
    // Check named uses
    if let Some((.., named_use)) = Self::find_closest_at(&func.named_uses, pos) {
      return Definition::try_from_named_use(Some(func), named_use)
    }

    // Check if we are on the function result type
    if let Some(result) = self.find_definition_at_function_result(sources, pos, func) {
      return Some(result); 
    }
    
    if let Some((.., expr)) = Self::find_closest_at(&func.expressions, pos) {
      return Definition::try_from_expression(&self.inner, Some(func), expr, false)
    }

    None
  }

  pub fn find_declaration_at_function<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition, func: &'a naga::Function) -> Option<Definition<'a>> {
    // Check named uses
    // if let Some((.., named_use)) = Self::find_closest_at(&func.named_uses, pos) {
      // return Definition::try_from_named_use(Some(func), named_use)
    // }

    if let Some((handle, ..)) = Self::find_closest_at(&func.local_variables, pos) {
      eprintln!("Got local var!"); 
      return Some(Definition::LocalVariable(func, handle))
    }

    if let Some((handle, ..)) = Self::find_closest_named_expression_at(sources, pos, func) {
      return Some(Definition::LocalNamedExpression(func, handle))
    }

    None
  }

  pub fn find_closest_named_expression_at<'a>(sources: &'a FileSources, pos: SearchPosition, func: &'a naga::Function) -> Option<(naga::Handle<Expression>, &'a NamedExpression)> {
    let item = func.named_expressions.iter()
      .map(|(handle, item)| (handle, item, item.span))
      .filter(move |(.., span)| pos.inside(span))
      .reduce(|prev, item| {
        let (.., span) = prev;
        let (.., next_span) = item; 
        let prev_size = span.end - span.start; 
        let size = next_span.end - next_span.start; 

        if size < prev_size {
          item
        } else {
          prev
        }
      });

    item.map(|(handle, item, ..)| (*handle, item))
  }
} 
