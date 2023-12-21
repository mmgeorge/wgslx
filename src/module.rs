use naga::{front::wgsl::{source_provider::{FileId, Files}, parse_module}, valid::{Validator, Capabilities, ValidationFlags}, NamedExpression, Expression, TypeInner};
use crate::{file_sources::FileSources, module::format::format_type};

use self::{error::Error, diagnostic::Diagnostic, search_position::SearchPosition, definition::Definition, completion_canditate::{CompletionCandiate, CompletionKind}, format::format_function_definition};

pub mod error;
pub mod diagnostic;
pub mod search_position;

pub mod definition;
pub mod completion_canditate;
pub mod format;

pub enum CompletionType {
  /// Import path
  ImportPath,
  /// Member of struct
  StructMember(String),
  /// Variable, let, or function definition
  Definition,
  /// Type definition
  TypeDefinition,
}

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

    vec![]
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

  pub fn find_hoverable_at<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition) -> Option<Definition<'a>> {
    if let Some((handle, ..)) = Self::find_closest_at(&self.inner.global_variables, pos) {
      return Some(Definition::GlobalVariable(handle))
    }

    if let Some((handle, ..)) = Self::find_closest_at(&self.inner.constants, pos) {
      return Some(Definition::Constant(handle))
    }

    // Otherwise descend into the function that intersects the span 
    if let Some((.., func)) = Self::find_closest_at(&self.inner.functions, pos) {
      return self.find_hoverable_at_function(sources, pos, func)
    }

    None
  }

  pub fn find_completion_candidates_at2<'a>(&'a self, sources: &'a FileSources, changed: &'a FileSources, pos: SearchPosition, pos_changed: SearchPosition) -> Option<Vec<CompletionCandiate>> {
    // Get completion type
    let source = changed.source(pos_changed.file_id).unwrap();
    let substr = &source[..pos_changed.location];
    let index = substr.rfind(&['<', '>', ';', ':', '(', '=', '.', '"'])?;
    let character = substr.chars().nth(index)?;
    let completion_type = match character {
      // TODO: Should limit to only f32, i32, u32?
      '<' | '>' | ':' => CompletionType::TypeDefinition,
      ';' | '(' => CompletionType::Definition,
      '=' => {
        // The only case in which an '=' should yield a type definition, is if two tokens priror
        // is the reserved word "alias", e.g.;
        //
        // alias Foo = u32; 
        let substr = &substr[..index]; 
        let token1_end = substr.rfind(|c| !char::is_whitespace(c))? + 1;
        let substr = &substr[..token1_end]; 
        let token1_start = substr.rfind(char::is_whitespace)? + 1;
        let substr = &substr[..token1_start];
        let token0_end = substr.rfind(|c| !char::is_whitespace(c))? + 1;
        let substr = &substr[..token0_end];
        let token0_start = substr.rfind(char::is_whitespace)? + 1;
        let token = &substr[token0_start..token0_end];

        match token {
          "alias" => CompletionType::TypeDefinition,
          _ => CompletionType::Definition
        }
      }, 
      '"' => CompletionType::ImportPath,
      '.' => {
        let token_start = substr.rfind(char::is_whitespace)? + 1;
        let token = &source[token_start..index];

        CompletionType::StructMember(token.to_string())
      }
      _ => None?
    };

    match completion_type {
      CompletionType::TypeDefinition => self.find_type_completion_candiates(),
      CompletionType::StructMember(token) => self.find_struct_member_completion_candiates_at(&token, pos),
      CompletionType::Definition => self.find_definition_completion_candidates_at(sources, pos),
      _ => None,  
    }
  }

  pub fn find_struct_member_completion_candiates_at<'a>(&'a self, token: &'a str, pos: SearchPosition) -> Option<Vec<CompletionCandiate>> {
    let global = self.inner.global_variables.iter().find(|(_, var)| var.name.clone().unwrap() == token)
      .map(|(_, var)| var.ty);
    let constant = self.inner.constants.iter().find(|(_, var)| var.name.clone().unwrap() == token)
      .map(|(_, var)| var.ty);

    // First check if we are in a function, & prefer local delcarations firsts
    if let Some((.., func)) = Self::find_closest_at(&self.inner.functions, pos) {
      let local_var = func.local_variables.iter()
        .find(|(_, var)| var.name.clone().unwrap() == token)
        .map(|(_, var)| var.ty);

      let named_expression = func.named_expressions.iter()
        .find(|(_, named)| named.name == token)
        .map(|(_, named)| named.ty.unwrap());

      let ty = local_var
        .or(named_expression)
        .or(global)
        .or(constant)?;

      let ty = &self.inner.types[ty];

      if let TypeInner::Struct { members, .. } = &ty.inner {
        let candidates = members.iter()
          .map(|member| {
            let name = member.name.clone().unwrap(); 
            let ty = &self.inner.types[member.ty];
            let detail = format_type(&self.inner, ty); 
            
            CompletionCandiate::new(name, detail, CompletionKind::Property)
          })
          .collect();

        return  Some(candidates); 
      }

      return None; 
    }

    let ty = global.or(constant)?;
    let ty = &self.inner.types[ty];

    if let TypeInner::Struct { members, .. } = &ty.inner {
      let candidates = members.iter()
        .map(|member| {
          let name = member.name.clone().unwrap(); 
          let ty = &self.inner.types[member.ty];
          let detail = format_type(&self.inner, ty); 
          
          CompletionCandiate::new(name, detail, CompletionKind::Property)
        })
        .collect();

      return  Some(candidates); 
    }

    None
  }

  pub fn find_type_completion_candiates<'a>(&'a self) -> Option<Vec<CompletionCandiate>> {
    let types = self.inner.types.iter()
      .filter(|(_, ty)| ty.name.is_some())
      .map(|(_, ty)| CompletionCandiate::new(ty.name.clone().unwrap(), String::new(), CompletionKind::Type));

    let standard_types = vec![
      "f32", 
      "i32",
      "u32", 
      "vec2f", 
      "vec3f", 
      "vec4f", 
      "vec2i", 
      "vec3i",
      "vec4i",
      "vec2u", 
      "vec3u",
      "vec4u",
      "mat2x2f",
      "mat3x3f",
      "mat4x4f",
      "mat2x2i",
      "mat3x3i",
      "mat4x4i",
      "mat2x2u",
      "mat3x3u",
      "mat4x4u"
    ].into_iter()
    .map(|name| CompletionCandiate::new(name.to_string(), "[Native]".to_string(), CompletionKind::Type));

    Some(types.chain(standard_types).collect())
  }

  pub fn find_definition_completion_candidates_at<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition) -> Option<Vec<CompletionCandiate>> {
    let globals = self.inner.global_variables.iter()
      .map(|(_, var)| {
        let ty_str = format_type(&self.inner, &self.inner.types[var.ty]);
        
        CompletionCandiate::new(var.name.clone().unwrap(), ty_str, CompletionKind::Variable)
      });

    let constants = self.inner.constants.iter()
      .map(|(_, var)| {
        let ty_str = format_type(&self.inner, &self.inner.types[var.ty]);

        CompletionCandiate::new(var.name.clone().unwrap(), ty_str, CompletionKind::Constant)
      });

    let types = self.inner.types.iter()
      .filter(|(_, ty)| ty.name.is_some())
      .map(|(_, ty)| CompletionCandiate::new(ty.name.clone().unwrap(), format_type(&self.inner, ty), CompletionKind::Type));

    let functions = self.inner.functions.iter()
      .map(|(handle, func)| CompletionCandiate::new(
        func.name.clone().unwrap(),
        format_function_definition(&self.inner, handle), 
        CompletionKind::Function
      ));

    if let Some((.., func)) = Self::find_closest_at(&self.inner.functions, pos) {
      let local_vars = func.local_variables.iter()
        .filter(|(handle, _)| func.local_variables.get_span(*handle).end <= pos.location as u32)
        .map(|(_, var)| {
          let ty_str = format_type(&self.inner, &self.inner.types[var.ty]);

          CompletionCandiate::new(var.name.clone().unwrap(), ty_str, CompletionKind::Variable)
        });

      let named_expressions = func.named_expressions.iter()
        .filter(|(_, named)| named.span.end <= pos.location as u32)
        .map(|(_, named)| {
          let ty_str = format_type(&self.inner, &self.inner.types[named.ty.unwrap()]);
          
          CompletionCandiate::new(named.name.clone(), ty_str, CompletionKind::Variable)
        }); 

      return Some(globals.chain(constants)
                  .chain(functions)
                  .chain(types)
                  .chain(local_vars)
                  .chain(named_expressions)
                  .collect::<Vec<_>>()); 
    }

    let out = globals.chain(constants)
      .chain(types)
      .collect::<Vec<_>>();

    Some(out)
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

  pub fn find_hoverable_at_function<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition, func: &'a naga::Function) -> Option<Definition<'a>> {
    if let Some((handle, ..)) = Self::find_closest_at(&func.local_variables, pos) {
      eprintln!("Got local var!"); 
      return Some(Definition::LocalVariable(func, handle))
    }

    if let Some((handle, ..)) = Self::find_closest_named_expression_at(sources, pos, func) {
      return Some(Definition::LocalNamedExpression(func, handle))
    }

    self.find_definition_at_function(sources, pos, func)
    // None
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
