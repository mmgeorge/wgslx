use naga::{front::wgsl::{source_provider::{FileId, SourceProvider, Files}, parse_module}, valid::{Validator, Capabilities, ValidationFlags}, Function};
use crate::file_sources::FileSources;

use self::{error::Error, naga_type::NagaType, diagnostic::Diagnostic, search_position::SearchPosition, definition::Definition};

pub mod error;
pub mod naga_type;
pub mod diagnostic;
pub mod search_position;

mod definition; 

pub struct Module {
  // sources: &'a FileSources,
  inner: naga::Module
}

impl Module {

  pub fn from(sources: &FileSources, id: FileId) -> Result<Module, Error> {
    let module = parse_module(sources, id)?;

    Ok(Self { inner: module })
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

  fn iter(&self) -> impl Iterator<Item = (NagaType, naga::Span)> {
    let module = &self.inner; 
    let globals = Self::arena_iter(&module.global_variables).map(|(item, span)| (NagaType::Global(item), span));
    let constants = Self::arena_iter(&module.constants).map(|(item, span)| (NagaType::Constant(item), span));
    let types = module.types.iter()
      .map(|(handle, var)| (var, module.types.get_span(handle)))
      .map(|(item, span)| (NagaType::Type(item), span)); 
    let const_exprs = Self::arena_iter(&module.const_expressions).map(|(item, span)| (NagaType::ConstExpression(item), span));
    let functions = Self::arena_iter(&module.functions).map(|(item, span)| (NagaType::Function(item), span));
    let function_exprs =  module.functions.iter()
      .flat_map(|(_, func)| Self::arena_iter(&func.expressions).map(|(expr, span)| (NagaType::FunctionExpression(func, expr), span)));

    let function_named_exprs =  module.functions.iter()
      .flat_map(|(_, func)| func.named_expressions.values().map(|expr| (NagaType::FunctionNamedExpression(expr), expr.span)));
    let function_named_uses =  module.functions.iter()
      .flat_map(|(_, func)| Self::arena_iter(&func.named_uses).map(|(expr, span)| (NagaType::FunctionNamedUse(func, expr), span)));
    
    let function_locals =  module.functions.iter()
      .flat_map(|(_, func)| Self::arena_iter(&func.local_variables))
      .map(|(item, span)| (NagaType::Local(item), span)); 
    let function_body =  module.functions.iter()
      .flat_map(|(_, func)| func.body.span_iter())
      .map(|(item, span)| (NagaType::Statment(item), *span));

    let function_args = module.functions.iter()
      .flat_map(|(_, func)| func.arguments.iter())
      .map(|arg| (NagaType::FunctionArgumentType(arg.ty), arg.ty_span));
    
    let function_result = module.functions.iter()
      .filter(|(_, func)| func.result.is_some())
      .map(|(_, func)| (NagaType::FunctionResult(func, func.result.as_ref().unwrap()), func.result.as_ref().unwrap().ty_span));

    globals
      .chain(constants)
      .chain(const_exprs)
      .chain(types)
      .chain(functions)
      .chain(function_exprs)
      .chain(function_named_exprs)
      .chain(function_named_uses)
      .chain(function_locals)
      .chain(function_body)
      .chain(function_args)
      .chain(function_result)
  }

  fn find_closest_at<T>(arena: &naga::Arena<T>, pos: SearchPosition) -> Option<&T> {
    let item = arena.iter()
      .map(|(handle, item)| (item, arena.get_span(handle)))
      .filter(move |(_item, span)| pos.inside(span))
      .reduce(|prev, item| {
        let (_, span) = prev;
        let (_, next_span) = item; 
        let prev_size = span.end - span.start; 
        let size = next_span.end - next_span.start; 

        if size < prev_size {
          item
        } else {
          prev
        }
      });

    item.map(|item| item.0)
  }
  

  fn iter_intersecting<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition) -> impl Iterator<Item = (NagaType, naga::Span)> {
    self.iter()
      .map(|(item, span)| {
        if let NagaType::FunctionExpression(func, expr) = item {
          eprintln!("named {:?} {:?}", expr, span); 
        }
        (item, span, sources.span_source(&span))
      })
      .filter(|(.., source)| source.is_some())
      .filter(move |(item, span, source)| {
        pos.location >= span.start as usize && pos.location <= span.end as usize
      })
      .map(|(item, span, _)| (item, span))
  }

  pub fn find_at<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition) -> Option<(NagaType, naga::Span)> {
    let out = self.iter_intersecting(sources, pos)
      .filter(|(_, span)| span.file_id == Some(pos.file_id))
      .reduce(|prev, item| {
        let (_, span) = prev;
        let (_, next_span) = item; 
        let prev_size = span.end - span.start; 
        let size = next_span.end - next_span.start; 

        if size < prev_size {
          item
        } else {
          prev
        }
      });

    out
  }

  pub fn find_definition_at<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition) -> Option<Definition<'a>> {
    // First check expressions at the module level
    if let Some(expr) = Self::find_closest_at(&self.inner.const_expressions, pos) {
      return Definition::try_from_expression(&self.inner, None, &expr)
    }

    // Otherwise descend into the function that intersects the span 
    if let Some(func) = Self::find_closest_at(&self.inner.functions, pos) {
      return self.find_definition_at_function(sources, pos, func)
    }

    None
  }

  pub fn find_definition_at_function<'a>(&'a self, sources: &'a FileSources, pos: SearchPosition, func: &'a naga::Function) -> Option<Definition<'a>> {
    // Check named uses
    if let Some(named_use) = Self::find_closest_at(&func.named_uses, pos) {
      return Definition::try_from_named_use(Some(func), named_use)
    }
    
    if let Some(expr) = Self::find_closest_at(&func.expressions, pos) {
      return Definition::try_from_expression(&self.inner, Some(func), &expr)
    }

    // // Otherwise descend into the function that intersects the span 
    // if let Some(func) = Self::find_closest_at(&self.inner.functions, pos) {
    //   return self.find_definition_at_function(&self, sources: &FileSources, pos: SearchPosition)
    // }

    None
  }
  

  pub fn find_span_at(&self, sources: &FileSources, pos: SearchPosition) -> Option<naga::Span> {
    let (naga_type, span) = self.find_at(sources, pos)?;
    let module = &self.inner; 
    let source = sources.source(pos.file_id).unwrap();
    let position_start = pos.location; 

      eprintln!("find_span_at ty: {:#?}", naga_type); 

    match naga_type {
      NagaType::FunctionNamedUse(func, naga::NamedExpressionUse { expression } )
        => Some(func.expressions.get_span(*expression)),
      NagaType::FunctionArgumentType(handle) => Some(module.types.get_span(handle)),
      NagaType::FunctionResult(_, naga::FunctionResult { ty, .. }) => Some(module.types.get_span(*ty)),
      NagaType::FunctionExpression(_, naga::Expression::GlobalVariable(handle)) => Some(module.global_variables.get_span(*handle)),
      NagaType::FunctionExpression(_, naga::Expression::Constant(handle)) => Some(module.constants.get_span(*handle)),
      NagaType::FunctionExpression(_, naga::Expression::CallResult(handle)) => Some(module.functions.get_span(*handle)),
      NagaType::FunctionExpression(func, naga::Expression::Compose { components, ty }) => {
        // We can only wind up here if we are pointing directly to a named expression. Otherwise we have a
        // load of some variable, or an accessor, which have their own spans and would have taken priority.
        let substr = &source[span.start as usize..position_start + 1]; // +1 for inclusive range

        // If we are left of the paren, jump to the type we are composing
        if substr.rfind('(').is_none() {
          return Some(module.types.get_span(*ty));
        }

        // Otherwise, we are within the compose. Jump to closest index definition 
        // let index = substr.matches(',').count();
        // let component = components[index];

        // Some(func.expressions.get_span(component))

        None
      },
      NagaType::FunctionExpression(func, naga::Expression::Load { pointer }) => {
        match &func.expressions[*pointer] {
          naga::Expression::LocalVariable(handle) => Some(func.local_variables.get_span(*handle)),
          naga::Expression::AccessIndex { base, .. } => {
            match &func.expressions[*base] {
              naga::Expression::FunctionArgument(index) => {
                let arg = &func.arguments[*index as usize];

                Some(module.types.get_span(arg.ty))
              },
              _ => None
            }
          },
          _ => None
        }
      }, 
      NagaType::FunctionExpression(func, naga::Expression::AccessIndex { base, index }) => {
        // AccessIndex refer to indexing a property on a struct. If we left of the .,
        // return the variable that we are indexing. Otherwise, we will jump to the
        // corresponding type definition for the struct
        let expression = &func.expressions[*base];
        let source_to_end = &source[..span.end as usize];
        let dot_offset = source_to_end.rfind('.').unwrap(); // Must exist for AccessIndex

        match expression {
          naga::Expression::FunctionArgument(arg_index) => {
            let arg = &func.arguments[*arg_index as usize];
            let ty = &module.types[arg.ty];
            let span = module.types.get_span(arg.ty); 

            // If we are before the dot, return the variable
            if position_start < dot_offset {
              return Some(func.expressions.get_span(*base)); 
            }

            match &ty.inner {
              naga::TypeInner::Struct { members, .. } => {
                let name = members[*index as usize].name.as_ref()?;
                let substr = sources.source_at(span)?;
                let substr_start = substr.find(name)? as u32;
                let start = substr_start + span.start;
                let end = start + substr.len() as u32; 

                Some(naga::Span::new(start, end, span.file_id))
              },
              _ => Some(span)
            }
          },
          _ => Some(func.expressions.get_span(*base))
        }
      }, 
      _ => None
    }
  }

  pub fn find_type_at(&self, sources: &FileSources, pos: SearchPosition) -> Option<&naga::Type> {
    let (item, span) = self.find_at(sources, pos)?;
    let module = &self.inner; 
    let source = sources.source(pos.file_id).unwrap();
    let location = pos.location; 

    let ty = match item {
      NagaType::Global(naga::GlobalVariable { ty, .. }) => Some(&module.types[*ty]),
      NagaType::Constant(naga::Constant { ty, .. }) => Some(&module.types[*ty]),
      NagaType::FunctionNamedExpression(naga::NamedExpression { ty: Some(ty), .. }) => Some(&module.types[*ty]),
      NagaType::Local(naga::LocalVariable { ty, .. }) => Some(&module.types[*ty]),
      NagaType::FunctionExpression(func, naga::Expression::Compose { components, ty }) => {
        // We can only wind up here if we are pointing directly to a named expression. Otherwise we have a
        // load of some variable, or an accessor, which have their own spans and would have taken priority.
        let substr = &source[span.start as usize..location + 1]; // +1 for inclusive range

        // If we are left of the paren, jump to the type we are composing
        if substr.rfind('(').is_none() {
          Some(&module.types[*ty])
        } else {
          // Otherwise, we are within the compose. Jump to closest index definition 
          let index = substr.matches(',').count();
          let component = components[index];
          let named_expr = func.named_expressions.get(&component)?; 
          let named_ty = &module.types[named_expr.ty?]; 
          
          Some(named_ty)
        }
      },
      NagaType::FunctionExpression(func, naga::Expression::Load { pointer }) => {
        match &func.expressions[*pointer] {
          naga::Expression::LocalVariable(handle) => Some(&module.types[func.local_variables[*handle].ty]),
          naga::Expression::AccessIndex { base, .. } => {
            match &func.expressions[*base] {
              naga::Expression::FunctionArgument(index) => {
                let arg = &func.arguments[*index as usize];

                Some(&module.types[arg.ty])
              },
              _ => None
            }
          },
          _ => None
        }
      }, 
      _ => None
    };

    ty
  }
} 
