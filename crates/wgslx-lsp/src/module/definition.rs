use naga::{Handle, StructMember};

use super::{Module, format::{format_type, format_function_definition}};


pub enum Definition<'a> {
  Type(Handle<naga::Type>), 
  StructMember(Handle<naga::Type>, u32), 
  Constant(Handle<naga::Constant>),
  GlobalVariable(Handle<naga::GlobalVariable>),
  LocalNamedExpression(&'a naga::Function, Handle<naga::Expression>), 
  LocalVariable(&'a naga::Function, Handle<naga::LocalVariable>), 
  FunctionArgument(&'a naga::Function, naga::FunctionArgument), 
  Function(Handle<naga::Function>), 
}

impl<'a> Definition<'a> {
  pub fn get_span(&self, module: &Module) -> naga::Span {
    match self {
      Definition::Type(handle) => module.inner.types.get_span(*handle),
      Definition::Constant(handle) => module.inner.constants.get_span(*handle),
      Definition::GlobalVariable(handle) => module.inner.global_variables.get_span(*handle),
      Definition::LocalNamedExpression(func, handle) => func.named_expressions.get(handle).unwrap().span, 
      Definition::LocalVariable(func, handle) => func.local_variables.get_span(*handle),
      Definition::FunctionArgument(_func, _arg) => self.get_type_span(&module.inner),
      Definition::StructMember(handle, index) => Self::struct_member(&module.inner, *handle, *index).span,
      Definition::Function(func) => module.inner.functions.get_span(*func), 
    }
  }

  pub fn struct_member(module: &naga::Module, ty: Handle<naga::Type>, index: u32) -> &StructMember {
    let ty = &module.types[ty] ;

    if let naga::TypeInner::Struct { members, .. } = &ty.inner {
      return &members[index as usize];
    }

    panic!("InternalError: Tried to access struct member on non-struct"); 
  }

  pub fn get_type(&self, module: &naga::Module) -> Handle<naga::Type> {
    match self {
      Definition::Type(handle) => *handle,
      Definition::Constant(handle) => module.constants[*handle].ty,
      Definition::GlobalVariable(handle) => module.global_variables[*handle].ty,
      Definition::LocalNamedExpression(func, handle) => func.named_expressions.get(handle).unwrap().ty.unwrap(), 
      Definition::LocalVariable(func, handle) => func.local_variables[*handle].ty,
      Definition::FunctionArgument(_, arg) => arg.ty,
      Definition::StructMember(handle, index) => Self::struct_member(module, *handle, *index).ty,
      Definition::Function(func) => module.functions[*func].result.clone()
        .expect("Cannot get type for function that returns void").ty, 
    }
  }

  pub fn get_hoverable_text(&self, module: &naga::Module) -> String {
    match self {
      Definition::Function(func) => format_function_definition(module, *func),
      _ => format_type(module, &module.types[self.get_type(module)]), 
    }
  }

  pub fn get_type_span(&self, module: &naga::Module) -> naga::Span {
    let ty = self.get_type(module);

    module.types.get_span(ty)
  }

  pub fn try_from_named_use(func: Option<&'a naga::Function>, named: &'a naga::NamedExpressionUse) -> Option<Definition<'a>> {
    Some(Definition::LocalNamedExpression(func.unwrap(), named.expression))
  }

  pub fn try_from_expression(module: &'a naga::Module, func: Option<&'a naga::Function>, expr: &'a naga::Expression, is_load: bool) -> Option<Definition<'a>> {
    eprintln!("try_from_expression {:#?} ", expr);
    
    match expr {
      naga::Expression::GlobalVariable(handle) => Some(Definition::GlobalVariable(*handle)),
      naga::Expression::Constant(handle) => Some(Definition::Constant(*handle)),
      naga::Expression::LocalVariable(handle) => Some(Definition::LocalVariable(func.unwrap(), *handle)),
      naga::Expression::CallResult(handle) => Some(Definition::Function(*handle)), 
      naga::Expression::Load { pointer } => {
        let expr = if let Some(func) = func {
          &func.expressions[*pointer]
        } else {
          // Can this happen?
          &module.const_expressions[*pointer]
        }; 

        Definition::try_from_expression(module, func, expr, true)
      },
      naga::Expression::Access { base, .. } => {
        let expr = if let Some(func) = func {
          &func.expressions[*base]
        } else {
          &module.const_expressions[*base]
        }; 

        Definition::try_from_expression(module, func, expr, is_load)
      },
      naga::Expression::AccessIndex { base, index } => {
        let expr = if let Some(func) = func {
          &func.expressions[*base]
        } else {
          &module.const_expressions[*base]
        }; 

        let definition = Definition::try_from_expression(module, func, expr, is_load)?;

        if is_load {
          return Some(definition); 
        }
        
        let ty = definition.get_type(module);

        Some(Definition::StructMember(ty, *index))
      }, 
      naga::Expression::Compose { ty, .. } => Some(Definition::Type(*ty)),
      naga::Expression::Splat { .. } => None, 
      naga::Expression::Swizzle { .. } => None, 
      naga::Expression::FunctionArgument(arg_index) => {
        let arg = &func.unwrap().arguments[*arg_index as usize]; 

        Some(Definition::FunctionArgument(func.unwrap(), arg.clone()))
      },
      naga::Expression::ImageSample { .. } => None, 
      naga::Expression::ImageLoad { .. } => None, 
      naga::Expression::ImageQuery { .. } => None, 
      naga::Expression::Unary { .. } => None,
      naga::Expression::Binary { .. } => None, 
      naga::Expression::Select { .. } => None,
      naga::Expression::Derivative { .. } => None, 
      naga::Expression::Relational { .. } => None, 
      naga::Expression::Math { .. } => None, 
      naga::Expression::As { .. } => None, 
      naga::Expression::AtomicResult { ty, .. } => Some(Definition::Type(*ty)),
      naga::Expression::WorkGroupUniformLoadResult { ty } => Some(Definition::Type(*ty)),
      naga::Expression::ArrayLength(_) => None,
      naga::Expression::Literal(_) => None,
      naga::Expression::ZeroValue(_) => None,
      naga::Expression::RayQueryProceedResult => todo!(),
      naga::Expression::RayQueryGetIntersection { .. } => todo!(),
    }
  }

}




// publish struct Definition {
//   pub ident: Option<String>,
//   pub ident_span: naga::Span,
//   pub dty: naga::Type,
//   pub ty_span: naga::Span, 
// }

// impl Definition {
//   pub fn try_from_expression(module: &naga::Module, expression: &naga::Expression) -> Option<Definition> {
//     match expression {
//       naga::Expression::Literal(_) => None,
//       naga::Expression::Constant(handle) => {
//         let naga::Constant { name, ty, .. } = &module.constants[*handle];
//         let span = module.constants.get_span(*handle); 

//         Some(Definition {
//           ident: name.clone(),
//           ident_span: span,
//           ty: module.types[*ty].clone(),
//           ty_span: module.types.get_span(*ty)
//         })
//       },
//       naga::Expression::ZeroValue(_) => todo!(),
//       naga::Expression::Compose { ty, components } => todo!(),
//       naga::Expression::Access { base, index } => todo!(),
//       naga::Expression::AccessIndex { base, index } => todo!(),
//       naga::Expression::Splat { size, value } => todo!(),
//       naga::Expression::Swizzle { size, vector, pattern } => todo!(),
//       naga::Expression::FunctionArgument(_) => todo!(),
//       naga::Expression::GlobalVariable(_) => todo!(),
//       naga::Expression::LocalVariable(_) => todo!(),
//       naga::Expression::Load { pointer } => todo!(),
//       naga::Expression::ImageSample { image, sampler, gather, coordinate, array_index, offset, level, depth_ref } => todo!(),
//       naga::Expression::ImageLoad { image, coordinate, array_index, sample, level } => todo!(),
//       naga::Expression::ImageQuery { image, query } => todo!(),
//       naga::Expression::Unary { op, expr } => todo!(),
//       naga::Expression::Binary { op, left, right } => todo!(),
//       naga::Expression::Select { condition, accept, reject } => todo!(),
//       naga::Expression::Derivative { axis, ctrl, expr } => todo!(),
//       naga::Expression::Relational { fun, argument } => todo!(),
//       naga::Expression::Math { fun, arg, arg1, arg2, arg3 } => todo!(),
//       naga::Expression::As { expr, kind, convert } => todo!(),
//       naga::Expression::CallResult(_) => todo!(),
//       naga::Expression::AtomicResult { ty, comparison } => todo!(),
//       naga::Expression::WorkGroupUniformLoadResult { ty } => todo!(),
//       naga::Expression::ArrayLength(_) => todo!(),
//       naga::Expression::RayQueryProceedResult => todo!(),
//       naga::Expression::RayQueryGetIntersection { query, committed } => todo!(),
//     }
//   }
// } 
