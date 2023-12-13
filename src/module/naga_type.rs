
#[derive(Debug, Clone, Copy)]
pub enum NagaType<'a> {
  Global(&'a naga::GlobalVariable),
  Constant(&'a naga::Constant),
  Local(&'a naga::LocalVariable),
  Function(&'a naga::Function),
  ConstExpression(&'a naga::Expression),
  FunctionExpression(&'a naga::Function, &'a naga::Expression),
  FunctionResult(&'a naga::Function, &'a naga::FunctionResult),
  FunctionNamedExpression(&'a naga::NamedExpression),
  FunctionArgumentType(naga::Handle<naga::Type>),
  Type(&'a naga::Type),
  Statment(&'a naga::Statement),
}

