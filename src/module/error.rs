
#[derive(Debug)]
pub enum Error {
  Parse(naga::front::wgsl::ParseError) ,
  Validation(naga::WithSpan<naga::valid::ValidationError>), 
}

impl From<naga::front::wgsl::ParseError> for Error {
  fn from(value: naga::front::wgsl::ParseError) -> Self {
    Self::Parse(value)
  }
}

impl From<naga::WithSpan<naga::valid::ValidationError>> for Error {
  fn from(value: naga::WithSpan<naga::valid::ValidationError>) -> Self {
    Self::Validation(value)
  }
}


