use naga::{front::wgsl::parse_translation_unit, front::wgsl::error::Error};

use crate::{module::{search_position::SearchPosition, definition::Definition}, FileSources};

pub enum CompletionType {
  /// Import path
  // ImportPath,
  /// Member of struct
  StructMember,
  /// Variable, let, or function definition
  Definition(naga::Span),
  /// Type definition
  TypeDefinition,
}

pub fn find_completion_type_at<'a>(sources: &'a FileSources, pos: SearchPosition) -> Option<CompletionType> {
  match parse_translation_unit(sources, pos.file_id) {
    Ok(unit) => {
        eprintln!("Completion, got tu {:#?}", unit); 
      Some(CompletionType::Definition(naga::Span::UNDEFINED))
    },
    Err(error) => match error {
      Error::Unexpected(span, expected_token) => {
        eprintln!("Expected {:?} {:?}", span, expected_token); 
        Some(CompletionType::Definition(span))
      },
      Error::UnexpectedComponents(_) => todo!(),
      Error::UnexpectedOperationInConstContext(_) => todo!(),
      Error::BadNumber(_, _) => todo!(),
      Error::BadPath { span } => todo!(),
      Error::BadMatrixScalarKind(_, _) => todo!(),
      Error::BadAccessor(_) => todo!(),
      Error::BadTexture(_) => todo!(),
      Error::BadTypeCast { span, from_type, to_type } => todo!(),
      Error::BadTextureSampleType { span, scalar } => todo!(),
      Error::BadIncrDecrReferenceType(_) => todo!(),
      Error::InvalidResolve(_) => todo!(),
      Error::InvalidForInitializer(_) => todo!(),
      Error::InvalidBreakIf(_) => todo!(),
      Error::InvalidGatherComponent(_) => todo!(),
      Error::InvalidConstructorComponentType(_, _) => todo!(),
      Error::InvalidIdentifierUnderscore(_) => todo!(),
      Error::ReservedIdentifierPrefix(_) => todo!(),
      Error::UnknownAddressSpace(_) => todo!(),
      Error::RepeatedAttribute(_) => todo!(),
      Error::UnknownAttribute(_) => todo!(),
      Error::UnknownBuiltin(_) => todo!(),
      Error::UnknownAccess(_) => todo!(),
      Error::UnknownIdent(_, _) => todo!(),
      Error::UnknownScalarType(_) => todo!(),
      Error::UnknownType(_) => todo!(),
      Error::UnknownStorageFormat(_) => todo!(),
      Error::UnknownConservativeDepth(_) => todo!(),
      Error::SizeAttributeTooLow(_, _) => todo!(),
      Error::AlignAttributeTooLow(_, _) => todo!(),
      Error::NonPowerOfTwoAlignAttribute(_) => todo!(),
      Error::InconsistentBinding(_) => todo!(),
      Error::TypeNotConstructible(_) => todo!(),
      Error::TypeNotInferrable(_) => todo!(),
      Error::InitializationTypeMismatch { name, expected, got } => todo!(),
      Error::MissingType(_) => todo!(),
      Error::MissingAttribute(_, _) => todo!(),
      Error::InvalidAtomicPointer(_) => todo!(),
      Error::InvalidAtomicOperandType(_) => todo!(),
      Error::InvalidRayQueryPointer(_) => todo!(),
      Error::Pointer(_, _) => todo!(),
      Error::NotPointer(_) => todo!(),
      Error::NotReference(_, _) => todo!(),
      Error::InvalidAssignment { span, ty } => todo!(),
      Error::ReservedKeyword(span) => {
        Some(CompletionType::Definition(span))
      },
      Error::Redefinition { previous, current } => todo!(),
      Error::RecursiveDeclaration { ident, usage } => todo!(),
      Error::CyclicDeclaration { ident, path } => todo!(),
      Error::InvalidSwitchValue { uint, span } => todo!(),
      Error::CalledEntryPoint(_) => todo!(),
      Error::WrongArgumentCount { span, expected, found } => todo!(),
      Error::FunctionReturnsVoid(_) => todo!(),
      Error::InvalidWorkGroupUniformLoad(_) => todo!(),
      Error::Internal(_) => todo!(),
      Error::ExpectedConstExprConcreteIntegerScalar(_) => todo!(),
      Error::ExpectedNonNegative(_) => todo!(),
      Error::ExpectedPositiveArrayLength(_) => todo!(),
      Error::MissingWorkgroupSize(_) => todo!(),
      Error::ConstantEvaluatorError(_, _) => todo!(),
      Error::AutoConversion { dest_span, dest_type, source_span, source_type } => todo!(),
    }
  }
}

