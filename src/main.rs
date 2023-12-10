
use std::cell::{UnsafeCell};
use std::collections::HashMap;
use std::fs::{self};
use std::path::{Path, PathBuf};

use naga::front::wgsl::source_provider::{FileId, SourceProvider, self, File};
use naga::front::wgsl::{parse_module};
use naga::valid::{Validator, ValidationFlags, Capabilities};
use tower_lsp::{lsp_types::*, jsonrpc};
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct WgslxLanguageServer {
  client: Client,
}

struct FileSources {
  inner: UnsafeCell::<FileSourcesInner>
}

struct FileSourcesInner {
  pub ids: HashMap<PathBuf, Option<FileId>>, 
  pub files: HashMap<FileId, File>,
  pub counter: u32, 
}

impl FileSourcesInner {
  fn ensure(&mut self, path: PathBuf) -> Option<FileId> {
    let entry = self.ids.entry(path.clone())
      .or_insert_with(|| {
        self.counter += 1;
        eprintln!("Read file {}", path.to_string_lossy()); 
        let source = fs::read_to_string(&path).expect("got value"); 
        eprintln!("got file {}", path.to_string_lossy()); 
        self.files.insert(
          self.counter,
          File::new(self.counter, path, source)); 
        Some(self.counter)
      });

    *entry
  }

  fn insert(&mut self, path: PathBuf, source: String) -> FileId {
    let id = self.ensure(path).unwrap();
    let file = self.files.get_mut(&id).unwrap();

    file.source = source;

    id
  }
}

impl FileSources {
  fn new() -> Self {
    Self {
      inner: FileSourcesInner {
        ids: HashMap::new(),
        files: HashMap::new(),
        counter: 0
      }.into()
    }
  }
}

impl FileSources {
  fn insert(&mut self, path: impl AsRef<Path>, source: &str) -> FileId {
    self.inner.get_mut().insert(path.as_ref().to_owned(), source.to_owned())
  }

  fn span_source(&self, span: &naga::Span) -> Option<&str> {
    let id = span.file_id?;
    let file = self.get(id)?;

    Some(file.source())
  }
}


impl source_provider::SourceProvider<'_> for FileSources {

  fn visit(&self, path: &Path) -> Option<FileId> {
    unsafe {
      let inner = &mut *self.inner.get();

      inner.ensure(path.to_owned())
    }
  }

  fn get(&self, id: FileId) -> Option<&source_provider::File> {
    unsafe {
      let inner = &*self.inner.get();

      inner.files.get(&id)
    }
  }
}

impl<'a> source_provider::Files<'a> for FileSources {
    type Source = &'a str;
    type FileId = FileId;
    type Name = &'a str;
    
    fn name(&'a self, file_id: FileId) -> Result<Self::Name, source_provider::Error> {
        let file = self.get(file_id).ok_or(source_provider::Error::FileMissing)?; 

        Ok(file.name())
    }

    fn source(&self, file_id: FileId) -> Result<&str, source_provider::Error> {
        let file = self.get(file_id).ok_or(source_provider::Error::FileMissing)?; 

        Ok(file.source().as_ref())
    }

    fn line_index(&self, file_id: FileId, byte_index: usize) -> Result<usize, source_provider::Error> {
        let file = self.get(file_id).ok_or(source_provider::Error::FileMissing)?; 

        file.line_index((), byte_index)
    }

    fn line_range(&self, file_id: FileId, line_index: usize) -> Result<std::ops::Range<usize>, source_provider::Error> {
        let file = self.get(file_id).ok_or(source_provider::Error::FileMissing)?; 

        file.line_range((), line_index)
    }
}

#[derive(Debug, Clone, Copy)]
enum NagaType<'a> {
  Global(&'a naga::GlobalVariable),
  Constant(&'a naga::Constant),
  Local(&'a naga::LocalVariable),
  Function(&'a naga::Function),
  ConstExpression(&'a naga::Expression),
  FunctionExpression(&'a naga::Function, &'a naga::Expression),
  Type(&'a naga::Type),
  Statment(&'a naga::Statement),
}


impl WgslxLanguageServer {

  fn diagnostics(&self, provider: &FileSources, id: FileId) -> Result<naga::Module, Error>{
    let module = parse_module(provider, id)?;
    let mut validator = Validator::new(ValidationFlags::all(), Capabilities::all());
    validator.validate(&module)?; 

    Ok(module)
  }

  fn arena_iter<T>(arena: &naga::Arena<T>) -> impl Iterator<Item = (&T, naga::Span)> {
    arena.iter()
      .map(|(handle, var)| (var, arena.get_span(handle)))
  }

  fn module_iter(module: &naga::Module) -> impl Iterator<Item = (NagaType, naga::Span)> {
    let globals = Self::arena_iter(&module.global_variables).map(|(item, span)| (NagaType::Global(item), span));
    let constants = Self::arena_iter(&module.constants).map(|(item, span)| (NagaType::Constant(item), span));
    let types = module.types.iter()
      .map(|(handle, var)| (var, module.types.get_span(handle)))
      .map(|(item, span)| (NagaType::Type(item), span)); 
    let const_exprs = Self::arena_iter(&module.const_expressions).map(|(item, span)| (NagaType::ConstExpression(item), span));
    let functions = Self::arena_iter(&module.functions).map(|(item, span)| (NagaType::Function(item), span));
    let function_exprs =  module.functions.iter()
          .flat_map(|(_, func)| Self::arena_iter(&func.expressions).map(|(expr, span)| (NagaType::FunctionExpression(func, expr), span)));
    let function_locals =  module.functions.iter()
      .flat_map(|(_, func)| Self::arena_iter(&func.local_variables))
      .map(|(item, span)| (NagaType::Local(item), span)); 
    let function_body =  module.functions.iter()
      .flat_map(|(_, func)| func.body.span_iter())
      .map(|(item, span)| (NagaType::Statment(item), *span)); 

    globals
      .chain(constants)
      .chain(const_exprs)
      .chain(types)
      .chain(functions)
      .chain(function_exprs)
      .chain(function_locals)
      .chain(function_body)
  }

  fn intersecting_items<'a>(&'a self, sources: &FileSources, module: &'a naga::Module, position: &Position) -> Vec<(NagaType, naga::Span)> {
    Self::module_iter(module)
      .map(|(item, span)| (item, span, sources.span_source(&span)))
      .filter(|(.., source)| source.is_some())
      .filter(|(item, span, source)| {
        let prefix = &source.unwrap()[..span.start as usize];
        let substring = &source.unwrap()[span.start as usize..span.end as usize];
        let line_number = prefix.matches('\n').count() as u32;
        let line_count = substring.matches('\n').count() as u32;
        let line_start = prefix.rfind('\n').map(|pos| pos + 1).unwrap_or(0); 
        let start_char = source.unwrap()[line_start..span.start as usize].chars().count() as u32;
        let end_char = line_start as u32 + span.end - span.start; //substring.rfind('\n').unwrap_or(span.end as usize) as u32; 

          if let NagaType::FunctionExpression(_, expr) = item {
              eprintln!("{:?}:{:?}-{:?}:{:?} {:?}:{:?} {:?}", line_number, start_char, line_number + line_count, end_char, position.line, position.character, expr); 
          }

          line_number <= position.line &&
              line_number + line_count >= position.line &&
              start_char <= position.character &&
              end_char >= position.character
      })
      .map(|(item, span, _)| (item, span))
      .collect::<Vec<(NagaType, naga::Span)>>()
  }

  fn direct_item<'a>(&'a self, sources: &FileSources, module: &'a naga::Module, position: &Position) -> Option<(NagaType, naga::Span)> {
    let items = self.intersecting_items(sources, module, position);

    items.iter()
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
      }).copied()
  }

  fn goto_span<'a>(&'a self, sources: &FileSources, module: &'a naga::Module, position: &Position) -> Option<naga::Span> {
    let (naga_type, span) = self.direct_item(&sources, &module, &position)?;

    eprintln!("call goto_span {:#?}", naga_type); 

    match naga_type {
        NagaType::FunctionExpression(_, naga::Expression::GlobalVariable(handle)) => Some(module.global_variables.get_span(*handle)),
        NagaType::FunctionExpression(_, naga::Expression::Constant(handle)) => Some(module.constants.get_span(*handle)),
        NagaType::FunctionExpression(_, naga::Expression::CallResult(handle)) => Some(module.functions.get_span(*handle)),
        NagaType::FunctionExpression(func, naga::Expression::Load { pointer }) => {
            let expression = &func.expressions[*pointer];

            match expression {
                naga::Expression::LocalVariable(handle) => Some(func.local_variables.get_span(*handle)),
                _ => None
            }
        }
        _ => None
    }
  }
}

#[tower_lsp::async_trait]
impl LanguageServer for WgslxLanguageServer {
  
  //--------------------------------------------------------------------------
  //
  //  Lifecycle
  //
  //--------------------------------------------------------------------------

  async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult, jsonrpc::Error> {
    eprintln!("Initializing {:#?}", params);
    
    Ok(InitializeResult {
      capabilities: ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
          TextDocumentSyncKind::FULL
        )),
        workspace: Some(WorkspaceServerCapabilities {
          file_operations: None,
          workspace_folders: Some(WorkspaceFoldersServerCapabilities {
            change_notifications: Some(OneOf::Left(true)),
            supported: Some(true), 
          })
        }),
        
        definition_provider: Some(OneOf::Left(true)),
        ..Default::default()
      }, 
      ..Default::default()
    })
  }

  async fn initialized(&self, _params: InitializedParams) {
    self.client.log_message(MessageType::INFO, "DID server initialized!").await;
  }

  async fn shutdown(&self) -> Result<(), jsonrpc::Error> {
    Ok(())
  }

  async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>, jsonrpc::Error> {
    eprintln!("Goto DEF"); 
    self.client.log_message(MessageType::INFO, format!("Request definition! {:#?}", params)).await;

    let uri = params.text_document_position_params.text_document.uri; 
    let sources = FileSources::new();
    let id = sources.visit(Path::new(uri.path()));
    let module = self.diagnostics(&sources, id.unwrap()).unwrap(); 
    let position = params.text_document_position_params.position;

    if let Some(span) = self.goto_span(&sources, &module, &position) {
      let id = span.file_id.unwrap() ; 
      let source = sources.get(id).unwrap().source(); 
      let location = span.location(source);

      let start = Position { line: location.line_number - 1, character: location.line_position - 1 };
      let end = Position { line: location.line_number - 1, character: location.line_position - 1 + location.length };

      let file = sources.get(id);
      let path = Url::from_file_path(file.unwrap().path()).unwrap();
      
      let response = GotoDefinitionResponse::Scalar(
        Location::new(path, Range::new(start, end)));

      return Ok(Some(response));   
    }

    Ok(None)
  }


  async fn did_open(&self, params: DidOpenTextDocumentParams) {
    self.client.log_message(MessageType::INFO, "file opened!").await; 
  }

  async fn did_change(&self, params: DidChangeTextDocumentParams) {
    self.client.log_message(MessageType::INFO, format!("changed! {}", params.text_document.uri)).await;

    let mut sources = FileSources::new();

    let id = sources.insert(params.text_document.uri.path(), &params.content_changes[0].text);

    let mut diagnostics_by_file = HashMap::<FileId, Vec<Diagnostic>>::new(); 

    if let Err(error) = self.diagnostics(&sources, id) {
     match error {
        Error::Parse(error) => {
        eprintln!("got error {:#?}", error); 
          let labels = error.labels();

          for (span, _label) in labels {
            let source = sources.get(id).unwrap().source(); 
            let location = span.location(&source);

            let start = Position { line: location.line_number - 1, character: location.line_position - 1 };
            let end = Position { line: location.line_number - 1, character: location.line_position - 1 + location.length };
            let diagnostics = diagnostics_by_file.entry(span.file_id.unwrap())
              .or_insert(Vec::new()); 
            
            diagnostics.push(Diagnostic::new_simple(
              Range::new(start, end),
              format!("[Parse] {}", error.emit_to_string_with_provider(&sources))
            ));         
          }    
        },
          Error::Validation(error) => {
            eprintln!("got error {:#?}", error); 
          // Naga validation errors include a vector of spans (with labels, such as "naga::Expression [8]")
          // for marking code, as well as the actual error message which to view in it's entirety requires traversing
          // errors sources
          //
          // For instance, for a function Foo with an expression pow(1, 2.), naga will return a
          // 
          //   ValidationError::Function = Function [6] 'Foo' is invalid:
          //     source: FunctionError::Expression =  Expression [8] is invalid
          //       source: ExpressionError::InvalidArgumentType = Argument [0] to Pow as expression [4] has an invalid type.
          // 
          // We ignore everything but the inner error, and associate it with the last span
          if let Some((span, _label)) = error.spans().last() {
            // Get the most inner error
            let text = sources.get(id).unwrap().source(); 

            let mut source: &dyn std::error::Error = &error.as_inner();
            while let Some(next) = std::error::Error::source(source) {
              source = next;
            }

            let location = span.location(text);
            let start = Position { line: location.line_number - 1, character: location.line_position - 1 };
            let end = Position { line: location.line_number - 1, character: location.line_position - 1 + location.length };

            // Append with the debug name of the enum to get a better sense of what the error is about
            // Should we remove everything after the parens? (e.g., InvalidArgumentType(Pow, 0, [4]))
            let mut message = format!("{:?}: ", source);

            message.push_str(&source.to_string());

            let diagnostics = diagnostics_by_file.entry(span.file_id.unwrap())
              .or_insert(Vec::new()); 
            
            diagnostics.push(Diagnostic::new_simple(
              Range::new(start, end),
              format!("[Validation] {}", message)
            ));         
          }
        }
      }
    }

    // Clear any old diagnostics for the current file
    self.client.publish_diagnostics(params.text_document.uri, Vec::new(), None).await; 

    for (file_id, diagnostics) in diagnostics_by_file.drain() {
      let file = sources.get(file_id);
      let path = Url::from_file_path(file.unwrap().path()).unwrap(); 
      
      self.client.publish_diagnostics(path, diagnostics, None).await; 
    }
  }

  async fn did_save(&self, params: DidSaveTextDocumentParams) {
    self.client.log_message(MessageType::INFO, "saved!").await;
  }
}

#[derive(Debug)]
enum Error {
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


#[tokio::main]
async fn main() {
  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let (service, socket) = LspService::new(|client| WgslxLanguageServer { client });
  Server::new(stdin, stdout, socket).serve(service).await;
}
