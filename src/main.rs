
use std::cell::{RefCell, UnsafeCell};
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

impl WgslxLanguageServer {
    fn diagnostics(&self, provider: &FileSources, id: FileId) -> Result<(), Error>{
        let module = parse_module(provider, id)?;
        let mut validator = Validator::new(ValidationFlags::all(), Capabilities::all());
        validator.validate(&module)?; 
        Ok(())
  }
}

#[tokio::main]
async fn main() {
  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let (service, socket) = LspService::new(|client| WgslxLanguageServer { client });
  Server::new(stdin, stdout, socket).serve(service).await;
}
