use std::path::{Path};
pub use file_sources::FileSources;
use module::Module;
use module::definition::Definition;
use module::format::format_type;
use module::search_position::SearchPosition;
use naga::front::wgsl::source_provider::{SourceProvider};
use tower_lsp::{lsp_types::*, jsonrpc};
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct WgslxLanguageServer {
  client: Client,
}

mod file_sources;
mod module; 


impl WgslxLanguageServer {

  fn do_hover(&self, sources: &FileSources, pos: SearchPosition) -> Option<Hover> {
    let module = Module::from(sources, pos.file_id).ok()?;
    let definition = module.find_declaration_at(sources, pos)?;
    let ty_handle = definition.get_type(module.as_inner());
    let ty = &module.as_inner().types[ty_handle]; 
    
    let contents = format_type(module.as_inner(), ty); 
    let hover = Hover {
      contents: HoverContents::Scalar(MarkedString::String(contents)),
      range: None
    }; 
    
    Some(hover)
  }

  fn goto(&self, sources: &FileSources, pos: SearchPosition) -> Option<GotoDefinitionResponse> {
    let module = Module::from(sources, pos.file_id).ok()?;
    let definition = module.find_definition_at(sources, pos)?;
    let span = definition.get_span(&module); //module.find_span_at(sources, pos)?;

    eprintln!("Got span {:?}", span); 

    // We may get back an empty span, created with Span::default(), e.g., in the case of a standard type
    // being returned from the types Arena
    let id = span.file_id?; 
    let source = sources.get(id).unwrap().source(); 
    let location = span.location(source);

    let start = Position { line: location.line_number - 1, character: location.line_position - 1 };
    let end = Position { line: location.line_number - 1, character: location.line_position - 1 + location.length };

    let file = sources.get(id);
    let path = Url::from_file_path(file.unwrap().path()).unwrap();
    
    Some(GotoDefinitionResponse::Scalar(Location::new(path, Range::new(start, end))))
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
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
          inter_file_dependencies: true,
          ..Default::default()
        })),
        position_encoding: Some(PositionEncodingKind::UTF8),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
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

  async fn hover(&self, params: HoverParams) -> Result<Option<Hover>, jsonrpc::Error> {
    eprintln!("{:?}", params);

    let uri = params.text_document_position_params.text_document.uri; 
    let sources = file_sources::FileSources::new();
    let id = sources.visit(Path::new(uri.path())).unwrap();
    let pos = SearchPosition::new(&sources, params.text_document_position_params.position, id);
    let hover = self.do_hover(&sources, pos);

    Ok(hover)
  }

  async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>, jsonrpc::Error> {
    self.client.log_message(MessageType::INFO, format!("Request definition! {:#?}", params)).await;

    let uri = params.text_document_position_params.text_document.uri; 
    let sources = FileSources::new();
    let id = sources.visit(uri.path()).unwrap();
    let pos = SearchPosition::new(&sources, params.text_document_position_params.position, id);
    let response = self.goto(&sources, pos);
    
    Ok(response)
  }

  async fn did_open(&self, params: DidOpenTextDocumentParams) {
    self.client.log_message(MessageType::INFO, "file opened!").await; 
  }

  async fn did_change(&self, params: DidChangeTextDocumentParams) {
    self.client.log_message(MessageType::INFO, format!("changed! {}", params.text_document.uri)).await;

    let mut sources = FileSources::new();
    let id = sources.insert(params.text_document.uri.path(), &params.content_changes[0].text);
    let diagnostics = Module::diagnostics(&sources, id)
      .into_iter()
      .filter(|diagnostic| diagnostic.span.file_id == Some(id))
      .map(|diagnostic| diagnostic.as_lsp(&sources))
      .collect();

    self.client.publish_diagnostics(params.text_document.uri, diagnostics, None).await; 
  }

  async fn did_save(&self, params: DidSaveTextDocumentParams) {
    self.client.log_message(MessageType::INFO, "saved!").await;
  }
}

#[tokio::main]
async fn main() {
  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let (service, socket) = LspService::new(|client| WgslxLanguageServer { client });
  Server::new(stdin, stdout, socket).serve(service).await;
}
