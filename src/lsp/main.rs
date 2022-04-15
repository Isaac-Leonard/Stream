#[path = "../ast.rs"]
pub mod ast;
#[path = "../compile.rs"]
pub mod compile;
pub mod completion;
#[path = "../errors.rs"]
pub mod errors;
mod jump_definition;
#[path = "../lexer.rs"]
mod lexer;
#[path = "../macros.rs"]
pub mod macros;
#[path = "../parser.rs"]
pub mod parser;
pub mod reference;
#[path = "../runner.rs"]
pub mod runner;
pub mod semantic_token;
#[path = "../settings.rs"]
pub mod settings;
#[path = "../shared.rs"]
pub mod shared;
use crate::runner::*;
use ast::*;
use dashmap::DashMap;
use errors::CompError;
use semantic_token::semantic_token_from_ast;
use semantic_token::ImCompleteSemanticToken;
use semantic_token::LEGEND_TYPE;
use settings::Settings;
use std::collections::HashMap;
use std::fs;

use ropey::Rope;
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    ast_map: DashMap<String, ExpEnvironment>,
    document_map: HashMap<String, Rope>,
    semantic_token_map: HashMap<String, Vec<ImCompleteSemanticToken>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),

                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("nrs".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.clone().into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                // definition: Some(GotoCapability::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        let semantic_tokens = || -> Option<Vec<SemanticToken>> { None }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let im_complete_tokens = self.semantic_token_map.get(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start as usize).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start as usize).ok()? as u32 - first;
                    let ret = Some(SemanticToken {
                        delta_line: line - pre_line,
                        delta_start: if start >= pre_start {
                            start - pre_start
                        } else {
                            start
                        },
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        Ok(Some(Vec::new()))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let ast = match self.ast_map.get(uri.path()) {
            Some(ast) => ast.clone(),
            None => {
                parse(create_settings(&TextDocumentItem {
                    uri: uri.clone(),
                    text: String::default(),
                    version: 0,
                }))
                .0
            }
        };

        let position = params.text_document_position_params.position;
        let file = fs::read_to_string(uri.path()).unwrap();
        let lines = calc_lines(&file);
        let line = lines[position.line as usize] as usize;
        let offset = line + position.character as usize;
        let span = match jump_definition::get_definition_of_expr(&ast, offset).1 {
            Some(span) => span,
            None => return Ok(None),
        };
        let line = lines.iter().position(|x| *x > span.1.start as i32).unwrap() - 1 as usize;
        let start_position = Position {
            line: line as u32 - 1,
            character: (span.1.start - (lines[line] as usize)) as u32 - 1,
        };
        let end_position = Position {
            line: line as u32 - 1,
            character: (span.1.end - (lines[line] as usize)) as u32 - 1,
        };
        let range = Range::new(start_position, end_position);

        Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
            uri, range,
        ))))
    }
    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let completions = || -> Option<Vec<CompletionItem>> {
            let rope = self.document_map.get(&uri.to_string())?;
            let ast = self.ast_map.get(&uri.to_string())?;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let completions = completion::completion(&ast, offset);
            let mut ret = Vec::with_capacity(completions.len());
            for (_, item) in completions {
                match item {
                    completion::ImCompleteCompletionItem::Variable(var) => {
                        ret.push(CompletionItem {
                            label: var.clone(),
                            insert_text: Some(var.clone()),
                            kind: Some(CompletionItemKind::VARIABLE),
                            detail: Some(var),
                            ..Default::default()
                        });
                    }
                    completion::ImCompleteCompletionItem::Function(name, args) => {
                        ret.push(CompletionItem {
                            label: name.clone(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            detail: Some(name.clone()),
                            insert_text: Some(format!(
                                "{}({})",
                                name,
                                args.iter()
                                    .enumerate()
                                    .map(|(index, item)| { format!("${{{}:{}}}", index + 1, item) })
                                    .collect::<Vec<_>>()
                                    .join(",")
                            )),
                            insert_text_format: Some(InsertTextFormat::SNIPPET),
                            ..Default::default()
                        });
                    }
                }
            }
            Some(ret)
        }();
        Ok(completions.map(CompletionResponse::Array))
    }
}
struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}
impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let (ast, errors, semantic_tokens) = parse(create_settings(&params));
        self.client
            .log_message(MessageType::INFO, format!("{:?}", errors))
            .await;
        let lines = calc_lines(&params.text);
        let diagnostics = errors
            .into_iter()
            .map(|item| {
                let message = item.get_msg(&lines);
                let pos = item.get_pos(&lines);
                let start_position = Position {
                    line: pos.0.line as u32,
                    character: pos.0.column as u32,
                };
                let end_position = Position {
                    line: pos.1.line as u32,
                    character: pos.1.column as u32,
                };
                Diagnostic::new_simple(Range::new(start_position, end_position), message)
            })
            .collect::<Vec<_>>();

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;

        self.ast_map.insert(params.uri.to_string(), ast);
        self.client
            .log_message(MessageType::INFO, &format!("{:?}", semantic_tokens))
            .await;
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        ast_map: DashMap::new(),
        document_map: HashMap::new(),
        semantic_token_map: HashMap::new(),
    })
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char;
    Some(Position::new(line as u32, column as u32))
}

fn parse(settings: Settings) -> (ExpEnvironment, Vec<CompError>, Vec<ImCompleteSemanticToken>) {
    let name = settings.input_name.clone();
    let mut files = parse_files(settings, HashMap::new());
    transform_files(&name, &mut files);
    (
        files
            .get(&name)
            .as_ref()
            .unwrap()
            .program
            .as_ref()
            .unwrap()
            .body
            .clone(),
        files.get(&name).as_ref().unwrap().errors.clone(),
        Vec::new(),
    )
}
fn create_settings(document: &TextDocumentItem) -> Settings {
    Settings {
        print_llvm: false,
        skip_optimizations: true,
        call_linker: false,
        input_name: document.uri.path().to_string(),
        object_name: String::new(),
    }
}
