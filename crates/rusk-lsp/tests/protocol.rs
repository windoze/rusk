use futures::StreamExt;
use rusk_lsp::{RuskLanguageServer, RuskLspConfig};
use tower::Service;
use tower::ServiceExt;
use tower_lsp::jsonrpc;
use tower_lsp::lsp_types::{
    DidOpenTextDocumentParams, DocumentSymbolParams, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverContents, HoverParams, InitializeParams, MarkupKind, Position,
    PublishDiagnosticsParams, TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams,
    Url,
};
use tokio::time::{Duration, timeout};

fn temp_file_path(name: &str) -> std::path::PathBuf {
    use std::time::{SystemTime, UNIX_EPOCH};
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time is after unix epoch")
        .as_nanos();
    std::env::temp_dir().join(format!("rusk_lsp_test_{nanos}_{name}"))
}

async fn init_service(
    service: &mut tower_lsp::LspService<RuskLanguageServer>,
) -> tower_lsp::jsonrpc::Response {
    let params = InitializeParams::default();
    init_service_with_params(service, params).await
}

async fn init_service_with_params(
    service: &mut tower_lsp::LspService<RuskLanguageServer>,
    params: InitializeParams,
) -> tower_lsp::jsonrpc::Response {

    let req = jsonrpc::Request::build("initialize")
        .id(1i64)
        .params(serde_json::to_value(params).expect("serialize initialize"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("service ready")
        .call(req)
        .await
        .expect("initialize response");

    resp.expect("initialize must return a response")
}

async fn notify(
    service: &mut tower_lsp::LspService<RuskLanguageServer>,
    method: &'static str,
    params: serde_json::Value,
) {
    let req = jsonrpc::Request::build(method).params(params).finish();
    let resp = service
        .ready()
        .await
        .expect("service ready")
        .call(req)
        .await
        .expect("notify call");
    assert!(resp.is_none(), "notifications must not return responses");
}

#[tokio::test]
async fn publish_diagnostics_uses_utf16_ranges() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");

    notify(&mut service, "initialized", serde_json::json!({})).await;

    let path = temp_file_path("utf16_main.rusk");
    let uri = Url::from_file_path(&path).expect("file uri");
    let src = "fn main() {\n  let s = \"😀\"; #\n}\n";

    let params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };

    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(params).expect("serialize didOpen"),
    )
    .await;

    let msg = socket.next().await.expect("publishDiagnostics");
    assert_eq!(msg.method(), "textDocument/publishDiagnostics");

    let pd: PublishDiagnosticsParams = serde_json::from_value(
        msg.params()
            .cloned()
            .expect("publishDiagnostics params must exist"),
    )
    .expect("deserialize diagnostics");

    assert_eq!(pd.uri, uri);
    assert_eq!(pd.diagnostics.len(), 1);

    let d = &pd.diagnostics[0];
    assert!(
        d.message.contains("unexpected character"),
        "unexpected message: {}",
        d.message
    );

    // Line 1: `  let s = "😀"; #`
    // UTF-16 columns: emoji counts as 2 code units, so `#` starts at col 16.
    assert_eq!(d.range.start.line, 1);
    assert_eq!(d.range.start.character, 16);
    assert_eq!(d.range.end.line, 1);
    assert_eq!(d.range.end.character, 17);
}

#[tokio::test]
async fn opening_sysroot_file_without_entry_files_clears_diagnostics_instead_of_reanalyzing() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let root = temp_file_path("sysroot_workspace_root");
    std::fs::create_dir_all(root.join("sysroot/core")).expect("create sysroot/core dir");
    std::fs::write(
        root.join("sysroot/core/mod.rusk"),
        "mod prelude;\n",
    )
    .expect("write sysroot core mod");
    std::fs::write(
        root.join("sysroot/core/prelude.rusk"),
        "fn dummy() { () }\n",
    )
    .expect("write sysroot prelude");

    let init_params = InitializeParams {
        root_uri: Some(Url::from_file_path(&root).expect("root uri")),
        initialization_options: Some(serde_json::json!({
            "sysroot": root.join("sysroot").to_string_lossy().to_string(),
        })),
        ..InitializeParams::default()
    };

    let init_resp = init_service_with_params(&mut service, init_params).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    let sysroot_file = root.join("sysroot/core/mod.rusk");
    let uri = Url::from_file_path(&sysroot_file).expect("sysroot uri");

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: "mod prelude;\n".to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    let msg = timeout(Duration::from_secs(1), socket.next())
        .await
        .expect("publishDiagnostics timeout")
        .expect("publishDiagnostics message");

    assert_eq!(msg.method(), "textDocument/publishDiagnostics");

    let pd: PublishDiagnosticsParams =
        serde_json::from_value(msg.params().cloned().expect("params")).expect("pd");

    assert_eq!(pd.uri, uri);
    assert!(
        pd.diagnostics.is_empty(),
        "expected sysroot open to clear diagnostics, got: {:?}",
        pd.diagnostics
    );
}

#[tokio::test]
async fn overlay_modules_are_loaded_from_unsaved_buffers() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    let root = temp_file_path("overlay_mod_root");
    let main_path = root.join("main.rusk");
    let foo_path = root.join("foo.rusk");
    let main_uri = Url::from_file_path(&main_path).expect("main uri");
    let foo_uri = Url::from_file_path(&foo_path).expect("foo uri");

    // Open `foo.rusk` (unsaved) with a parse error.
    let foo_src = "#\n";
    let foo_open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: foo_uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: foo_src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(foo_open).expect("serialize foo open"),
    )
    .await;

    // Drain the first diagnostics publication (foo-as-entry).
    let _ = socket.next().await.expect("first publishDiagnostics");

    // Open `main.rusk` which references `mod foo;` — the loader should consult the overlay and
    // report the parse error in `foo.rusk`, not a "module not found" error in `main.rusk`.
    let main_src = "mod foo;\nfn main() { () }\n";
    let main_open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: main_uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: main_src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(main_open).expect("serialize main open"),
    )
    .await;

    let msg = socket.next().await.expect("second publishDiagnostics");
    let pd: PublishDiagnosticsParams =
        serde_json::from_value(msg.params().cloned().expect("params")).expect("pd");

    assert_eq!(pd.uri, foo_uri);
    assert_eq!(pd.diagnostics.len(), 1);
    assert!(
        pd.diagnostics[0].message.contains("unexpected character"),
        "unexpected message: {}",
        pd.diagnostics[0].message
    );
}

#[tokio::test]
async fn document_symbols_return_top_level_items() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    // Keep socket in scope to avoid blocking client->server notifications when the channel fills.
    let _socket = &mut socket;

    let path = temp_file_path("symbols.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "struct S { a: int }\nfn main() { () }\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    // Request document symbols.
    let req_params = DocumentSymbolParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let req = jsonrpc::Request::build("textDocument/documentSymbol")
        .id(2i64)
        .params(serde_json::to_value(req_params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("documentSymbol response")
        .expect("must return response");

    assert!(resp.is_ok(), "documentSymbol failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<tower_lsp::lsp_types::DocumentSymbolResponse> =
        serde_json::from_value(result).expect("decode result");

    let Some(tower_lsp::lsp_types::DocumentSymbolResponse::Nested(symbols)) = decoded else {
        panic!("expected Nested symbols, got {decoded:?}");
    };

    let names: Vec<String> = symbols.into_iter().map(|s| s.name).collect();
    assert!(
        names.contains(&"S".to_string()),
        "missing struct symbol: {names:?}"
    );
    assert!(
        names.contains(&"main".to_string()),
        "missing main symbol: {names:?}"
    );
}

#[tokio::test]
async fn document_symbols_include_doc_comment_detail() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    // Keep socket in scope to avoid blocking client->server notifications when the channel fills.
    let _socket = &mut socket;

    let path = temp_file_path("symbols_docs.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "/// Doc for S\nstruct S { a: int }\n\n/// Doc for main\nfn main() { () }\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    // Request document symbols.
    let req_params = DocumentSymbolParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let req = jsonrpc::Request::build("textDocument/documentSymbol")
        .id(20i64)
        .params(serde_json::to_value(req_params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("documentSymbol response")
        .expect("must return response");

    assert!(resp.is_ok(), "documentSymbol failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<tower_lsp::lsp_types::DocumentSymbolResponse> =
        serde_json::from_value(result).expect("decode result");

    let Some(tower_lsp::lsp_types::DocumentSymbolResponse::Nested(symbols)) = decoded else {
        panic!("expected Nested symbols, got {decoded:?}");
    };

    let sym_s = symbols
        .iter()
        .find(|s| s.name == "S")
        .expect("struct symbol");
    assert_eq!(sym_s.detail.as_deref(), Some("Doc for S"));

    let sym_main = symbols
        .iter()
        .find(|s| s.name == "main")
        .expect("main symbol");
    assert_eq!(sym_main.detail.as_deref(), Some("Doc for main"));
}

#[tokio::test]
async fn goto_definition_jumps_to_top_level_symbol() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    // Keep socket in scope to avoid blocking client->server notifications when the channel fills.
    let _socket = &mut socket;

    let path = temp_file_path("definition.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "fn foo() { () }\nfn main() { foo(); }\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    let position = tower_lsp::lsp_types::Position {
        line: 1,
        character: 12,
    };
    let params = GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/definition")
        .id(3i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("definition response")
        .expect("must return response");

    assert!(resp.is_ok(), "definition failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<GotoDefinitionResponse> =
        serde_json::from_value(result).expect("decode result");

    let Some(GotoDefinitionResponse::Scalar(loc)) = decoded else {
        panic!("expected Scalar location, got {decoded:?}");
    };

    assert_eq!(loc.uri, uri);
    assert_eq!(loc.range.start.line, 0);
    assert_eq!(loc.range.start.character, 3);
    assert_eq!(loc.range.end.line, 0);
    assert_eq!(loc.range.end.character, 6);
}

#[tokio::test]
async fn goto_definition_jumps_to_function_parameter() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    // Keep socket in scope to avoid blocking client->server notifications when the channel fills.
    let _socket = &mut socket;

    let path = temp_file_path("definition_param.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "fn foo(x: int) -> int {\n  x\n}\nfn main() { foo(1); }\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    // Position on the `x` in the function body: line 1, "  x".
    let position = tower_lsp::lsp_types::Position {
        line: 1,
        character: 2,
    };
    let params = GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/definition")
        .id(21i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("definition response")
        .expect("must return response");

    assert!(resp.is_ok(), "definition failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<GotoDefinitionResponse> =
        serde_json::from_value(result).expect("decode result");

    let Some(GotoDefinitionResponse::Scalar(loc)) = decoded else {
        panic!("expected Scalar location, got {decoded:?}");
    };

    assert_eq!(loc.uri, uri);
    assert_eq!(loc.range.start.line, 0);
    assert_eq!(loc.range.start.character, 7);
    assert_eq!(loc.range.end.line, 0);
    assert_eq!(loc.range.end.character, 8);
}

#[tokio::test]
async fn goto_definition_jumps_to_generic_parameter() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    // Keep socket in scope to avoid blocking client->server notifications when the channel fills.
    let _socket = &mut socket;

    let path = temp_file_path("definition_generic.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "fn id<T>(x: T) -> T {\n  let y: T = x;\n  y\n}\nfn main() { id::<int>(1); }\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    // Position on the `T` in `let y: T = x;` (line 1).
    let position = tower_lsp::lsp_types::Position {
        line: 1,
        character: 9,
    };
    let params = GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/definition")
        .id(22i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("definition response")
        .expect("must return response");

    assert!(resp.is_ok(), "definition failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<GotoDefinitionResponse> =
        serde_json::from_value(result).expect("decode result");

    let Some(GotoDefinitionResponse::Scalar(loc)) = decoded else {
        panic!("expected Scalar location, got {decoded:?}");
    };

    assert_eq!(loc.uri, uri);
    assert_eq!(loc.range.start.line, 0);
    assert_eq!(loc.range.start.character, 6);
    assert_eq!(loc.range.end.line, 0);
    assert_eq!(loc.range.end.character, 7);
}

#[tokio::test]
async fn goto_definition_jumps_to_struct_field() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    // Keep socket in scope to avoid blocking client->server notifications when the channel fills.
    let _socket = &mut socket;

    let path = temp_file_path("definition_struct_field.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "struct S { a: int, b: int }\nfn main() {\n  let s = S { a: 1, b: 2 };\n  s.a;\n}\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    // Position on the `a` in `s.a` (line 3, "  s.a;").
    let position = tower_lsp::lsp_types::Position {
        line: 3,
        character: 4,
    };
    let params = GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/definition")
        .id(23i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("definition response")
        .expect("must return response");

    assert!(resp.is_ok(), "definition failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<GotoDefinitionResponse> =
        serde_json::from_value(result).expect("decode result");

    let Some(GotoDefinitionResponse::Scalar(loc)) = decoded else {
        panic!("expected Scalar location, got {decoded:?}");
    };

    assert_eq!(loc.uri, uri);
    assert_eq!(loc.range.start.line, 0);
    assert_eq!(loc.range.start.character, 11);
    assert_eq!(loc.range.end.line, 0);
    assert_eq!(loc.range.end.character, 12);
}

#[tokio::test]
async fn goto_definition_jumps_to_inherent_method() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    // Keep socket in scope to avoid blocking client->server notifications when the channel fills.
    let _socket = &mut socket;

    let path = temp_file_path("definition_inherent_method.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "struct S { a: int }\nimpl S {\n  fn get() -> int { self.a }\n}\nfn main() {\n  let s = S { a: 1 };\n  s.get();\n}\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    // Position on the `get` in `s.get()` (line 6, "  s.get();").
    let position = tower_lsp::lsp_types::Position {
        line: 6,
        character: 5,
    };
    let params = GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/definition")
        .id(24i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("definition response")
        .expect("must return response");

    assert!(resp.is_ok(), "definition failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<GotoDefinitionResponse> =
        serde_json::from_value(result).expect("decode result");

    let Some(GotoDefinitionResponse::Scalar(loc)) = decoded else {
        panic!("expected Scalar location, got {decoded:?}");
    };

    assert_eq!(loc.uri, uri);
    assert_eq!(loc.range.start.line, 2);
    assert_eq!(loc.range.start.character, 5);
    assert_eq!(loc.range.end.line, 2);
    assert_eq!(loc.range.end.character, 8);
}

#[tokio::test]
async fn goto_definition_jumps_to_effect_interface_method() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    // Keep socket in scope to avoid blocking client->server notifications when the channel fills.
    let _socket = &mut socket;

    let path = temp_file_path("definition_effect_method.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "interface I {\n  fn m();\n}\nfn main() {\n  @I.m();\n}\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    // Position on the `m` in `@I.m()` (line 4, "  @I.m();").
    let position = tower_lsp::lsp_types::Position {
        line: 4,
        character: 5,
    };
    let params = GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/definition")
        .id(25i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("definition response")
        .expect("must return response");

    assert!(resp.is_ok(), "definition failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<GotoDefinitionResponse> =
        serde_json::from_value(result).expect("decode result");

    let Some(GotoDefinitionResponse::Scalar(loc)) = decoded else {
        panic!("expected Scalar location, got {decoded:?}");
    };

    assert_eq!(loc.uri, uri);
    assert_eq!(loc.range.start.line, 1);
    assert_eq!(loc.range.start.character, 5);
    assert_eq!(loc.range.end.line, 1);
    assert_eq!(loc.range.end.character, 6);
}

#[tokio::test]
async fn hover_shows_inferred_type_information() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    let path = temp_file_path("hover_type.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "fn main() -> int {\n  let x = 1;\n  x\n}\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    // Keep socket in scope to avoid blocking client->server notifications when the channel fills.
    let _socket = &mut socket;

    // Request hover on the `x` in the tail position (line 2, "  x").
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 2,
                character: 2,
            },
        },
        work_done_progress_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/hover")
        .id(30i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("hover response")
        .expect("must return response");

    assert!(resp.is_ok(), "hover failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<Hover> = serde_json::from_value(result).expect("decode result");

    let Some(hover) = decoded else {
        panic!("expected hover result, got None");
    };

    let hover_text = match hover.contents {
        HoverContents::Markup(markup) => {
            assert_eq!(markup.kind, MarkupKind::Markdown);
            markup.value
        }
        other => panic!("expected Markup hover contents, got: {other:?}"),
    };

    assert!(
        hover_text.contains("int"),
        "hover text should mention inferred type `int`, got: {hover_text:?}"
    );
}

#[tokio::test]
async fn hover_on_struct_literal_field_name_prefers_field_value_type() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    let path = temp_file_path("hover_struct_field.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "/// Key struct\nstruct Key { x: int }\nfn main() { let k = Key { x: 1 }; () }\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    // Keep socket in scope to avoid blocking client->server notifications when the channel fills.
    let _socket = &mut socket;

    // Hover on the `x` field name inside the struct literal `Key { x: 1 }`.
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 2,
                character: 26,
            },
        },
        work_done_progress_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/hover")
        .id(31i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("hover response")
        .expect("must return response");

    assert!(resp.is_ok(), "hover failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<Hover> = serde_json::from_value(result).expect("decode result");

    let Some(hover) = decoded else {
        panic!("expected hover result, got None");
    };

    let hover_text = match hover.contents {
        HoverContents::Markup(markup) => markup.value,
        other => panic!("expected Markup hover contents, got: {other:?}"),
    };

    assert!(
        hover_text.contains("int"),
        "hover on field name should show the field value type, got: {hover_text:?}"
    );
    assert!(
        !hover_text.contains("Key struct"),
        "field hover should not inherit outer struct docs, got: {hover_text:?}"
    );
}

#[tokio::test]
async fn hover_on_type_name_in_struct_literal_includes_doc_comment() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    let path = temp_file_path("hover_struct_type_name.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "/// Key struct\nstruct Key { x: int }\nfn main() { let k = Key { x: 1 }; () }\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    let _socket = &mut socket;

    // Hover on the `Key` type name in `Key { x: 1 }` (line 2).
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 2,
                character: 20,
            },
        },
        work_done_progress_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/hover")
        .id(32i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("hover response")
        .expect("must return response");

    assert!(resp.is_ok(), "hover failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<Hover> = serde_json::from_value(result).expect("decode result");

    let Some(hover) = decoded else {
        panic!("expected hover result, got None");
    };

    let hover_text = match hover.contents {
        HoverContents::Markup(markup) => markup.value,
        other => panic!("expected Markup hover contents, got: {other:?}"),
    };

    assert!(
        hover_text.contains("Key struct"),
        "type hover should include the doc comment for the definition, got: {hover_text:?}"
    );
}

#[tokio::test]
async fn hover_on_interface_name_in_generic_bounds_is_available() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    let path = temp_file_path("hover_generic_bound_iface.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "/// Eq docs\ninterface MyEq { readonly fn eq(other: Self) -> bool; }\nstruct Box<T: MyEq> { v: T }\nfn main() { () }\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    let _socket = &mut socket;

    // Hover on `MyEq` in `struct Box<T: MyEq> ...`.
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 2,
                character: 14,
            },
        },
        work_done_progress_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/hover")
        .id(33i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("hover response")
        .expect("must return response");

    assert!(resp.is_ok(), "hover failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<Hover> = serde_json::from_value(result).expect("decode result");

    let Some(hover) = decoded else {
        panic!("expected hover result, got None");
    };

    let hover_text = match hover.contents {
        HoverContents::Markup(markup) => markup.value,
        other => panic!("expected Markup hover contents, got: {other:?}"),
    };

    assert!(
        hover_text.contains("Eq docs"),
        "expected hover to include interface docs, got: {hover_text:?}"
    );
}

#[tokio::test]
async fn hover_on_qualified_interface_call_shows_symbol_info_instead_of_call_type() {
    let (mut service, mut socket) = tower_lsp::LspService::new(|client| {
        RuskLanguageServer::new(client, RuskLspConfig::default())
    });

    let init_resp = init_service(&mut service).await;
    assert!(init_resp.is_ok(), "initialize failed: {init_resp:?}");
    notify(&mut service, "initialized", serde_json::json!({})).await;

    let path = temp_file_path("hover_iface_qualified_call.rusk");
    let uri = Url::from_file_path(&path).expect("uri");
    let src = "/// MyHash docs\ninterface MyHash {\n    /// hash docs\n    readonly fn hash() -> int;\n}\n\nstruct Key { x: int }\n\nimpl MyHash for Key {\n    readonly fn hash() -> int { 7 }\n}\n\nfn main() -> int {\n    let k = Key { x: 1 };\n    MyHash::hash(k)\n}\n";

    let open = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rusk".to_string(),
            version: 1,
            text: src.to_string(),
        },
    };
    notify(
        &mut service,
        "textDocument/didOpen",
        serde_json::to_value(open).expect("serialize open"),
    )
    .await;

    let _socket = &mut socket;

    // Hover on `MyHash` in `MyHash::hash(k)`.
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 14,
                character: 5,
            },
        },
        work_done_progress_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/hover")
        .id(34i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("hover response")
        .expect("must return response");

    assert!(resp.is_ok(), "hover failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<Hover> = serde_json::from_value(result).expect("decode result");
    let Some(hover) = decoded else {
        panic!("expected hover result, got None");
    };
    let hover_text = match hover.contents {
        HoverContents::Markup(markup) => markup.value,
        other => panic!("expected Markup hover contents, got: {other:?}"),
    };
    assert!(
        hover_text.contains("interface MyHash"),
        "expected interface header, got: {hover_text:?}"
    );
    assert!(
        hover_text.contains("MyHash docs"),
        "expected interface docs, got: {hover_text:?}"
    );
    assert!(
        !hover_text.contains("```rusk\nint\n```"),
        "hover should not be only the call return type, got: {hover_text:?}"
    );

    // Hover on `hash` in `MyHash::hash(k)`.
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 14,
                character: 13,
            },
        },
        work_done_progress_params: Default::default(),
    };

    let req = jsonrpc::Request::build("textDocument/hover")
        .id(35i64)
        .params(serde_json::to_value(params).expect("serialize request"))
        .finish();

    let resp = service
        .ready()
        .await
        .expect("ready")
        .call(req)
        .await
        .expect("hover response")
        .expect("must return response");

    assert!(resp.is_ok(), "hover failed: {resp:?}");
    let result = resp.result().cloned().expect("result json");
    let decoded: Option<Hover> = serde_json::from_value(result).expect("decode result");
    let Some(hover) = decoded else {
        panic!("expected hover result, got None");
    };
    let hover_text = match hover.contents {
        HoverContents::Markup(markup) => markup.value,
        other => panic!("expected Markup hover contents, got: {other:?}"),
    };

    assert!(
        hover_text.contains("fn hash"),
        "expected method signature header, got: {hover_text:?}"
    );
    assert!(
        hover_text.contains("hash docs"),
        "expected method docs, got: {hover_text:?}"
    );
    assert!(
        !hover_text.contains("```rusk\nint\n```"),
        "hover should not be only the call return type, got: {hover_text:?}"
    );
}
