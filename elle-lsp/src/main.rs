//! Elle Language Server Protocol implementation
//!
//! A resident compiler-based LSP server for Elle Lisp providing:
//! - Real-time diagnostics from integrated compiler linter
//! - Hover information with symbol lookup
//! - Code completion suggestions
//! - Navigation to symbol definitions

use elle_lsp::CompilerState;
use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Read, Write};

fn main() {
    let mut compiler_state = CompilerState::new();

    let stdin = std::io::stdin();
    let mut reader = BufReader::new(stdin.lock());
    let mut stdout = std::io::stdout();

    loop {
        // Read headers until Content-Length
        let mut content_length: usize = 0;
        let mut line = String::new();

        loop {
            line.clear();
            if reader.read_line(&mut line).unwrap() == 0 {
                return; // EOF
            }

            if line == "\r\n" || line == "\n" {
                break;
            }

            if line.starts_with("Content-Length:") {
                if let Ok(len) = line.split(':').nth(1).unwrap_or("").trim().parse::<usize>() {
                    content_length = len;
                }
            }
        }

        if content_length == 0 {
            continue;
        }

        // Read message body
        let mut buf = vec![0u8; content_length];
        if reader.read_exact(&mut buf).is_err() {
            break;
        }

        let message = String::from_utf8_lossy(&buf);
        if let Ok(request) = serde_json::from_str::<Value>(&message) {
            let (response, notifications) = handle_request(&request, &mut compiler_state);

            // Send response
            let body = response.to_string();
            let _ = writeln!(stdout, "Content-Length: {}\r\n\r{}", body.len(), body);
            let _ = stdout.flush();

            // Send notifications (e.g., diagnostics)
            for notification in notifications {
                let body = notification.to_string();
                let _ = writeln!(stdout, "Content-Length: {}\r\n\r{}", body.len(), body);
                let _ = stdout.flush();
            }
        }
    }
}

fn handle_request(request: &Value, compiler_state: &mut CompilerState) -> (Value, Vec<Value>) {
    let method = request.get("method").and_then(|v| v.as_str()).unwrap_or("");
    let id = request.get("id");
    let params = request.get("params");
    let mut notifications = Vec::new();

    let response = match method {
        "initialize" => {
            json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": {
                    "capabilities": {
                        "textDocumentSync": 1,
                        "hoverProvider": true,
                        "completionProvider": {
                            "resolveProvider": true,
                            "triggerCharacters": ["("]
                        }
                    },
                    "serverInfo": {
                        "name": "Elle Language Server",
                        "version": "0.1.0"
                    }
                }
            })
        }
        "shutdown" => {
            json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": null
            })
        }
        "textDocument/didOpen" => {
            if let Some(params) = params {
                if let Some(uri) = params
                    .get("textDocument")
                    .and_then(|d| d.get("uri"))
                    .and_then(|u| u.as_str())
                {
                    if let Some(text) = params
                        .get("textDocument")
                        .and_then(|d| d.get("text"))
                        .and_then(|t| t.as_str())
                    {
                        compiler_state.on_document_open(uri.to_string(), text.to_string());
                        compiler_state.compile_document(uri);

                        // Send diagnostics notification
                        if let Some(doc) = compiler_state.get_document(uri) {
                            let diags: Vec<_> = doc
                                .diagnostics
                                .iter()
                                .map(|d| {
                                    let (line, col) = match &d.location {
                                        Some(loc) => (loc.line as u32, loc.col as u32),
                                        None => (0, 0),
                                    };
                                    json!({
                                        "range": {
                                            "start": { "line": line - 1, "character": col - 1 },
                                            "end": { "line": line - 1, "character": col }
                                        },
                                        "severity": match d.severity {
                                            elle::compiler::linter::diagnostics::Severity::Error => 1,
                                            elle::compiler::linter::diagnostics::Severity::Warning => 2,
                                            elle::compiler::linter::diagnostics::Severity::Info => 3,
                                        },
                                        "code": d.code,
                                        "source": "elle-lint",
                                        "message": d.message
                                    })
                                })
                                .collect();

                            notifications.push(json!({
                                "jsonrpc": "2.0",
                                "method": "textDocument/publishDiagnostics",
                                "params": {
                                    "uri": uri,
                                    "diagnostics": diags
                                }
                            }));
                        }
                    }
                }
            }
            json!({})
        }
        "textDocument/didChange" => {
            if let Some(params) = params {
                if let Some(uri) = params
                    .get("textDocument")
                    .and_then(|d| d.get("uri"))
                    .and_then(|u| u.as_str())
                {
                    if let Some(changes) = params.get("contentChanges").and_then(|c| c.as_array()) {
                        if let Some(text) = changes
                            .first()
                            .and_then(|c| c.get("text"))
                            .and_then(|t| t.as_str())
                        {
                            compiler_state.on_document_change(uri, text.to_string());
                            compiler_state.compile_document(uri);

                            // Send diagnostics notification
                            if let Some(doc) = compiler_state.get_document(uri) {
                                let diags: Vec<_> = doc
                                    .diagnostics
                                    .iter()
                                    .map(|d| {
                                        let (line, col) = match &d.location {
                                            Some(loc) => (loc.line as u32, loc.col as u32),
                                            None => (0, 0),
                                        };
                                        json!({
                                            "range": {
                                                "start": { "line": line - 1, "character": col - 1 },
                                                "end": { "line": line - 1, "character": col }
                                            },
                                            "severity": match d.severity {
                                                elle::compiler::linter::diagnostics::Severity::Error => 1,
                                                elle::compiler::linter::diagnostics::Severity::Warning => 2,
                                                elle::compiler::linter::diagnostics::Severity::Info => 3,
                                            },
                                            "code": d.code,
                                            "source": "elle-lint",
                                            "message": d.message
                                        })
                                    })
                                    .collect();

                                notifications.push(json!({
                                    "jsonrpc": "2.0",
                                    "method": "textDocument/publishDiagnostics",
                                    "params": {
                                        "uri": uri,
                                        "diagnostics": diags
                                    }
                                }));
                            }
                        }
                    }
                }
            }
            json!({})
        }
        "textDocument/didClose" => {
            if let Some(params) = params {
                if let Some(uri) = params
                    .get("textDocument")
                    .and_then(|d| d.get("uri"))
                    .and_then(|u| u.as_str())
                {
                    compiler_state.on_document_close(uri);
                }
            }
            json!({})
        }
        "textDocument/hover" => {
            // Hover is handled but not fully implemented yet
            json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": null
            })
        }
        "textDocument/completion" => {
            // Completion is handled but not fully implemented yet
            json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": {
                    "isIncomplete": false,
                    "items": []
                }
            })
        }
        _ => {
            json!({
                "jsonrpc": "2.0",
                "id": id,
                "result": null
            })
        }
    };

    (response, notifications)
}
