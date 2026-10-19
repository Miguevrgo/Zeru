//! The language server driven as an editor drives it: open a small program
//! with one error, then ask about the rest of it.

use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

use serde_json::{Value, json};

const PROGRAM: &str = "struct Point {
    x: i32,
    y: i32,

    fn sum(self) i32 {
        return self.x + self.y;
    }
}

// The one error: a dot with no name after it.
fn main() {
    var v: Vec<i32> = Vec.new();
    var p = Point { x: 1, y: 2 };
    v.push(p.sum());
    v.
}
";

const URI: &str = "file:///tmp/zeru-lsp-test/main.zr";

struct Server {
    child: Child,
    input: ChildStdin,
    output: BufReader<ChildStdout>,
    next: i64,
}

impl Server {
    fn start() -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_zeru"))
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("zeru lsp starts");
        let input = child.stdin.take().unwrap();
        let output = BufReader::new(child.stdout.take().unwrap());
        Self {
            child,
            input,
            output,
            next: 0,
        }
    }

    fn send(&mut self, message: Value) {
        let body = message.to_string();
        write!(self.input, "Content-Length: {}\r\n\r\n{body}", body.len()).unwrap();
        self.input.flush().unwrap();
    }

    fn receive(&mut self) -> Value {
        let mut length = 0;
        loop {
            let mut line = String::new();
            self.output.read_line(&mut line).unwrap();
            match line.trim().strip_prefix("Content-Length: ") {
                Some(value) => length = value.parse().unwrap(),
                None if line.trim().is_empty() => break,
                None => {}
            }
        }
        let mut body = vec![0; length];
        self.output.read_exact(&mut body).unwrap();
        serde_json::from_slice(&body).unwrap()
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(json!({ "jsonrpc": "2.0", "method": method, "params": params }));
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        self.next += 1;
        let id = self.next;
        self.send(json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params }));
        loop {
            let message = self.receive();
            if message["id"] == id {
                return message["result"].clone();
            }
        }
    }

    fn notification(&mut self, method: &str) -> Value {
        loop {
            let message = self.receive();
            if message["method"] == method {
                return message["params"].clone();
            }
        }
    }

    fn at(&mut self, method: &str, line: u32, character: u32) -> Value {
        let position = json!({ "line": line, "character": character });
        let params = json!({ "textDocument": { "uri": URI }, "position": position });
        self.request(method, params)
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        self.request("shutdown", Value::Null);
        self.notify("exit", Value::Null);
        self.child.wait().unwrap();
    }
}

#[test]
fn answers_about_a_program_with_an_error() {
    let mut server = Server::start();
    let initialized = server.request("initialize", json!({ "capabilities": {} }));
    let legend = initialized["capabilities"]["semanticTokensProvider"]["legend"]["tokenTypes"]
        .as_array()
        .unwrap()
        .clone();
    server.notify("initialized", json!({}));
    let document = json!({ "uri": URI, "languageId": "zeru", "version": 1, "text": PROGRAM });
    server.notify("textDocument/didOpen", json!({ "textDocument": document }));

    let published = server.notification("textDocument/publishDiagnostics");
    let diagnostics = published["diagnostics"].as_array().unwrap();
    assert_eq!(diagnostics.len(), 1, "{published}");
    assert_eq!(diagnostics[0]["message"], "Expected a field name after '.'");
    assert_eq!(diagnostics[0]["range"]["start"]["line"], 15);

    let hover = server.at("textDocument/hover", 13, 11);
    assert!(
        hover["contents"]["value"]
            .as_str()
            .unwrap()
            .contains("var p: Point"),
        "{hover}"
    );
    let hover = server.at("textDocument/hover", 13, 14);
    assert!(
        hover["contents"]["value"]
            .as_str()
            .unwrap()
            .contains("fn sum(self) i32"),
        "{hover}"
    );

    let definition = server.at("textDocument/definition", 13, 14);
    assert_eq!(definition["uri"], URI);
    assert_eq!(
        definition["range"]["start"],
        json!({ "line": 4, "character": 7 })
    );

    let completion = server.at("textDocument/completion", 14, 6);
    let labels: Vec<&str> = completion
        .as_array()
        .unwrap()
        .iter()
        .map(|item| item["label"].as_str().unwrap())
        .collect();
    assert!(
        labels.contains(&"push") && labels.contains(&"len"),
        "{labels:?}"
    );

    let symbols = server.request(
        "textDocument/documentSymbol",
        json!({ "textDocument": { "uri": URI } }),
    );
    let names = |symbols: &Value| -> Vec<String> {
        let symbols = symbols.as_array().unwrap().iter();
        symbols
            .map(|s| s["name"].as_str().unwrap().into())
            .collect()
    };
    assert_eq!(names(&symbols), ["Point", "main"]);
    assert_eq!(names(&symbols[0]["children"]), ["x", "y", "sum"]);

    let tokens = server.request(
        "textDocument/semanticTokens/full",
        json!({ "textDocument": { "uri": URI } }),
    );
    let data: Vec<u64> = tokens["data"]
        .as_array()
        .unwrap()
        .iter()
        .map(|n| n.as_u64().unwrap())
        .collect();
    let mut classed = Vec::new();
    let (mut line, mut column) = (0, 0);
    for token in data.chunks(5) {
        line += token[0];
        column = if token[0] == 0 {
            column + token[1]
        } else {
            token[1]
        };
        classed.push(((line, column), legend[token[3] as usize].clone()));
    }
    let class = |line, column| {
        let found = classed.iter().find(|(at, _)| *at == (line, column));
        found.map(|(_, kind)| kind.as_str().unwrap().to_string())
    };
    assert_eq!(class(0, 7).as_deref(), Some("struct"));
    assert_eq!(class(1, 4).as_deref(), Some("property"));
    assert_eq!(class(4, 7).as_deref(), Some("method"));
    assert_eq!(class(9, 0).as_deref(), Some("comment"));
    assert_eq!(class(11, 8).as_deref(), Some("variable"));
    assert_eq!(class(11, 11).as_deref(), Some("type"));
    assert_eq!(class(12, 12).as_deref(), Some("struct"));
}
