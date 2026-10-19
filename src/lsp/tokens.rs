//! Semantic tokens: each token of the root file classed by what the checker
//! found it names, or by what kind of token it is, and each comment.

use std::collections::HashSet;

use lsp_types::{
    SemanticToken, SemanticTokenType, SemanticTokens, SemanticTokensParams, SemanticTokensResult,
};

use super::analysis::{Analysis, analyse};
use super::{Docs, Lines, document, read};
use crate::ast::{PRIMITIVES, StatementKind};
use crate::check::{Def, PRINTS};
use crate::items::{ItemKind, ModuleId};
use crate::lexer::KEYWORDS;
use crate::token::Token;

pub const LEGEND: [SemanticTokenType; 17] = [
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::TYPE,
    SemanticTokenType::STRUCT,
    SemanticTokenType::ENUM,
    SemanticTokenType::INTERFACE,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::METHOD,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::NUMBER,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::MACRO,
];

pub fn semantic_tokens(docs: &Docs, params: SemanticTokensParams) -> Option<SemanticTokensResult> {
    let (path, text) = document(docs, &params.text_document.uri)?;
    let data = analyse(&path, text, &|file| read(docs, file), encode);
    Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data,
    }))
}

fn encode(analysis: &mut Analysis) -> Vec<SemanticToken> {
    let root = analysis.root();
    let mut found = comments(analysis);
    let functions: HashSet<_> = analysis
        .items
        .items
        .iter()
        .filter(|item| item.kind == ItemKind::Fn)
        .map(|item| item.decl.span)
        .collect();
    let modules: HashSet<&str> = analysis.modules[ModuleId::ROOT.0 as usize]
        .program
        .statements
        .iter()
        .filter_map(|statement| match &statement.kind {
            StatementKind::Import { path, .. } => path.last().map(String::as_str),
            _ => None,
        })
        .collect();
    for (at, (token, span)) in root.tokens.iter().enumerate() {
        let next = root.tokens.get(at + 1).map(|(next, _)| next);
        let def = analysis.at(span.start).filter(|(name, _)| name == span);
        let kind = match (token, def) {
            (Token::Identifier(_), Some((_, def))) => match def {
                Def::Local(decl, _) if functions.contains(&decl) => SemanticTokenType::PARAMETER,
                Def::Local(..) => SemanticTokenType::VARIABLE,
                Def::Field(..) => SemanticTokenType::PROPERTY,
                Def::Variant(..) => SemanticTokenType::ENUM_MEMBER,
                Def::Item(def) => {
                    let item = analysis.items.item(def);
                    match item.kind {
                        ItemKind::Fn if item.owner.is_some() => SemanticTokenType::METHOD,
                        ItemKind::Fn => SemanticTokenType::FUNCTION,
                        ItemKind::Struct => SemanticTokenType::STRUCT,
                        ItemKind::Enum => SemanticTokenType::ENUM,
                        ItemKind::Trait => SemanticTokenType::INTERFACE,
                        ItemKind::Const => SemanticTokenType::VARIABLE,
                    }
                }
            },
            (Token::Identifier(name), None) => match name.as_str() {
                name if modules.contains(name) && next == Some(&Token::DoubleColon) => {
                    SemanticTokenType::NAMESPACE
                }
                name if PRIMITIVES.contains(&name) || ["void", "Vec", "Array"].contains(&name) => {
                    SemanticTokenType::TYPE
                }
                name if PRINTS.contains(&name) => SemanticTokenType::MACRO,
                "Some" | "Ok" | "Err" => SemanticTokenType::ENUM_MEMBER,
                _ => continue,
            },
            (Token::Int(_) | Token::Float(_), _) => SemanticTokenType::NUMBER,
            (Token::StringLit(_), _) => SemanticTokenType::STRING,
            (Token::Str, _) => SemanticTokenType::TYPE,
            (
                Token::LParen
                | Token::RParen
                | Token::LBrace
                | Token::RBrace
                | Token::LBracket
                | Token::RBracket
                | Token::Colon
                | Token::DoubleColon
                | Token::Semicolon
                | Token::Comma
                | Token::Dot
                | Token::Illegal(_)
                | Token::Eof,
                _,
            ) => continue,
            (token, _) if KEYWORDS.iter().any(|(_, word)| word == token) => {
                SemanticTokenType::KEYWORD
            }
            _ => SemanticTokenType::OPERATOR,
        };
        found.push((span.start, span.end, kind));
    }
    found.sort_by_key(|&(start, ..)| start);

    let lines = Lines::new(root.text);
    let mut data = Vec::new();
    let (mut line, mut column) = (0, 0);
    for (start, end, kind) in found {
        let token_type = LEGEND
            .iter()
            .position(|legend| *legend == kind)
            .unwrap_or(0) as u32;
        let mut from = start - root.start;
        for piece in root.text[from..end - root.start].split('\n') {
            let at = lines.position(from);
            from += piece.len() + 1;
            if piece.is_empty() {
                continue;
            }
            let delta_line = at.line - line;
            let delta_start = match delta_line {
                0 => at.character - column,
                _ => at.character,
            };
            data.push(SemanticToken {
                delta_line,
                delta_start,
                length: piece.encode_utf16().count() as u32,
                token_type,
                token_modifiers_bitset: 0,
            });
            (line, column) = (at.line, at.character);
        }
    }
    data
}

/// The comments of the root file, which lie between its tokens, as spans.
fn comments(analysis: &Analysis) -> Vec<(usize, usize, SemanticTokenType)> {
    let root = analysis.root();
    let ends = std::iter::once(root.start).chain(root.tokens.iter().map(|(_, span)| span.end));
    let starts = root
        .tokens
        .iter()
        .map(|(_, span)| span.start)
        .chain([root.start + root.text.len()]);
    let mut found = Vec::new();
    for (from, to) in ends.zip(starts) {
        let gap = &root.text[from - root.start..to - root.start];
        let mut at = 0;
        while let Some(start) = gap[at..].find('/').map(|start| at + start) {
            let end = match gap[start..].strip_prefix("/*") {
                Some(rest) => rest.find("*/").map_or(gap.len(), |end| start + end + 4),
                None => gap[start..].find('\n').map_or(gap.len(), |end| start + end),
            };
            found.push((from + start, from + end, SemanticTokenType::COMMENT));
            at = end;
        }
    }
    found
}
