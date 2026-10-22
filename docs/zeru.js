// Zeru highlighting for the book's code blocks, registered after the
// built-in languages and applied to every block marked `zeru`.
hljs.registerLanguage("zeru", function (hljs) {
  return {
    name: "Zeru",
    keywords: {
      keyword:
        "fn var const pub if else import while return for in break continue as " +
        "struct enum trait match default asm volatile",
      literal: "true false None",
      built_in:
        "Some Ok Err self print println eprint eprintln assert exit Vec String Array",
      type: "i8 i16 i32 i64 u8 u16 u32 u64 usize f32 f64 bool str",
    },
    contains: [
      hljs.C_LINE_COMMENT_MODE,
      { className: "string", begin: '"', end: '"', contains: [hljs.BACKSLASH_ESCAPE] },
      { className: "string", begin: "`", end: "`" },
      { className: "string", begin: "'(\\\\.|[^'\\\\])'" },
      {
        className: "number",
        begin: "\\b(0x[0-9a-fA-F_]+|0b[01_]+|0o[0-7_]+|[0-9][0-9_]*(\\.[0-9_]+)?([eE][+-]?[0-9_]+)?)",
      },
      { className: "title", begin: "\\bfn\\s+", end: "[A-Za-z_]\\w*", excludeBegin: true },
      { className: "type", begin: "\\b[A-Z]\\w*\\b" },
    ],
  };
});

document.querySelectorAll("code.language-zeru").forEach(function (block) {
  hljs.highlightBlock(block);
});
