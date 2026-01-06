/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "zeru",

  extras: ($) => [/\s/, $.line_comment, $.block_comment],

  word: ($) => $.identifier,

  conflicts: ($) => [
    [$.generic_type, $.identifier],
    [$._simple_type, $.identifier],
    [$.struct_literal, $.identifier],
  ],

  precedences: ($) => [
    [
      "unary",
      "cast",
      "multiplicative",
      "additive",
      "shift",
      "relational",
      "equality",
      "bitand",
      "bitxor",
      "bitor",
      "and",
      "or",
    ],
  ],

  rules: {
    source_file: ($) => repeat($._definition),

    _definition: ($) =>
      choice(
        $.function_definition,
        $.struct_definition,
        $.enum_definition,
        $.trait_definition,
        $.import_statement,
        $.const_declaration
      ),

    // ============
    // Comments
    // ============
    line_comment: ($) => token(seq("//", /.*/)),
    block_comment: ($) => token(seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/")),

    // ============
    // Imports
    // ============
    import_statement: ($) =>
      seq(
        "import",
        $.module_path,
        optional(seq("::", "{", commaSep1($.identifier), "}")),
        optional(";")
      ),

    module_path: ($) => sep1($.identifier, "."),

    // ============
    // Constants
    // ============
    const_declaration: ($) =>
      seq(
        "const",
        field("name", $.identifier),
        ":",
        field("type", $._type),
        "=",
        field("value", $._expression),
        ";"
      ),

    // ============
    // Functions
    // ============
    function_definition: ($) =>
      seq(
        optional("pub"),
        "fn",
        field("name", $.identifier),
        optional($.type_parameters),
        $.parameter_list,
        optional(field("return_type", $._type)),
        $.block
      ),

    parameter_list: ($) => seq("(", commaSep($.parameter), ")"),

    parameter: ($) =>
      seq(
        optional("var"),
        field("name", choice($.identifier, "self")),
        optional(seq(":", field("type", $._type)))
      ),

    type_parameters: ($) => seq("<", commaSep1($.identifier), ">"),

    // ============
    // Structs
    // ============
    struct_definition: ($) =>
      seq(
        optional("pub"),
        "struct",
        field("name", $.identifier),
        optional($.type_parameters),
        "{",
        repeat(choice($.struct_field, $.function_definition)),
        "}"
      ),

    struct_field: ($) =>
      seq(field("name", $.identifier), ":", field("type", $._type), optional(",")),

    // ============
    // Enums
    // ============
    enum_definition: ($) =>
      seq(
        optional("pub"),
        "enum",
        field("name", $.identifier),
        "{",
        commaSep($.enum_variant),
        optional(","),
        "}"
      ),

    enum_variant: ($) =>
      seq(field("name", $.identifier), optional(seq("(", commaSep($._type), ")"))),

    // ============
    // Traits
    // ============
    trait_definition: ($) =>
      seq("trait", field("name", $.identifier), "{", repeat($.trait_method), "}"),

    trait_method: ($) =>
      seq(
        "fn",
        field("name", $.identifier),
        $.parameter_list,
        optional(field("return_type", $._type)),
        choice($.block, ";")
      ),

    // ============
    // Types
    // ============
    _type: ($) =>
      choice(
        $._simple_type,
        $.pointer_type,
        $.optional_type,
        $.result_type,
        $.slice_type,
        $.tuple_type
      ),

    _simple_type: ($) =>
      choice($.primitive_type, $.identifier, $.generic_type, $.array_type),

    primitive_type: ($) =>
      choice(
        "i8", "i16", "i32", "i64",
        "u8", "u16", "u32", "u64",
        "f32", "f64",
        "bool", "str", "void"
      ),

    generic_type: ($) =>
      prec(1, seq(field("name", $.identifier), "<", commaSep1($._type), ">")),

    array_type: ($) =>
      seq("Array", "<", field("element", $._type), ",", field("size", $.integer_literal), ">"),

    pointer_type: ($) => seq("*", $._simple_type),
    optional_type: ($) => seq($._simple_type, "?"),
    result_type: ($) => seq($._simple_type, "!"),
    slice_type: ($) => seq("&", "[", $._type, "]"),
    tuple_type: ($) => seq("(", commaSep1($._type), ")"),

    // ============
    // Statements
    // ============
    block: ($) => seq("{", repeat($._statement), "}"),

    _statement: ($) =>
      choice(
        $.variable_declaration,
        $.const_declaration,
        $.assignment_statement,
        $.return_statement,
        $.if_statement,
        $.while_statement,
        $.for_in_statement,
        $.break_statement,
        $.continue_statement,
        $.expression_statement
      ),

    variable_declaration: ($) =>
      seq(
        "var",
        field("name", $.identifier),
        optional(seq(":", field("type", $._type))),
        optional(seq("=", field("value", $._expression))),
        ";"
      ),

    assignment_statement: ($) =>
      seq(
        field("target", $._lvalue),
        field("operator", choice("=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=")),
        field("value", $._expression),
        ";"
      ),

    _lvalue: ($) =>
      choice(
        $.identifier,
        $.field_access,
        $.index_expression,
        $.deref_expression
      ),

    return_statement: ($) => seq("return", optional($._expression), ";"),
    break_statement: ($) => seq("break", ";"),
    continue_statement: ($) => seq("continue", ";"),

    if_statement: ($) =>
      seq(
        "if",
        field("condition", $._expression),
        field("consequence", $.block),
        repeat($.else_if_clause),
        optional($.else_clause)
      ),

    else_if_clause: ($) =>
      seq("else", "if", field("condition", $._expression), field("consequence", $.block)),

    else_clause: ($) => seq("else", field("body", $.block)),

    while_statement: ($) =>
      seq("while", field("condition", $._expression), field("body", $.block)),

    for_in_statement: ($) =>
      seq(
        "for",
        field("element", $.identifier),
        "in",
        field("iterable", $._expression),
        field("body", $.block)
      ),

    expression_statement: ($) => seq($._expression, ";"),

    // ============
    // Expressions
    // ============
    _expression: ($) =>
      choice(
        $.identifier,
        $.integer_literal,
        $.float_literal,
        $.string_literal,
        $.char_literal,
        $.boolean_literal,
        $.none_literal,
        $.array_literal,
        $.tuple_expression,
        $.struct_literal,
        $.binary_expression,
        $.unary_expression,
        $.deref_expression,
        $.ref_expression,
        $.call_expression,
        $.method_call,
        $.field_access,
        $.index_expression,
        $.cast_expression,
        $.match_expression,
        $.parenthesized_expression,
        $.range_expression,
        $.scoped_identifier
      ),

    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    binary_expression: ($) =>
      choice(
        prec.left("or", seq($._expression, "||", $._expression)),
        prec.left("and", seq($._expression, "&&", $._expression)),
        prec.left("bitor", seq($._expression, "|", $._expression)),
        prec.left("bitxor", seq($._expression, "^", $._expression)),
        prec.left("bitand", seq($._expression, "&", $._expression)),
        prec.left("equality", seq($._expression, choice("==", "!="), $._expression)),
        prec.left("relational", seq($._expression, choice("<", ">", "<=", ">="), $._expression)),
        prec.left("shift", seq($._expression, choice("<<", ">>", ">>>"), $._expression)),
        prec.left("additive", seq($._expression, choice("+", "-"), $._expression)),
        prec.left("multiplicative", seq($._expression, choice("*", "/", "%"), $._expression))
      ),

    unary_expression: ($) => prec.right("unary", seq(choice("-", "!"), $._expression)),
    deref_expression: ($) => prec.right("unary", seq("*", $._expression)),
    ref_expression: ($) => prec.right("unary", seq("&", $._expression)),

    call_expression: ($) =>
      prec(15, seq(
        field("function", choice($.identifier, $.scoped_identifier)),
        optional(seq("::", "<", commaSep1($._type), ">")),
        $.argument_list
      )),

    method_call: ($) =>
      prec(16, seq(
        field("object", $._expression),
        ".",
        field("method", $.identifier),
        $.argument_list
      )),

    field_access: ($) =>
      prec(16, seq(
        field("object", $._expression),
        ".",
        field("field", choice($.identifier, $.integer_literal))
      )),

    index_expression: ($) =>
      prec(16, seq(field("array", $._expression), "[", field("index", $._expression), "]")),

    cast_expression: ($) => prec.left("cast", seq($._expression, "as", $._type)),

    argument_list: ($) => seq("(", commaSep($._expression), ")"),

    scoped_identifier: ($) => prec(1, seq($.identifier, "::", $.identifier)),

    range_expression: ($) => prec.left(1, seq($._expression, "..", $._expression)),

    // ============
    // Match
    // ============
    match_expression: ($) =>
      seq("match", field("value", $._expression), "{", repeat($.match_arm), "}"),

    match_arm: ($) =>
      seq(field("pattern", $.match_pattern), "=>", field("value", $._expression), optional(",")),

    match_pattern: ($) =>
      choice(
        "default",
        "_",
        $.scoped_identifier,
        seq($.scoped_identifier, "(", commaSep($.identifier), ")"),
        $.integer_literal,
        $.string_literal,
        $.boolean_literal
      ),

    // ============
    // Literals
    // ============
    array_literal: ($) =>
      choice(
        seq("[", commaSep($._expression), "]"),
        seq("[", $._expression, ";", $.integer_literal, "]")
      ),

    tuple_expression: ($) => seq("(", $._expression, ",", commaSep($._expression), ")"),

    struct_literal: ($) =>
      prec(-1, seq(field("type", $.identifier), "{", commaSep($.struct_field_init), "}")),

    struct_field_init: ($) =>
      seq(field("name", $.identifier), ":", field("value", $._expression)),

    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    integer_literal: ($) =>
      token(choice(/0x[0-9a-fA-F]+/, /0b[01]+/, /0o[0-7]+/, /[0-9]+/)),

    float_literal: ($) => /[0-9]+\.[0-9]+/,

    string_literal: ($) => seq('"', repeat(choice(/[^"\\]+/, $.escape_sequence)), '"'),

    escape_sequence: ($) =>
      token.immediate(seq("\\", choice("n", "t", "r", "\\", '"', "0", /x[0-9a-fA-F]{2}/))),

    char_literal: ($) => seq("'", choice(/[^'\\]/, $.escape_sequence), "'"),

    boolean_literal: ($) => choice("true", "false"),

    none_literal: ($) => "None",
  },
});

// Helper functions
function commaSep(rule) {
  return optional(commaSep1(rule));
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
