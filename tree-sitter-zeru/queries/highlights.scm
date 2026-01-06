; Keywords
[
  "fn"
  "struct"
  "enum"
  "trait"
  "import"
  "const"
  "var"
  "return"
  "if"
  "else"
  "while"
  "for"
  "in"
  "match"
  "break"
  "continue"
  "as"
  "pub"
] @keyword

"default" @keyword

; Operators
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "="
  "+="
  "-="
  "*="
  "/="
  "%="
  "&="
  "|="
  "^="
  "<<="
  ">>="
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "&&"
  "||"
  "!"
  "&"
  "|"
  "^"
  "<<"
  ">>"
  ">>>"
  ".."
  "=>"
  "::"
] @operator

; Delimiters
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
  "<"
  ">"
] @punctuation.bracket

[
  ";"
  ","
  "."
  ":"
] @punctuation.delimiter

; Types
(primitive_type) @type.builtin

(generic_type
  name: (identifier) @type)

(array_type
  "Array" @type.builtin)

(struct_definition
  name: (identifier) @type)

(enum_definition
  name: (identifier) @type)

(trait_definition
  name: (identifier) @type)

(type_parameters
  (identifier) @type.parameter)

; Functions
(function_definition
  name: (identifier) @function)

(call_expression
  function: (identifier) @function.call)

(call_expression
  function: (scoped_identifier
    (identifier) @namespace
    (identifier) @function.call))

(method_call
  method: (identifier) @function.method.call)

(trait_method
  name: (identifier) @function)

; Variables & Parameters
(parameter
  name: (identifier) @variable.parameter)

"self" @variable.builtin

(variable_declaration
  name: (identifier) @variable)

(const_declaration
  name: (identifier) @constant)

; Fields
(struct_field
  name: (identifier) @field)

(struct_field_init
  name: (identifier) @field)

(field_access
  field: (identifier) @field)

; Enum variants
(enum_variant
  name: (identifier) @constant)

(scoped_identifier
  (identifier) @type
  (identifier) @constant)

; Literals
(integer_literal) @number
(float_literal) @number.float
(string_literal) @string
(char_literal) @character
(escape_sequence) @string.escape
(boolean_literal) @boolean
(none_literal) @constant.builtin

; Comments
(line_comment) @comment
(block_comment) @comment

; Special identifiers
((identifier) @variable.builtin
  (#match? @variable.builtin "^(self|Self)$"))

((identifier) @type.builtin
  (#match? @type.builtin "^(Option|Result|Vec|Array|String)$"))

; Module paths
(module_path
  (identifier) @namespace)

; Error handling
"?" @operator
"!" @operator
