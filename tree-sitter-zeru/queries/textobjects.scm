; Text objects for Zeru (used by nvim-treesitter-textobjects)

; Function text objects
(function_definition) @function.outer
(function_definition
  (block) @function.inner)

; Class/struct text objects
(struct_definition) @class.outer
(struct_definition
  "{" . (_)* @class.inner . "}")

; Parameter text objects
(parameter) @parameter.outer
(parameter
  name: (identifier) @parameter.inner)

; Conditional text objects
(if_statement) @conditional.outer
(if_statement
  consequence: (block) @conditional.inner)

; Loop text objects
(while_statement) @loop.outer
(while_statement
  body: (block) @loop.inner)

(for_in_statement) @loop.outer
(for_in_statement
  body: (block) @loop.inner)

; Comment text objects
(line_comment) @comment.outer
(block_comment) @comment.outer

; Block text objects
(block) @block.outer
(block
  "{" . (_)* @block.inner . "}")

; Call text objects
(call_expression) @call.outer
(call_expression
  (argument_list) @call.inner)

; Return statement
(return_statement) @return.outer
(return_statement
  (_) @return.inner)
