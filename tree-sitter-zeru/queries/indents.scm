; Indent rules for Zeru

[
  (block)
  (struct_definition)
  (enum_definition)
  (trait_definition)
  (match_expression)
  (array_literal)
] @indent.begin

[
  "}"
  "]"
] @indent.end

[
  "{"
  "["
] @indent.branch
