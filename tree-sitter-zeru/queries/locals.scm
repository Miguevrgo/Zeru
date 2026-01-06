; Scopes
(block) @local.scope
(function_definition) @local.scope
(if_statement) @local.scope
(while_statement) @local.scope
(for_in_statement) @local.scope

; Definitions
(function_definition
  name: (identifier) @local.definition.function)

(parameter
  name: (identifier) @local.definition.parameter)

(variable_declaration
  name: (identifier) @local.definition.var)

(const_declaration
  name: (identifier) @local.definition.constant)

(struct_definition
  name: (identifier) @local.definition.type)

(enum_definition
  name: (identifier) @local.definition.type)

(trait_definition
  name: (identifier) @local.definition.type)

(struct_field
  name: (identifier) @local.definition.field)

(enum_variant
  name: (identifier) @local.definition.constant)

(for_in_statement
  element: (identifier) @local.definition.var)

; References
(identifier) @local.reference
