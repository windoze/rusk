; Comments
(line_comment) @comment
(block_comment) @comment
(shebang) @comment

; Keywords
[
  "use"
  "mod"
  "derive"
  "as"
  "is"
  "fn"
  "extern"
  "intrinsic"
  "cont"
  "let"
  "const"
  "readonly"
  "static"
  "struct"
  "enum"
  "interface"
  "impl"
  "type"
  "if"
  "else"
  "match"
  "return"
  "loop"
  "while"
  "for"
  "in"
  "break"
  "continue"
] @keyword

(visibility) @keyword

; Operators / punctuation
[
  "->"
  "=>"
  "::"
  "::<"
  "."
  ","
  ":"
  ";"
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.delimiter

[
  "="
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  "<="
  ">"
  ">="
  "&&"
  "||"
  "!"
  "?"
  "@"
  "|"
  "&"
  "^"
  "<<"
  ">>"
  ">>>"
] @operator

; Literals
(int_literal) @number
(float_literal) @number.float
(char_literal) @string.special
(string_literal) @string
(bytes_literal) @string
(formatted_string_literal) @string
(escape_sequence) @string.escape
(formatted_string_escape_brace) @string.escape

; Builtin types
[
  "unit"
  "bool"
  "int"
  "float"
  "byte"
  "char"
  "string"
  "bytes"
  "!"
] @type.builtin

; Identifiers
(type_identifier) @type

(function_item name: [(identifier) (type_identifier)] @function)
(extern_function_item name: [(identifier) (type_identifier)] @function)
(intrinsic_function_item name: [(identifier) (type_identifier)] @function)

(struct_item name: [(identifier) (type_identifier)] @type)
(enum_item name: [(identifier) (type_identifier)] @type)
(interface_item name: [(identifier) (type_identifier)] @type)

(impl_method name: [(identifier) (type_identifier)] @function.method)
(interface_method name: [(identifier) (type_identifier)] @function.method)

(field_declaration name: [(identifier) (type_identifier)] @property)
(struct_field_init name: [(identifier) (type_identifier)] @property)
(struct_pattern_field name: [(identifier) (type_identifier)] @property)

; Special identifiers
((identifier) @variable.builtin
  (#eq? @variable.builtin "self"))
((identifier) @variable.builtin
  (#eq? @variable.builtin "loaf"))
((identifier) @variable.builtin
  (#eq? @variable.builtin "super"))
