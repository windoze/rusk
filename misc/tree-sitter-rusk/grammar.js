// eslint-disable-next-line no-undef
module.exports = grammar({
  name: "rusk",

  externals: ($) => [$.block_comment],

  extras: ($) => [/\s/, $.line_comment, $.block_comment],

  word: ($) => $.identifier,

  supertypes: ($) => [$._item, $._statement, $._expression, $._type, $._pattern],

  conflicts: ($) => [
    // Path 相关规则在 pattern / type / expression 位置会出现局部歧义：
    // - `Foo::Bar` 既可能是 enum variant（bare pattern），也可能是结构体路径的前缀；
    // - `path_expression`/`qualified_identifier`/`struct_type_path` 在 `::` 的归属上存在 LR 冲突。
    //
    // 这里显式声明冲突，允许 tree-sitter 使用 GLR 解析并在后续 token 中消歧。
    [$.struct_type_path, $.path_expression],
    [$.struct_type_path, $.qualified_identifier, $.path_expression],
    [$.struct_type_path, $._path_segment],
    [$._identifier, $._path_segment],
    // `{ ... }` / `if ... { ... }` 等表达式在 block 里既可能是“无分号语句”，
    // 也可能作为 block 的 tail expression（产生值）。这里允许 GLR 消歧。
    [$._block_like_expression, $.primary_expression],
    [$.primary_expression, $.match_arm],
    [$.primary_expression, $.primary_expression_no_block],
    [$._expression, $._expression_no_block],
    // `TypePath { Assoc = T }`（关联类型绑定）在函数签名里会与函数体 `{ ... }` 冲突。
    // 允许 GLR 在读到 `}` 后再决定 `{` 的归属。
    [$.path_type],
    // `x as T<...> < y`：`<` 既可能是类型实参，也可能是比较运算符。
    [$.path_type_segment],
    // `|` 既可能是位运算 OR，也可能开启 trailing closure（`foo |x| {}`）。
    // 这需要上下文/前瞻才能判断，交给 GLR 处理。
    [$.unary_expression, $.binary_expression, $.call_expression],
    [$.binary_expression, $.call_expression],
    [$.unary_expression_no_struct_lit, $.binary_expression_no_struct_lit, $.call_expression_no_struct_lit],
    [$.binary_expression_no_struct_lit, $.call_expression_no_struct_lit],
  ],

  rules: {
    source_file: ($) => seq(optional($.shebang), repeat($._item)),

    shebang: (_) => token(seq("#!", /.*/)),

    line_comment: (_) => token(seq("//", /.*/)),

    _item: ($) =>
      choice(
        $.function_item,
        $.intrinsic_function_item,
        $.struct_item,
        $.enum_item,
        $.interface_item,
        $.impl_item,
        $.module_item,
        $.use_item,
        $.derive_item,
      ),

    visibility: (_) => "pub",

    function_item: ($) =>
      seq(
        optional(field("visibility", $.visibility)),
        "fn",
        field("name", $._identifier),
        field("type_parameters", optional($.generic_parameters)),
        field("parameters", $.parameters),
        field("return_type", optional($.return_type)),
        field("body", $.block),
      ),

    intrinsic_function_item: ($) =>
      seq(
        optional(field("visibility", $.visibility)),
        "intrinsic",
        "fn",
        field("name", $._identifier),
        field("type_parameters", optional($.generic_parameters)),
        field("parameters", $.parameters),
        field("return_type", optional($.return_type)),
        ";",
      ),

    struct_item: ($) =>
      seq(
        optional(field("visibility", $.visibility)),
        "struct",
        field("name", $._identifier),
        field("type_parameters", optional($.generic_parameters)),
        choice($.named_struct_body, $.newtype_struct_body),
      ),

    named_struct_body: ($) =>
      seq("{", optional($.field_declarations), "}"),

    newtype_struct_body: ($) => seq("(", field("type", $._type), ")", ";"),

    field_declarations: ($) =>
      commaSep1(field("field", $.field_declaration)),

    field_declaration: ($) =>
      seq(field("name", $._identifier), ":", field("type", $._type)),

    enum_item: ($) =>
      seq(
        optional(field("visibility", $.visibility)),
        "enum",
        field("name", $._identifier),
        field("type_parameters", optional($.generic_parameters)),
        "{",
        optional(commaSep1($.enum_variant)),
        "}",
      ),

    enum_variant: ($) =>
      seq(
        field("name", $._identifier),
        optional(field("fields", $.tuple_type_list)),
      ),

    interface_item: ($) =>
      seq(
        optional(field("visibility", $.visibility)),
        "interface",
        field("name", $._identifier),
        field("type_parameters", optional($.generic_parameters)),
        field("supers", optional($.interface_supers)),
        field("body", $.interface_body),
      ),

    interface_supers: ($) =>
      seq(":", commaSep1Plus(field("super", $.path_type_no_assoc_bindings))),

    interface_body: ($) => seq("{", repeat($.interface_member), "}"),

    interface_member: ($) =>
      choice($.associated_type_declaration, $.interface_method),

    associated_type_declaration: ($) => seq("type", field("name", $._identifier), ";"),

    interface_method: ($) =>
      seq(
        optional(field("qualifier", choice("readonly", "static"))),
        "fn",
        field("name", $._identifier),
        field("type_parameters", optional($.generic_parameters)),
        field("parameters", $.parameters),
        field("return_type", optional($.return_type)),
        choice(";", field("body", $.block)),
      ),

    impl_item: ($) =>
      seq(
        "impl",
        field("type_parameters", optional($.generic_parameters)),
        field("header", $.impl_header),
        "{",
        repeat($.impl_member),
        "}",
      ),

    impl_header: ($) =>
      choice(
        seq(field("type", $.path_type_no_assoc_bindings)),
        seq(
          field("interface", $.path_type_no_assoc_bindings),
          "for",
          field("type", $.path_type_no_assoc_bindings),
        ),
      ),

    impl_member: ($) => choice($.impl_associated_type, $.impl_method),

    impl_associated_type: ($) =>
      seq("type", field("name", $._identifier), "=", field("type", $._type), ";"),

    impl_method: ($) =>
      seq(
        optional(field("qualifier", choice("readonly", "static"))),
        "fn",
        field("name", $._identifier),
        field("type_parameters", optional($.generic_parameters)),
        field("parameters", $.parameters),
        field("return_type", optional($.return_type)),
        field("body", $.block),
      ),

    module_item: ($) =>
      seq(
        optional(field("visibility", $.visibility)),
        "mod",
        field("name", $._identifier),
        choice(";", field("body", $.module_body)),
      ),

    module_body: ($) => seq("{", repeat($._item), "}"),

    use_item: ($) =>
      seq(
        optional(field("visibility", $.visibility)),
        "use",
        field("path", $.path_expression),
        optional(seq("as", field("alias", $._identifier))),
        ";",
      ),

    derive_item: ($) =>
      seq(
        "derive",
        field("derives", commaSep1Plus(field("derive", $._identifier))),
        "for",
        field("target", $._identifier),
        ";",
      ),

    // ---- Statements / Blocks ----

    block: ($) =>
      seq(
        "{",
        repeat($._statement),
        optional(field("tail", $._expression)),
        "}",
      ),

    _statement: ($) =>
      choice(
        $.let_statement,
        $.const_statement,
        $.readonly_statement,
        $.return_statement,
        $.break_statement,
        $.continue_statement,
        $.expression_statement,
      ),

    let_statement: ($) =>
      seq(
        "let",
        field("pattern", $._pattern),
        optional(seq(":", field("type", $._type))),
        "=",
        field("value", $._expression),
        ";",
      ),

    const_statement: ($) =>
      seq(
        "const",
        field("pattern", $._pattern),
        optional(seq(":", field("type", $._type))),
        "=",
        field("value", $._expression),
        ";",
      ),

    readonly_statement: ($) =>
      seq(
        "readonly",
        field("pattern", $._pattern),
        optional(seq(":", field("type", $._type))),
        "=",
        field("value", $._expression),
        ";",
      ),

    return_statement: ($) => seq("return", optional(field("value", $._expression)), ";"),

    break_statement: (_) => seq("break", ";"),

    continue_statement: (_) => seq("continue", ";"),

    // block-like 表达式在语句位置允许省略分号；其他表达式必须以 `;` 结尾。
    // 为避免 `block` 同时被当作 `primary_expression` 导致 LR 冲突，这里显式区分两类表达式。
    expression_statement: ($) =>
      choice(
        seq($._block_like_expression, optional(";")),
        seq($._non_block_expression, ";"),
      ),

    _block_like_expression: ($) =>
      choice($.block, $.if_expression, $.match_expression, $.loop_expression, $.while_expression, $.for_expression),

    _non_block_expression: ($) =>
      choice(
        $.assignment_expression,
        $.binary_expression,
        $.as_expression,
        $.is_expression,
        $.unary_expression,
        $.call_expression,
        $.field_expression,
        $.index_expression,
        $.non_block_primary_expression,
      ),

    non_block_primary_expression: ($) =>
      choice(
        $.literal,
        $.path_expression,
        $.array_expression,
        $.tuple_expression,
        $.struct_literal,
        $.effect_call,
        $.lambda_expression,
        $.parenthesized_expression,
      ),

    // ---- Expressions ----

    _expression: ($) =>
      choice(
        $.assignment_expression,
        $.binary_expression,
        $.as_expression,
        $.is_expression,
        $.unary_expression,
        $.call_expression,
        $.field_expression,
        $.index_expression,
        $.primary_expression,
      ),

    // 与 `_expression` 相同，但不把单独的 `{ ... }` 作为一个表达式分支（仍允许 `{...}(...)` 这类“以 block 开头”的 call）。
    _expression_no_block: ($) =>
      choice(
        $.assignment_expression,
        $.binary_expression,
        $.as_expression,
        $.is_expression,
        $.unary_expression,
        $.call_expression,
        $.field_expression,
        $.index_expression,
        $.primary_expression_no_block,
      ),

    // 用于 `if cond { ... }` / `match scrutinee { ... }` / `while cond { ... }` / `for .. in iter { ... }`
    // 这类“紧跟一个 block”的表达式位置：
    // - 禁止 `Foo { ... }` 直接作为 struct literal（需要写成 `(Foo { ... })`）
    // - 禁止 `foo { ... }` 直接作为 trailing block closure（需要写成 `(foo { ... })`）
    //
    // 与编译器里 `parse_expr_no_struct_lit` 的行为保持一致。
    _expression_no_struct_lit: ($) =>
      choice(
        $.assignment_expression_no_struct_lit,
        $.binary_expression_no_struct_lit,
        $.as_expression_no_struct_lit,
        $.is_expression_no_struct_lit,
        $.unary_expression_no_struct_lit,
        $.call_expression_no_struct_lit,
        $.field_expression_no_struct_lit,
        $.index_expression_no_struct_lit,
        $.primary_expression_no_struct_lit,
      ),

    primary_expression: ($) =>
      choice(
        $.literal,
        $.path_expression,
        $.array_expression,
        $.tuple_expression,
        $.struct_literal,
        $.effect_call,
        $.lambda_expression,
        $.if_expression,
        $.match_expression,
        $.loop_expression,
        $.while_expression,
        $.for_expression,
        $.block,
        $.parenthesized_expression,
      ),

    primary_expression_no_block: ($) =>
      choice(
        $.literal,
        $.path_expression,
        $.array_expression,
        $.tuple_expression,
        $.struct_literal,
        $.effect_call,
        $.lambda_expression,
        $.if_expression,
        $.match_expression,
        $.loop_expression,
        $.while_expression,
        $.for_expression,
        $.parenthesized_expression,
      ),

    primary_expression_no_struct_lit: ($) =>
      choice(
        $.literal,
        $.path_expression,
        $.array_expression,
        $.tuple_expression,
        $.effect_call,
        $.lambda_expression,
        $.if_expression,
        $.match_expression,
        $.loop_expression,
        $.while_expression,
        $.for_expression,
        $.block,
        $.parenthesized_expression,
      ),

    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    assignment_expression: ($) =>
      prec.right(1, seq(field("left", $._expression), "=", field("right", $._expression))),

    assignment_expression_no_struct_lit: ($) =>
      prec.right(
        1,
        seq(
          field("left", $._expression_no_struct_lit),
          "=",
          field("right", $._expression_no_struct_lit),
        ),
      ),

    unary_expression: ($) =>
      prec.right(23, seq(field("operator", choice("!", "-")), field("argument", $._expression))),

    unary_expression_no_struct_lit: ($) =>
      prec.right(
        23,
        seq(
          field("operator", choice("!", "-")),
          field("argument", $._expression_no_struct_lit),
        ),
      ),

    as_expression: ($) =>
      prec.left(
        21,
        seq(
          field("value", $._expression),
          "as",
          optional(field("checked", "?")),
          field("type", $._type),
        ),
      ),

    as_expression_no_struct_lit: ($) =>
      prec.left(
        21,
        seq(
          field("value", $._expression_no_struct_lit),
          "as",
          optional(field("checked", "?")),
          field("type", $._type),
        ),
      ),

    is_expression: ($) =>
      prec.left(7, seq(field("value", $._expression), "is", field("type", $._type))),

    is_expression_no_struct_lit: ($) =>
      prec.left(
        7,
        seq(
          field("value", $._expression_no_struct_lit),
          "is",
          field("type", $._type),
        ),
      ),

    binary_expression: ($) =>
      choice(
        prec.left(3, seq(field("left", $._expression), "||", field("right", $._expression))),
        prec.left(5, seq(field("left", $._expression), "&&", field("right", $._expression))),

        // comparisons + equality (same precedence group in the reference parser)
        prec.left(
          7,
          seq(
            field("left", $._expression),
            field(
              "operator",
              choice("==", "!=", "<", "<=", ">", ">="),
            ),
            field("right", $._expression),
          ),
        ),

        // bitwise
        prec.left(9, seq(field("left", $._expression), "|", field("right", $._expression))),
        prec.left(11, seq(field("left", $._expression), "^", field("right", $._expression))),
        prec.left(13, seq(field("left", $._expression), "&", field("right", $._expression))),

        // shifts
        prec.left(15, seq(field("left", $._expression), "<<", field("right", $._expression))),
        prec.left(15, seq(field("left", $._expression), ">>>", field("right", $._expression))),
        prec.left(15, seq(field("left", $._expression), ">>", field("right", $._expression))),

        // arithmetic
        prec.left(
          17,
          seq(field("left", $._expression), field("operator", choice("+", "-")), field("right", $._expression)),
        ),
        prec.left(
          19,
          seq(field("left", $._expression), field("operator", choice("*", "/", "%")), field("right", $._expression)),
        ),
      ),

    binary_expression_no_struct_lit: ($) =>
      choice(
        prec.left(
          3,
          seq(
            field("left", $._expression_no_struct_lit),
            "||",
            field("right", $._expression_no_struct_lit),
          ),
        ),
        prec.left(
          5,
          seq(
            field("left", $._expression_no_struct_lit),
            "&&",
            field("right", $._expression_no_struct_lit),
          ),
        ),
        prec.left(
          7,
          seq(
            field("left", $._expression_no_struct_lit),
            field("operator", choice("==", "!=", "<", "<=", ">", ">=")),
            field("right", $._expression_no_struct_lit),
          ),
        ),
        prec.left(
          9,
          seq(
            field("left", $._expression_no_struct_lit),
            "|",
            field("right", $._expression_no_struct_lit),
          ),
        ),
        prec.left(
          11,
          seq(
            field("left", $._expression_no_struct_lit),
            "^",
            field("right", $._expression_no_struct_lit),
          ),
        ),
        prec.left(
          13,
          seq(
            field("left", $._expression_no_struct_lit),
            "&",
            field("right", $._expression_no_struct_lit),
          ),
        ),
        prec.left(
          15,
          seq(
            field("left", $._expression_no_struct_lit),
            "<<",
            field("right", $._expression_no_struct_lit),
          ),
        ),
        prec.left(
          15,
          seq(
            field("left", $._expression_no_struct_lit),
            ">>>",
            field("right", $._expression_no_struct_lit),
          ),
        ),
        prec.left(
          15,
          seq(
            field("left", $._expression_no_struct_lit),
            ">>",
            field("right", $._expression_no_struct_lit),
          ),
        ),
        prec.left(
          17,
          seq(
            field("left", $._expression_no_struct_lit),
            field("operator", choice("+", "-")),
            field("right", $._expression_no_struct_lit),
          ),
        ),
        prec.left(
          19,
          seq(
            field("left", $._expression_no_struct_lit),
            field("operator", choice("*", "/", "%")),
            field("right", $._expression_no_struct_lit),
          ),
        ),
      ),

    field_expression: ($) =>
      prec.left(
        27,
        seq(
          field("base", $._expression),
          ".",
          field("field", choice($._identifier, $.int_literal)),
        ),
      ),

    field_expression_no_struct_lit: ($) =>
      prec.left(
        27,
        seq(
          field("base", $._expression_no_struct_lit),
          ".",
          field("field", choice($._identifier, $.int_literal)),
        ),
      ),

    index_expression: ($) =>
      prec.left(
        25,
        seq(field("base", $._expression), "[", field("index", $._expression), "]"),
      ),

    // 与编译器保持一致：`[..]` 的索引表达式本身不需要禁用 struct literal；
    // 这里仅限制 base，index 仍允许完整表达式。
    index_expression_no_struct_lit: ($) =>
      prec.left(
        25,
        seq(field("base", $._expression_no_struct_lit), "[", field("index", $._expression), "]"),
      ),

    call_expression: ($) =>
      prec.left(
        25,
        choice(
          seq(
            field("function", $._expression),
            optional(field("type_arguments", $.turbofish)),
            field("arguments", $.argument_list),
            optional(field("trailing_closure", $.trailing_closure)),
          ),
          seq(
            field("function", $._expression),
            optional(field("type_arguments", $.turbofish)),
            field("trailing_closure", $.trailing_closure),
          ),
        ),
      ),

    // no_struct_lit 版本：禁止 `{ ... }` block 作为 trailing closure（避免与后续 block 歧义）
    call_expression_no_struct_lit: ($) =>
      prec.left(
        25,
        choice(
          seq(
            field("function", $._expression_no_struct_lit),
            optional(field("type_arguments", $.turbofish)),
            field("arguments", $.argument_list),
            optional(field("trailing_closure", $.lambda_expression)),
          ),
          seq(
            field("function", $._expression_no_struct_lit),
            optional(field("type_arguments", $.turbofish)),
            field("trailing_closure", $.lambda_expression),
          ),
        ),
      ),

    turbofish: ($) => seq("::<", optional($.type_list), ">"),

    argument_list: ($) => seq("(", optional($.argument_list_items), ")"),

    argument_list_items: ($) => commaSep1(field("argument", $._expression)),

    trailing_closure: ($) => choice($.block, $.lambda_expression),

    lambda_expression: ($) =>
      seq(
        "|",
        optional(commaSep1(field("parameter", $.lambda_parameter))),
        "|",
        field("body", $.block),
      ),

    lambda_parameter: ($) =>
      seq(
        field("name", $._identifier),
        optional(seq(":", field("type", $._type))),
      ),

    if_expression: ($) =>
      prec.right(
        seq(
          "if",
          field("condition", $._expression_no_struct_lit),
          field("consequence", $.block),
          optional(
            seq(
              "else",
              field("alternative", choice($.if_expression, $.block)),
            ),
          ),
        ),
      ),

    match_expression: ($) =>
      seq(
        "match",
        field("scrutinee", $._expression_no_struct_lit),
        "{",
        repeat($.match_arm),
        "}",
      ),

    match_arm: ($) =>
      seq(
        field("pattern", choice($.effect_pattern, $._pattern)),
        "=>",
        field("body", choice($.block, $._expression_no_block)),
        optional(","),
      ),

    effect_pattern: ($) =>
      seq(
        "@",
        field("interface", $.path_type),
        ".",
        field("method", $._identifier),
        "(",
        optional(commaSep1(field("argument", $._pattern))),
        ")",
        optional(seq("->", field("continuation", $._identifier))),
      ),

    effect_call: ($) =>
      seq(
        "@",
        field("interface", $.path_type),
        ".",
        field("method", $._identifier),
        field("arguments", $.argument_list),
      ),

    loop_expression: ($) => seq("loop", field("body", $.block)),

    while_expression: ($) => seq("while", field("condition", $._expression_no_struct_lit), field("body", $.block)),

    for_expression: ($) =>
      seq(
        "for",
        field("pattern", $._pattern),
        "in",
        field("iterable", $._expression_no_struct_lit),
        field("body", $.block),
      ),

    array_expression: ($) => seq("[", optional(commaSep1(field("item", $._expression))), "]"),

    tuple_expression: ($) =>
      seq("(", field("first", $._expression), ",", optional($.argument_list_items), ")"),

    struct_literal: ($) =>
      seq(
        field("type", $.struct_type_path),
        "{",
        optional(commaSep1(field("field", $.struct_field_init))),
        "}",
      ),

    struct_type_path: ($) =>
      seq(repeat(seq($._path_segment, "::")), field("name", $.type_identifier)),

    struct_field_init: ($) =>
      seq(field("name", $._identifier), ":", field("value", $._expression)),

    // ---- Patterns ----

    _pattern: ($) =>
      choice(
        $.wildcard_pattern,
        $.literal_pattern,
        $.tuple_pattern,
        $.array_pattern,
        $.struct_pattern,
        $.constructor_pattern,
        $.enum_pattern,
        $.binding_pattern,
      ),

    wildcard_pattern: ($) => field("wildcard", $.wildcard),

    binding_pattern: ($) => field("name", $._identifier),

    enum_pattern: ($) =>
      // Bare enum variant: `EnumPath::Variant`
      seq(field("path", $.qualified_identifier)),

    constructor_pattern: ($) =>
      // Constructor-like pattern (resolved during typechecking):
      // - enum variant with fields: `EnumPath::Variant(p1, p2)`
      // - new-type struct: `TypePath(p)`
      seq(field("path", $.path_expression), field("arguments", $.pattern_arguments)),

    pattern_arguments: ($) =>
      seq("(", optional(commaSep1(field("argument", $._pattern))), ")"),

    struct_pattern: ($) =>
      seq(
        field("type", $.struct_type_path),
        "{",
        optional(commaSep1(field("field", $.struct_pattern_field))),
        "}",
      ),

    struct_pattern_field: ($) =>
      choice(
        "..",
        seq(
          field("name", $._identifier),
          optional(seq(":", field("pattern", $._pattern))),
        ),
      ),

    tuple_pattern: ($) =>
      choice(
        // Unit pattern.
        "()",
        // Tuple pattern with leading rest: `(..rest)`
        seq("(", field("rest", $.rest_pattern), ")"),
        // Parenthesized pattern: `(p)`
        seq("(", field("pattern", $._pattern), ")"),
        // Tuple pattern: `(a, b)` / `(a,)` / `(..rest, a, b)`
        $.pattern_tuple,
      ),

    pattern_tuple: ($) =>
      seq(
        "(",
        field("item", choice($._pattern, $.rest_pattern)),
        ",",
        optional(commaSep1(field("item", choice($._pattern, $.rest_pattern)))),
        ")",
      ),

    rest_pattern: ($) => seq("..", optional(field("name", $._identifier))),

    array_pattern: ($) =>
      seq(
        "[",
        optional(commaSep1(field("item", choice($._pattern, $.rest_pattern)))),
        "]",
      ),

    // `()` 在 pattern 位置由 `tuple_pattern` 的 unit pattern 分支负责解析；
    // 这里刻意排除 `unit_literal` 以避免与 `tuple_pattern` 产生歧义冲突。
    literal_pattern: ($) =>
      choice(
        $.bool_literal,
        $.int_literal,
        $.float_literal,
        $.char_literal,
        $.string_literal,
        $.bytes_literal,
        $.formatted_string_literal,
      ),

    // ---- Types ----

    _type: ($) => choice($.readonly_type, $.type_atom),

    readonly_type: ($) => seq("readonly", field("type", $._type)),

    type_atom: ($) =>
      choice(
        $.unit_type,
        $.primitive_type,
        $.array_type,
        $.function_type,
        $.continuation_type,
        $.tuple_type,
        $.parenthesized_type,
        $.path_type,
      ),

    unit_type: (_) => "()",

    primitive_type: (_) =>
      choice("unit", "bool", "int", "float", "byte", "char", "string", "bytes", "!"),

    array_type: ($) => seq("[", field("type", $._type), "]"),

    function_type: ($) =>
      seq(
        "fn",
        "(",
        optional($.type_list),
        ")",
        optional(seq("->", field("return_type", $._type))),
      ),

    continuation_type: ($) =>
      seq(
        "cont",
        "(",
        field("parameter", $._type),
        ")",
        "->",
        field("return_type", $._type),
      ),

    parenthesized_type: ($) => seq("(", $._type, ")"),

    tuple_type: ($) =>
      seq(
        "(",
        field("type", $._type),
        ",",
        optional(commaSep1(field("type", $._type))),
        ")",
      ),

    tuple_type_list: ($) =>
      seq("(", optional($.type_list), ")"),

    type_list: ($) => commaSep1(field("type", $._type)),

    return_type: ($) => seq("->", field("type", $._type)),

    parameters: ($) => seq("(", optional($.parameter_list), ")"),

    parameter_list: ($) => commaSep1(field("parameter", $.parameter)),

    parameter: ($) =>
      seq(
        optional(field("readonly", "readonly")),
        field("pattern", $._pattern),
        ":",
        field("type", $._type),
      ),

    path_type_no_assoc_bindings: ($) =>
      seq($.path_type_segment, repeat(seq("::", $.path_type_segment))),

    path_type: ($) =>
      seq(
        $.path_type_segment,
        repeat(seq("::", $.path_type_segment)),
        optional(field("assoc_bindings", $.assoc_bindings)),
      ),

    path_type_segment: ($) =>
      seq(
        field("name", $._identifier),
        optional(field("type_arguments", $.generic_arguments)),
      ),

    assoc_bindings: ($) =>
      seq(
        "{",
        optional(commaSep1(field("binding", $.assoc_binding))),
        "}",
      ),

    assoc_binding: ($) => seq(field("name", $._identifier), "=", field("type", $._type)),

    generic_parameters: ($) =>
      seq("<", commaSep1(field("parameter", $.generic_parameter)), ">"),

    generic_parameter: ($) =>
      seq(
        field("name", $._identifier),
        optional(field("hkt_arity", $.hkt_arity)),
        optional(field("bounds", $.bounds)),
      ),

    hkt_arity: ($) => seq("<", "_", repeat(seq(",", "_")), ">"),

    bounds: ($) => seq(":", commaSep1Plus(field("bound", $.path_type))),

    generic_arguments: ($) =>
      seq("<", commaSep1(field("argument", $._type)), ">"),

    // ---- Identifiers / Paths ----

    wildcard: (_) => token(prec(2, "_")),

    identifier: (_) => token(prec(0, /[_\p{XID_Start}][_\p{XID_Continue}]*/u)),

    type_identifier: (_) => token(prec(1, /[A-Z][A-Za-z0-9_]*/)),

    _identifier: ($) => choice($.identifier, $.type_identifier),

    _path_segment: ($) => choice($.identifier, $.type_identifier),

    qualified_identifier: ($) =>
      seq(
        field("head", $._path_segment),
        repeat1(seq("::", field("segment", $._path_segment))),
      ),

    path_expression: ($) =>
      seq(field("head", $._path_segment), repeat(seq("::", field("segment", $._path_segment)))),

    // ---- Literals ----

    literal: ($) =>
      choice(
        $.unit_literal,
        $.bool_literal,
        $.int_literal,
        $.float_literal,
        $.char_literal,
        $.string_literal,
        $.bytes_literal,
        $.formatted_string_literal,
      ),

    unit_literal: (_) => "()", // expression form

    bool_literal: (_) => choice("true", "false"),

    int_literal: (_) =>
      token(
        choice(
          /0x[0-9a-fA-F](?:[0-9a-fA-F_])*/,
          /0o[0-7](?:[0-7_])*/,
          /0b[01](?:[01_])*/,
          /[0-9](?:[0-9_])*/,
        ),
      ),

    float_literal: (_) =>
      token(
        choice(
          /[0-9](?:[0-9_])*\.[0-9](?:[0-9_])*(?:[eE][+-]?[0-9](?:[0-9_])*)?/,
          /[0-9](?:[0-9_])*(?:[eE][+-]?[0-9](?:[0-9_])*)/,
        ),
      ),

    char_literal: (_) => token(/'(?:[^'\\]|\\(?:x[0-9a-fA-F]{2}|u\{[0-9a-fA-F]+\}|.))'/),

    string_literal: (_) => token(/"(?:[^"\\]|\\(?:u\{[0-9a-fA-F]+\}|.))*"/),

    bytes_literal: (_) => token(/b"(?:[^"\\]|\\(?:x[0-9a-fA-F]{2}|.))*"/),

    formatted_string_literal: ($) =>
      seq(
        'f"',
        repeat(
          choice(
            $.escape_sequence,
            $.formatted_string_text,
            $.formatted_string_escape_brace,
            $.interpolation,
          ),
        ),
        '"',
      ),

    formatted_string_text: (_) => token(prec(1, /[^"\\{}]+/)),

    formatted_string_escape_brace: (_) => token(choice("{{", "}}")),

    escape_sequence: (_) =>
      token(/\\(?:\\|"|n|r|t|0|u\{[0-9a-fA-F]+\})/),

    interpolation: ($) => seq("{", field("expression", $._expression), "}"),
  },
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)), optional(","));
}

function commaSep1Plus(rule) {
  return seq(rule, repeat(seq("+", rule)));
}
