const PREC = {
  OR: 1, // or
  AND: 2, // and
  COMPARE: 3, // < > <= >= ~= ==
  BIT_OR: 4, // |
  BIT_NOT: 5, // ~
  BIT_AND: 6, // &
  BIT_SHIFT: 7, // << >>
  CONCAT: 8, // ..
  PLUS: 9, // + -
  MULTI: 10, // * / // %
  UNARY: 11, // not # - ~
  POWER: 12, // ^
};

const list_seq = (rule, separator, trailing_separator = false) =>
  trailing_separator
    ? seq(rule, repeat(seq(separator, rule)), optional(separator))
    : seq(rule, repeat(seq(separator, rule)));

const optional_block = ($) => alias(optional($._block), $.block);
const terra_optional_block = ($) => alias(optional($._terra_block), $.terra_block);

// namelist ::= Name {',' Name}
const name_list = ($) => list_seq(field('name', $.identifier), ',');

const type_name_list = ($) =>
  list_seq(
    choice(
      seq(
        field('name', $.identifier),
        // optional(field('type', alias($.expression, $.type)))
        ':', field('type', $.expression),
      ),
      $.terra_macro_escape,
    ),
    ','
  );


module.exports = grammar({
  name: 'terra',

  extras: ($) => [$.comment, /\s/],

  externals: ($) => [
    $._block_comment_start,
    $._block_comment_content,
    $._block_comment_end,

    $._block_string_start,
    $._block_string_content,
    $._block_string_end,
  ],

  supertypes: ($) => [$.statement, $.expression, $.declaration, $.variable],

  word: ($) => $.identifier,

  rules: {
    // chunk ::= block
    chunk: ($) =>
      seq(
        optional($.hash_bang_line),
        repeat($.statement),
        optional($.return_statement)
      ),

    hash_bang_line: (_) => /#.*/,

    // block ::= {stat} [retstat]
    _block: ($) =>
      choice(
        seq(repeat1($.statement), optional($.return_statement)),
        seq(repeat($.statement), $.return_statement)
      ),

    // block ::= {stat} [retstat]
    _terra_block: ($) =>
      choice(
        // seq(repeat1($.terra_statement), optional($.terra_return_statement)),
        // seq(repeat($.terra_statement), $.terra_return_statement)
        seq(repeat1($.terra_statement), optional($.return_statement)),
        seq(repeat($.terra_statement), $.return_statement)
      ),

    /*
      stat ::=  ';' |
                varlist '=' explist |
                functioncall |
                label |
                break |
                goto Name |
                do block end |
                while exp do block end |
                repeat block until exp |
                if exp then block {elseif exp then block} [else block] end |
                for Name '=' exp ',' exp [',' exp] do block end |
                for namelist in explist do block end |
                function funcname funcbody |
                local function Name funcbody |
                local namelist ['=' explist]
    */
    statement: ($) =>
      choice(
        $.empty_statement,
        $.assignment_statement,
        $.function_call,
        $.label_statement,
        $.break_statement,
        $.goto_statement,
        $.do_statement,
        $.while_statement,
        $.repeat_statement,
        $.if_statement,
        $.for_statement,
        $.declaration,
        $.terra_struct_decl,
        // $.terra_variable_declaration,
      ),

    terra_struct_decl: ($) =>
      choice(
        seq('local', $.terra_struct),
        $.terra_struct,
      ),

    // if exp then block {elseif exp then block} [else block] end
    terra_if_statement: ($) =>
      seq(
        'if',
        field('condition', $.expression),
        'then',
        field('consequence', terra_optional_block($)),
        repeat(field('alternative', $.terra_elseif_statement)),
        optional(field('alternative', $.terra_else_statement)),
        'end'
      ),
    // elseif exp then block
    terra_elseif_statement: ($) =>
      seq(
        'elseif',
        field('condition', $.expression),
        'then',
        field('consequence', terra_optional_block($))
      ),
    // else block
    terra_else_statement: ($) => seq('else', field('body', terra_optional_block($))),

    // while exp do block end
    terra_while_statement: ($) =>
      seq(
        'while',
        field('condition', $.expression),
        'do',
        field('body', terra_optional_block($)),
        'end'
      ),

    // repeat block until exp
    terra_repeat_statement: ($) =>
      seq(
        'repeat',
        field('body', terra_optional_block($)),
        'until',
        field('condition', $.expression)
      ),

    terra_statement: ($) =>
      seq(choice(
        // $.empty_statement,
        // $.assignment_statement,
        $.function_call,
        $.terra_label_statement,
        $.break_statement,
        $.terra_goto_statement,
        // $.do_statement,
        $.terra_while_statement,
        $.terra_repeat_statement,
        $.terra_if_statement,
        $.terra_for_statement,
        // $.declaration,
        $.terra_variable_declaration,
        $.terra_assignment,
        $.terra_macro_escape,
        $.terra_macro_emit,
      ), optional(';')),

    // for Name '=' exp ',' exp [',' exp] do block end
    // for namelist in explist do block end
    terra_for_statement: ($) =>
      seq(
        'for',
        field('clause', choice($.terra_for_generic_clause, $.terra_for_numeric_clause)),
        'do',
        field('body', terra_optional_block($)),
        'end'
      ),
    // namelist in explist
    terra_for_generic_clause: ($) =>
      seq(
        alias($._name_list, $.variable_list),
        'in',
        alias($._expression_list, $.expression_list)
      ),
    // Name '=' exp ',' exp [',' exp]
    terra_for_numeric_clause: ($) =>
      seq(
        field('name', $._terra_identifier_or_escape),
        '=',
        field('start', $.expression),
        ',',
        field('end', $.expression),
        optional(seq(',', field('step', $.expression)))
      ),

    terra_macro_escape: ($) => choice($._terra_macro_escape_bracket,
        $._terra_macro_escape_block),

    _terra_identifier_or_escape: ($) => choice($.identifier,
       $.terra_macro_escape),

    // goto Name
    terra_goto_statement: ($) => seq('goto',
       $._terra_identifier_or_escape),

    // label ::= '::' Name '::'
    terra_label_statement: ($) => seq('::', $._terra_identifier_or_escape, '::'),


    terra_assignment: ($) =>
      seq(
        list_seq(choice(
          $.terra_deref,
          $.terra_macro_escape,
          $.identifier,
          $.bracket_index_expression,
          $.dot_index_expression,
        ), ','),
        '=',
        list_seq($.expression, ','),
      ),

    _type_name_list: ($) =>
      list_seq(
        seq(
          field('name', $._terra_identifier_or_escape),
          // optional(field('type', alias($.expression, $.type)))
          optional(seq(':', field('type', $.expression)))
        ),
        ','
      ),

    // local namelist ['=' explist]
    terra_variable_declaration: ($) =>
      seq(
        'var',
        $._type_name_list,
        // type_name_list($),
        optional(
          // seq(
          //   '=',
          //   field('terra_assignment', $.expression),
          // ),
          seq(
            '=',
            field('terra_assignment', list_seq($.expression, ',')),
          ),
        )
        // choice(
        //   alias($._type_name_list, $.variable_list),
        //   // alias($._local_variable_assignment, $.assignment_statement)
        //   alias($._terra_local_variable_assignment, $.assignment_statement)
        // )
      ),
    // _terra_local_variable_assignment: ($) =>
    //   seq(
    //     alias($._type_name_list, $.variable_list),
    //     '=',
    //     alias($._variable_assignment_explist, $.expression_list)
    //   ),

    // retstat ::= return [explist] [';']
    return_statement: ($) =>
      seq(
        'return',
        optional(alias($._expression_list, $.expression_list)),
        optional(';')
      ),

    // ';'
    empty_statement: (_) => ';',

    // varlist '=' explist
    assignment_statement: ($) =>
      seq(
        alias($._variable_assignment_varlist, $.variable_list),
        '=',
        alias($._variable_assignment_explist, $.expression_list)
      ),
    // varlist ::= var {',' var}
    _variable_assignment_varlist: ($) =>
      list_seq(field('name', $.variable), ','),
    // explist ::= exp {',' exp}
    _variable_assignment_explist: ($) =>
      list_seq(field('value', $.expression), ','),

    // label ::= '::' Name '::'
    label_statement: ($) => seq('::', $.identifier, '::'),

    // break
    break_statement: (_) => 'break',

    // goto Name
    goto_statement: ($) => seq('goto', $.identifier),

    // do block end
    do_statement: ($) => seq('do', field('body', optional_block($)), 'end'),

    // while exp do block end
    while_statement: ($) =>
      seq(
        'while',
        field('condition', $.expression),
        'do',
        field('body', optional_block($)),
        'end'
      ),

    // repeat block until exp
    repeat_statement: ($) =>
      seq(
        'repeat',
        field('body', optional_block($)),
        'until',
        field('condition', $.expression)
      ),

    // if exp then block {elseif exp then block} [else block] end
    if_statement: ($) =>
      seq(
        'if',
        field('condition', $.expression),
        'then',
        field('consequence', optional_block($)),
        repeat(field('alternative', $.elseif_statement)),
        optional(field('alternative', $.else_statement)),
        'end'
      ),
    // elseif exp then block
    elseif_statement: ($) =>
      seq(
        'elseif',
        field('condition', $.expression),
        'then',
        field('consequence', optional_block($))
      ),
    // else block
    else_statement: ($) => seq('else', field('body', optional_block($))),

    // for Name '=' exp ',' exp [',' exp] do block end
    // for namelist in explist do block end
    for_statement: ($) =>
      seq(
        'for',
        field('clause', choice($.for_generic_clause, $.for_numeric_clause)),
        'do',
        field('body', optional_block($)),
        'end'
      ),
    // namelist in explist
    for_generic_clause: ($) =>
      seq(
        alias($._name_list, $.variable_list),
        'in',
        alias($._expression_list, $.expression_list)
      ),
    // Name '=' exp ',' exp [',' exp]
    for_numeric_clause: ($) =>
      seq(
        field('name', $.identifier),
        '=',
        field('start', $.expression),
        ',',
        field('end', $.expression),
        optional(seq(',', field('step', $.expression)))
      ),

    // function funcname funcbody
    // local function Name funcbody
    // local namelist ['=' explist]
    declaration: ($) =>
      choice(
        $.terra_function_declaration,
        $.terra_local_function_declaration,
        $.function_declaration,
        field(
          'local_declaration',
          alias($._local_function_declaration, $.function_declaration)
        ),
        field('local_declaration', $.variable_declaration)
      ),
    // function funcname funcbody
    terra_function_declaration: ($) =>
      seq('terra', field('name', $._function_name), $._terra_function_body),
    // function funcname funcbody
    terra_local_function_declaration: ($) =>
      seq('local', 'terra', field('name', $._function_name), $._terra_function_body),
    terra_function_definition: ($) =>
      seq('terra', $._terra_function_body),
    // function funcname funcbody
    function_declaration: ($) =>
      seq('function', field('name', $._function_name), $._function_body),
    // local function Name funcbody
    _local_function_declaration: ($) =>
      seq('local', 'function', field('name', $.identifier), $._function_body),
    // funcname ::= Name {'.' Name} [':' Name]
    _function_name: ($) =>
      choice(
        $._function_name_prefix_expression,
        alias(
          $._function_name_method_index_expression,
          $.method_index_expression
        )
      ),
    _function_name_prefix_expression: ($) =>
      choice(
        $.identifier,
        alias($._function_name_dot_index_expression, $.dot_index_expression)
      ),
    _function_name_dot_index_expression: ($) =>
      seq(
        field('table', $._function_name_prefix_expression),
        '.',
        field('field', $.identifier)
      ),
    _function_name_method_index_expression: ($) =>
      seq(
        field('table', $._function_name_prefix_expression),
        ':',
        field('method', $.identifier)
      ),

    // local namelist ['=' explist]
    variable_declaration: ($) =>
      seq(
        'local',
        choice(
          alias($._att_name_list, $.variable_list),
          alias($._local_variable_assignment, $.assignment_statement)
        )
      ),
    _local_variable_assignment: ($) =>
      seq(
        alias($._att_name_list, $.variable_list),
        '=',
        alias($._variable_assignment_explist, $.expression_list)
      ),
    // namelist ::= Name {',' Name}
    _name_list: ($) => name_list($),

    // attnamelist ::=  Name attrib {‘,’ Name attrib}
    _att_name_list: ($) =>
      list_seq(
        seq(
          field('name', $.identifier),
          optional(field('attribute', alias($._attrib, $.attribute)))
        ),
        ','
      ),
    // attrib ::= [‘<’ Name ‘>’]
    _attrib: ($) => seq('<', $.identifier, '>'),

    // explist ::= exp {',' exp}
    _expression_list: ($) => list_seq($.expression, ','),

    /*
      exp ::=  nil | false | true | Numeral | LiteralString | '...' | functiondef |
               prefixexp | tableconstructor | exp binop exp | unop exp
     */
    expression: ($) =>
      choice(
        $.nil,
        $.false,
        $.true,
        $.number,
        $.string,
        $.vararg_expression,
        $.function_definition,
        $.terra_function_definition,
        $.variable,
        $.function_call,
        $.parenthesized_expression,
        $.table_constructor,
        $.binary_expression,
        $.unary_expression,
        $.terra_cast,
        $.terra_macro_tick,
        $.terra_macro_escape,
        $.terra_macro_quote_block,
        $.terra_pointer,
        $.terra_deref,
        $.terra_struct,
        $.terra_function_type,
      ),

    terra_cast: ($) =>
      prec(13, seq(
        $._terra_macro_escape_bracket,
        '(',
        $.expression,
        ')',
      )),

    terra_macro_tick: ($) =>
      seq(
        '`',
        field('escaped', $.expression),
      ),

    terra_pointer: ($) =>
      seq(
        '&',
        $.expression,
      ),

    terra_struct: ($) =>
      prec(13, seq(
        'struct',
        optional(field('name', choice($.variable, $.terra_macro_escape))),
        '{',
        repeat(
                seq(
                  choice(
                    seq(
                      field('name', $.identifier),
                      // optional(field('type', alias($.expression, $.type)))
                      ':', field('type', $.expression),
                    ),
                    $.terra_macro_escape,
                  ),
                  optional(choice(',', ';'))
                  )
                ),
        '}',
      )),

    // terra_type: ($) =>
    //   prec(15, choice(
    //     $.variable,
    //     $.terra_macro_escape,
    //     // $.terra_pointer,
    //     seq('&', $.terra_type),
    //     seq('{', optional(list_seq($.terra_type, ',')), '}')
    //   )),

    terra_function_type: ($) =>
      prec.right(20, seq(
        // $.terra_type,
        $.expression,
        '->',
        $.expression,
        // $.terra_type,
      )),

    // terra_function_type: ($) =>
    //   list_seq($.expression, '->'),

    terra_deref: ($) =>
      seq(
        '@',
        choice(
          $.identifier,
          seq('(', $.expression, ')'),
        )
      ),

    _terra_macro_escape_bracket: ($) =>
      seq(
        '[',
        $.expression,
        ']',
      ),

    _terra_macro_escape_block: ($) =>
      seq(
        'escape',
        optional_block($),
        'end',
      ),

    terra_macro_quote_block: ($) =>
      seq(
        'quote',
        terra_optional_block($),
        'end',
      ),

    terra_macro_emit: ($) =>
      seq(
        'emit',
        choice(
          $.terra_macro_quote_block,
          $.terra_macro_tick,
        )
      ),


    // nil
    nil: (_) => 'nil',

    // false
    false: (_) => 'false',

    // true
    true: (_) => 'true',

    // Numeral
    number: (_) => {
      function number_literal(digits, exponent_marker, exponent_digits) {
        return choice(
          seq(digits, /U?LL/i),
          seq(
            choice(
              seq(optional(digits), optional('.'), digits),
              seq(digits, optional('.'), optional(digits))
            ),
            optional(
              seq(
                choice(
                  exponent_marker.toLowerCase(),
                  exponent_marker.toUpperCase()
                ),
                seq(optional(choice('-', '+')), exponent_digits)
              )
            ),
            optional(choice('i', 'I'))
          )
        );
      }

      const decimal_digits = /[0-9]+/;
      const decimal_literal = number_literal(
        decimal_digits,
        'e',
        decimal_digits
      );

      const hex_digits = /[a-fA-F0-9]+/;
      const hex_literal = seq(
        choice('0x', '0X'),
        number_literal(hex_digits, 'p', decimal_digits)
      );

      return token(choice(decimal_literal, hex_literal));
    },

    // LiteralString
    string: ($) => choice($._quote_string, $._block_string),

    _quote_string: ($) =>
      choice(
        seq(
          field('start', alias('"', '"')),
          field(
            'content',
            optional(alias($._doublequote_string_content, $.string_content))
          ),
          field('end', alias('"', '"'))
        ),
        seq(
          field('start', alias("'", "'")),
          field(
            'content',
            optional(alias($._singlequote_string_content, $.string_content))
          ),
          field('end', alias("'", "'"))
        )
      ),

    _doublequote_string_content: ($) =>
      repeat1(choice(token.immediate(prec(1, /[^"\\]+/)), $.escape_sequence)),

    _singlequote_string_content: ($) =>
      repeat1(choice(token.immediate(prec(1, /[^'\\]+/)), $.escape_sequence)),

    _block_string: ($) =>
      seq(
        field('start', alias($._block_string_start, '[[')),
        field('content', alias($._block_string_content, $.string_content)),
        field('end', alias($._block_string_end, ']]'))
      ),

    escape_sequence: () =>
      token.immediate(
        seq(
          '\\',
          choice(
            /[\nabfnrtv\\'"]/,
            /z\s*/,
            /[0-9]{1,3}/,
            /x[0-9a-fA-F]{2}/,
            /u\{[0-9a-fA-F]+\}/
          )
        )
      ),

    // '...'
    vararg_expression: (_) => '...',

    // functiondef ::= function funcbody
    function_definition: ($) => seq('function', $._function_body),
    // funcbody ::= '(' [parlist] ')' block end
    _function_body: ($) =>
      seq(
        field('parameters', $.parameters),
        field('body', optional_block($)),
        'end'
      ),
    // '(' [parlist] ')'
    parameters: ($) => seq('(', optional($._parameter_list), ')'),
    // parlist ::= namelist [',' '...'] | '...'
    _parameter_list: ($) =>
      choice(
        seq(name_list($), optional(seq(',', $.vararg_expression))),
        $.vararg_expression
      ),

    // funcbody ::= '(' [parlist] ')' block end
    _terra_function_body: ($) =>
      seq(
        field('parameters', $.terra_parameters),
        optional(seq(':', field('return_type', choice($.variable, $.terra_macro_escape)))),
        field('body', terra_optional_block($)),
        'end'
      ),
    // '(' [parlist] ')'
    terra_parameters: ($) => seq('(', optional($._terra_parameter_list), ')'),
    // parlist ::= namelist [',' '...'] | '...'
    _terra_parameter_list: ($) =>
      choice(
        seq(type_name_list($), optional(seq(',', $.vararg_expression))),
        $.vararg_expression
      ),

    // prefixexp ::= var | functioncall | '(' exp ')'
    _prefix_expression: ($) =>
      prec(1, choice($.variable, $.function_call, $.parenthesized_expression)),

    // var ::=  Name | prefixexp [ exp ] | prefixexp . Name
    variable: ($) =>
      choice(
          // $.terra_deref,
          // $.terra_macro_escape,
        $.identifier, $.bracket_index_expression, $.dot_index_expression),
    // prefixexp [ exp ]
    bracket_index_expression: ($) =>
      seq(
        field('table', $._prefix_expression),
        '[',
        field('field', $.expression),
        ']'
      ),
    // prefixexp . Name
    dot_index_expression: ($) =>
      seq(
        field('table', $._prefix_expression),
        '.',
        field('field', $.identifier)
      ),

    // functioncall ::=  prefixexp args | prefixexp ':' Name args
    function_call: ($) =>
      seq(
        field('name', choice($._prefix_expression, $.method_index_expression)),
        field('arguments', $.arguments)
      ),
    // prefixexp ':' Name
    method_index_expression: ($) =>
      seq(
        field('table', $._prefix_expression),
        ':',
        field('method', $.identifier)
      ),
    // args ::=  '(' [explist] ')' | tableconstructor | LiteralString
    arguments: ($) =>
      choice(
        seq('(', optional(list_seq($.expression, ',')), ')'),
        $.table_constructor,
        $.string
      ),

    // '(' exp ')'
    parenthesized_expression: ($) => seq('(', $.expression, ')'),

    // tableconstructor ::= '{' [fieldlist] '}'
    table_constructor: ($) => seq('{', optional($._field_list), '}'),
    // fieldlist ::= field {fieldsep field} [fieldsep]
    _field_list: ($) => list_seq($.field, $._field_sep, true),
    // fieldsep ::= ',' | ';'
    _field_sep: (_) => choice(',', ';'),
    // field ::= '[' exp ']' '=' exp | Name '=' exp | exp
    field: ($) =>
      choice(
        seq(
          '[',
          field('name', $.expression),
          ']',
          '=',
          field('value', $.expression)
        ),
        seq(field('name', $.identifier), '=', field('value', $.expression)),
        field('value', $.expression)
      ),

    // exp binop exp
    binary_expression: ($) =>
      choice(
        ...[
          ['or', PREC.OR],
          ['and', PREC.AND],
          ['<', PREC.COMPARE],
          ['<=', PREC.COMPARE],
          ['==', PREC.COMPARE],
          ['~=', PREC.COMPARE],
          ['>=', PREC.COMPARE],
          ['>', PREC.COMPARE],
          ['|', PREC.BIT_OR],
          ['~', PREC.BIT_NOT],
          ['&', PREC.BIT_AND],
          ['<<', PREC.BIT_SHIFT],
          ['>>', PREC.BIT_SHIFT],
          ['+', PREC.PLUS],
          ['-', PREC.PLUS],
          ['*', PREC.MULTI],
          ['/', PREC.MULTI],
          ['//', PREC.MULTI],
          ['%', PREC.MULTI],
        ].map(([operator, precedence]) =>
          prec.left(
            precedence,
            seq(
              field('left', $.expression),
              operator,
              field('right', $.expression)
            )
          )
        ),
        ...[
          ['..', PREC.CONCAT],
          ['^', PREC.POWER],
        ].map(([operator, precedence]) =>
          prec.right(
            precedence,
            seq(
              field('left', $.expression),
              operator,
              field('right', $.expression)
            )
          )
        )
      ),

    // unop exp
    unary_expression: ($) =>
      prec.left(
        PREC.UNARY,
        seq(choice('not', '#', '-', '~'), field('operand', $.expression))
      ),

    // Name
    identifier: (_) => {
      const identifier_start =
        /[^\p{Control}\s+\-*/%^#&~|<>=(){}\[\]`@;:,.\\'"\d]/;
      const identifier_continue =
        /[^\p{Control}\s+\-*/%^#&~|<>=(){}\[\]`@;:,.\\'"]*/;
      return token(seq(identifier_start, identifier_continue));
    },

    // comment
    comment: ($) =>
      choice(
        seq(
          field('start', '--'),
          field('content', alias(/[^\r\n]*/, $.comment_content))
        ),
        seq(
          field('start', alias($._block_comment_start, '[[')),
          field('content', alias($._block_comment_content, $.comment_content)),
          field('end', alias($._block_comment_end, ']]'))
        )
      ),
  },
});
