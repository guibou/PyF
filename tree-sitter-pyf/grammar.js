module.exports = grammar({
  name: 'pyf',

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => repeat($._text_or_interpolation),

    _text_or_interpolation: $ => choice(
        $.escape,
        $.interpolation,
        $.text),
    interpolation: $ => seq(
      '{',
      $.interpolation_content,
        optional($.format_string),
      '}'
    ),
    interpolation_content: $ => repeat1(choice(/[^:]/, "::")),
    text: $ => /[^{}]+/,
    escape: $ => choice(
        "}}",
        "{{",
    ),
    // format_string: $ => /:[^}]+/,
    format_string: $ => $.format_spec,
    format_spec:     $ =>  seq(":",
        optional(seq(optional($.fill), $.align)),
        optional($.sign),
        optional($.alternate),
        optional($.zero),
        optional($.width),
        optional($.grouping_option),
        optional(seq($.precision_dot, $.precision)),
        optional($.type),
    ),
    zero: $ => "0",
    alternate: $ => "#",
    fill:            $ =>  /./,
    align:           $ =>  choice("<", ">", "=", "^"),
    sign:            $ =>  choice("+", "-", " "),
    width:           $ =>  /[0-9]+/,
    grouping_option: $ =>  choice("_", ","),
    precision:       $ =>  /[0-9]+/,
    precision_dot: $ => ".",
    type:            $ =>  choice("b" , "c" , "d" , "e" , "E" , "f" , "F" , "g" , "G" , "n" , "o" , "s" , "x" , "X" , "%"),
  }
});
