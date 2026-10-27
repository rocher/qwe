// Generate the grammar depending on the language name and line prefix e.g.
// for Ada: make_qwe_grammar('ada', '--'), for Python:
// make_qwe_grammar('python', '#'), etc.

function make_qwe_grammar(name, prefix) {
  return grammar({
    name: 'qwe_' + name,

    rules: {
      source_file: $ => repeat($._line),

      _line: $ => choice(
        $.directive_line,
        $.paragraph_line,
        $.other_line
      ),

      // Line with a QWE directive (starts with @lp)
      directive_line: $ => seq(
        /^[ \t]*/,
        prefix,
        /[ \t]*/,
        '@lp',
        /[ \t]*/,
        $._directive,
        label,
        /[ \t]*/,
        text,
        /\r?\n/
      ),

      label: $ => /(:[_a-zA-Z][_a-zA-Z0-9]*)?/,

      text: $ => /[^\n\r]*/,

      // Continuation of a paragraph (does not start with @)
      paragraph_line: $ => seq(
        prefix,
        token(prec(1, /[ \t]*[^@\n\r][^\n\r]*/)),
        /\r?\n/
      ),

      _directive: $ => choice(
        $.section,
      //   $.knwl_artifact
        // ...
        $.subsection
      ),

      section: $ => seq('section', field('title', /[\n\r][^\n\r]*/)),

      subsection: $ => seq('subsection', field('title', /[\n\r][^\n\r]*/)),

      other_line: $ => /[^\n]*\n/
    }
  });
}

// Exportem la gramàtica específica per a Ada (per exemple)
// e.g. module.exports = make_qwe_grammar('ada', '--');
