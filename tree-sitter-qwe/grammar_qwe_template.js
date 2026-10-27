/**
 * @file Grammar for QWE for the tree-sitter parsing library
 * @author Francesc Rocher <francesc.rocher@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

function make_qwe_grammar(grammar_name, comment_prefix) {
    return grammar({
        name: 'qwe_' + grammar_name,
/*
        extras: $ => [
            ///\s/,
            $._code_line
        ],
*/
        rules: {
            source_file: $ => repeat(choice(
                $._qwe_line,
                $._code_line
            )),

            _qwe_line: $ => seq(
                comment_prefix,
                '@lp',
                choice(
                    $.division
                )
                ),

            division: $ => seq(
                $.division_type, $.division_name
            ),
            division_type: $ => choice(
                'part',
                'chapter',
                'section',
                'subsection',
                'subsubsection'
            ),
            division_name: $ => /[^\n]*/,

            _code_line: $ => /[^\n]*/
        }
    });
}

// e.g.: export default make_qwe_grammar('ada', '--');
