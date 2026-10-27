#!/bin/bash

RP=$(realpath "$0")
DN=$(dirname "$RP")
cd "$DN" || exit 1

mkdir -p lib

declare -A lang # Associative array for language names
lang[ada]="ada"
lang[c]="c"
lang[latex]="latex"
lang[lisp]="lisp"
lang[python]="python"

declare -A prefix # Associative array for languages
prefix[ada]="--"
prefix[c]="\\/\\/"
prefix[latex]="%"
prefix[lisp]=";"
prefix[python]="#"

if [ -z "$1" ]; then
   LANGS="${!lang[@]}"
else
   LANGS="$1"
fi

echo "Languages to process: $LANGS"

for L in $LANGS; do
   LANG=${lang[$L]}
   PREFIX=${prefix[$L]}
   echo "Generating grammar for language: ${LANG} with prefix: '${PREFIX}'"

   GFILE="grammar_qwe_${LANG}.js"
   LFILE="lib/qwe_${LANG}.so"

   cp grammar_qwe_template.js "${GFILE}"
   echo "export default make_qwe_grammar(\"${LANG}\", \"${PREFIX}\");" \
        >> "${GFILE}"

   tree-sitter generate "${GFILE}"
   tree-sitter build --output "${LFILE}"
done
