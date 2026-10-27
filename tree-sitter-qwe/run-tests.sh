#!/bin/bash

RP=$(realpath "$0")
DN=$(dirname "$RP")
cd "$DN" || exit 1

declare -A lang # Associative array for language names
lang[ada]="ada"
lang[c]="c"
lang[latex]="latex"
lang[lisp]="lisp"
lang[python]="python"

declare -A test # Associative array for test names
test[hello_world]='Hello world'

if [ -z "$1" ]; then
   LANGS="${!lang[@]}"
else
   LANGS="$1"
fi

echo "Languages to process: $LANGS"

for L in $LANGS; do
   LANG=${lang[$L]}
   PREFIX=${prefix[$L]}
   echo "Testing language: ${LANG}"

   LIB_FILE="lib/qwe_${LANG}.so"

   for T in "${!test[@]}"; do
       TEST_NAME="${test[$T]} ${LANG}"
       tree-sitter test -l "${LIB_FILE}" -i "$TEST_NAME"
   done
done
