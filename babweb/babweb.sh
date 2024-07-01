#! /bin/bash
export LC_ALL="C"
export GUILE_LOAD_PATH=guileloadpath
export GUILE_LOAD_COMPILED_PATH=guileloadcompiledpath
cd $1
guileexecutable -L . -e '(babweb)' -s babwebstorepath/share/guile/site/3.0/babweb.scm $1 $2

