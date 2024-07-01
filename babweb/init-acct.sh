#! /bin/bash

export LC_ALL="C"
export GUILE_LOAD_PATH=guileloadpath
export GUILE_LOAD_COMPILED_PATH=guileloadcompiledpath
guileexecutable -e '(babweb initacct)' -s babwebstorepath/share/guile/site/3.0/babweb/initacct.scm $1 $2 $3
