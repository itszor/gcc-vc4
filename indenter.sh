#!/bin/sh

INDENT=indent
INDENT="$INDENT -kr"
INDENT="$INDENT --no-tabs"
INDENT="$INDENT --braces-after-if-line --brace-indent 0"
INDENT="$INDENT --case-brace-indentation 0 --case-indentation 4"
INDENT="$INDENT --dont-cuddle-else --dont-cuddle-do-while"
INDENT="$INDENT --no-space-after-function-call-names"

$INDENT -v gcc/config/vc4/*.[ch]

