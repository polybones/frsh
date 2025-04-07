#!/bin/sh
set -xe

FFLAGS="-std=f2008 -fbackslash -Wall -Wextra"
LIBS="-lreadline"
SRC="src/rl.f90 src/utf8.f90 src/proc.f90 src/lex.f90 src/paths.f90 src/sig.f90 src/main.f90"

mkdir -p build/
gcc -c src/util.c -o build/util.o
gfortran $FFLAGS -J build/ -o build/frsh $SRC build/util.o $LIBS

if [ "$1" == "run" ]; then
  ./build/frsh
fi
