#!/bin/sh
set -xe

FFLAGS="-std=f2008 -fbackslash -Wall -Wextra"
LIBS="-lreadline"
SRC="src/rl.f90 src/utf8.f90 src/proc.f90 src/lex.f90 src/paths.f90 src/sig.f90 src/main.f90"

mkdir -p bin/
gcc -c src/util.c -o bin/util.o
gfortran $FFLAGS -J bin/ -o bin/frsh $SRC bin/util.o $LIBS

if [ "$1" == "run" ]; then
  ./bin/frsh
fi
