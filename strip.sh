#!/bin/sh
MOTIF="(in-package :sudoku)"
for i in $*
do
    sed  -e "s/^(in-package :sudoku)//" $i
done
