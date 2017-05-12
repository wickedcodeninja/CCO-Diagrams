#!/bin/bash

for i in test/parser/*.diag
do
    cat $i | stack exec parse-tdiag | stack exec tc-tdiag | stack exec tdiag2picture | stack exec pp-picture | pdflatex -jobname $i.pdf
done
