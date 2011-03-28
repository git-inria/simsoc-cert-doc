#!/bin/bash
set -e
dot -Tpdf g0.txt -o g0.pdf
dot -Tpdf g1.txt -o g1.pdf
dot -Tpdf g2.txt -o g2.pdf
dot -Tpdf g3.txt -o g3.pdf
dot -Tpdf g4.txt -o g4.pdf
dot -Tpdf g5.txt -o g5.pdf
dot -Tpdf g6.txt -o g6.pdf
#dot -Tpdf g7.txt -o g7.pdf
ghostscript -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=resultat.pdf g0.pdf g1.pdf g2.pdf g3.pdf g4.pdf g5.pdf g6.pdf #g7.pdf
rm g0.pdf g1.pdf g2.pdf g3.pdf g4.pdf g5.pdf g6.pdf #g7.pdf