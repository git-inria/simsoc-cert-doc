#!/bin/bash
set -e
#set -x

mkdir -p _melt
cd _melt

cp ../_tags .
cp ../melt_lib.ml melt_lib.mlt
cp ../melt_highlight.ml melt_highlight.mlt
cp ../simsoc_cert.ml simsoc_cert.mlt
cp ../t.ml t.mlt
cp ../t.bib .
cp ../Makefile .
cp ../coq_lex.mll .
cp ../stat_arm1_1789 .
cp ../stat_arm2_1789 .
cp ../stat_armocaml_1789 .

meltpp t.mlt -o t.ml -open Latex -open Melt
meltpp simsoc_cert.mlt -o simsoc_cert.ml -open Latex -open Melt
meltpp melt_highlight.mlt -o melt_highlight.ml -open Latex -open Melt
meltpp melt_lib.mlt -o melt_lib.ml -open Latex -open Melt
mlpost -cairo -pdf -ocamlbuild -ccopt "-use-ocamlfind" t.ml
ocamlbuild -use-ocamlfind -no-links t.native --
make 2>&1 > /dev/null

exit 0
pdftotext -layout ../t.pdf t1.txt0
pdftotext -layout t.pdf t2.txt0
cat t1.txt0 | tr -d '\014' | tr -s '\n' > t1.txt
cat t2.txt0 | tr -d '\014' | tr -s '\n' > t2.txt
meld t1.txt t2.txt
#meltbuild -pdf -no-link -I `ocamlfind -query batteries` t.mlt