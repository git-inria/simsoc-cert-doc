#!/bin/bash
set -e
#set -x

mkdir -p _melt
cd _melt

cp ../../tuong11/_tags .
cp ../../tuong11/melt_lib.ml melt_lib.mlt
cp ../../tuong11/melt_highlight.ml melt_highlight.mlt
cp ../../tuong11/simsoc_cert.ml simsoc_cert.mlt
cp ../t.ml t.mlt
cp ../../tuong11/t.bib .
cp ../../tuong11/Makefile .
cp ../../tuong11/coq_lex.mll .
#cp ../../tuong11/stat_arm1_1789 .
#cp ../../tuong11/stat_arm2_1789 .
#cp ../../tuong11/stat_armocaml_1789 .
libreoffice --headless --invisible --convert-to pdf ../doc/*.odt

meltpp t.mlt -o t.ml -open Latex -open Melt
meltpp simsoc_cert.mlt -o simsoc_cert.ml -open Latex -open Melt
meltpp melt_highlight.mlt -o melt_highlight.ml -open Latex -open Melt
meltpp melt_lib.mlt -o melt_lib.ml -open Latex -open Melt
execopt=..
mlpost -cairo -pdf -ocamlbuild -ccopt "-use-ocamlfind" t.ml -execopt "$execopt"
ocamlbuild -use-ocamlfind -no-links t.native -- "$execopt"
make 2>&1 > /dev/null
