SRC=$(PWD)
FIG=$(SRC)/fig
NAME=t

TEXINPUTS=.:$(SRC):$(SRC)/sty:$(SRC)/LaTeX-template:$(SRC)/pdf:$(FIG)::

FIGURESBASE=${shell grep -h '\\insertfig' $(SRC)/*.tex |  perl -pe 's/^.*\\insertfig\{([a-zA-Z0-9_-]*)\}.*$$/\1.fig/'}
FIGUREPS=$(FIGURESBASE:%.fig=%.pstex)
FIGUREPDFTEX=$(FIGURESBASE:%.fig=%.pdftex_t)
FIGUREPDF=$(FIGURESBASE:%.fig=%.pdf)
FIGUREOBJS=$(FIGUREPS) $(FIGUREPDFTEX) $(FIGUREPDF)

FIG2PDFTEX=fig2dev -L pdftex_t -p $*.pdf $< $@
FIG2PS=fig2dev -L pstex $< $*.pstex
FIG2PDF=epstopdf $*.pstex -o $*.pdf
# JF -> Claude : pstopdf obsoleted by epstopdf


all: $(NAME).pdf

clean:
	rm -f *~ $(FIGUREOBJS) $(NAME).aux $(NAME).blg $(NAME).bbl $(NAME).log

cleanall: clean
	rm -f $(NAME).pdf

%.pdftex_t: $(FIG)/%.fig
	$(FIG2PDFTEX)

%.pstex: $(FIG)/%.fig
	$(FIG2PS)
	$(FIG2PDF)

# fake
%.pdf: %.pstex
	$(FIG2PDF)

$(NAME).pdf: force $(FIGUREOBJS)
	TEXINPUTS=$(TEXINPUTS); export TEXINPUTS; \
	pdflatex --interaction nonstopmode $(SRC)/$(NAME)

bib: force
	TEXINPUTS=$(TEXINPUTS); export TEXINPUTS; \
	bibtex $(NAME)

force:

