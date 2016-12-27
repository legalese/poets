DOCUMENT = tr

LHS2TEX = @lhs2tex
LATEXMK = @latexmk -pdf
BIBTEX = @bibtex
DEL = @rm -f

%.tex : %.lhs
	lhs2TeX $< > $@

%.sty : %.lhs
	lhs2TeX $< > $@

all:	mylhs2tex.sty implementation.tex
	$(LATEXMK) $(DOCUMENT)

bib:
	$(BIBTEX) $(DOCUMENT)

clean:
	$(LATEXMK) -CA $(DOCUMENT)
	$(DEL) $(DOCUMENT).bbl $(DOCUMENT).ptb $(DOCUMENT).rel
	$(DEL) mylhs2tex.sty
	$(DEL) implementation.tex