# Makefile to use knitr for package vignettes
## R_HOME=/usr/local

# put all PDF targets here, separated by spaces
PDFS= coefplot2.pdf

all: $(PDFS)

clean:
	rm -Rf *.tex *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary unnamed-* *.dvi *.idv *.css *.4tc *.4ct glmmadmb-figs/

%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "library(knitr); knit2pdf('$*.Rnw')"
