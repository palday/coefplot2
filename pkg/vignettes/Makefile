# Makefile to use knitr for package vignettes

# put all PDF targets here, separated by spaces
PDFS= coefplot2.pdf

all: $(PDFS)

clean:
	rm -Rf *.tex *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary lme4-extras-figs/

%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "library(knitr); knit2pdf('$*.Rnw')"
