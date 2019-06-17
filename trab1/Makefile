TEXC = pdflatex
BIBC = bibtex

FILE = main

FINAL = Trab1_Estatistica
GRAPH = Graphs

.PHONY: all pdf final view viewFinal clean cleanFinal graph

all: pdf view
final: $(FINAL).pdf
pdf: $(FILE).pdf
graph: $(GRAPH).pdf

viewFinal: $(FINAL).pdf
	evince $(FINAL).pdf

view: $(FILE).pdf
	evince $(FILE).pdf

$(GRAPH).pdf: clean_data.csv process.R
	./process.R
	cp Rplots.pdf $(GRAPH).pdf

$(FINAL).pdf: pdf
	cp $(FILE).pdf $(FINAL).pdf

$(FILE).pdf: $(FILE).tex structure.tex $(GRAPH).pdf
	$(TEXC) $(FILE).tex
	$(TEXC) $(FILE).tex

clean:
	rm -f $(FILE).pdf
	rm -f $(FILE).aux
	rm -f $(FILE).bbl
	rm -f $(FILE).blg
	rm -f $(FILE).dvi
	rm -f $(FILE).log

cleanFinal:
	rm -f $(FINAL).pdf
