all: userManual.pdf

userManual.pdf: userManual.tex mcerlangorg.pdf
	pdflatex userManual
	pdflatex userManual
	pdflatex userManual

mcerlangorg.pdf: mcerlangorg.fig
	fig2dev -L pdf mcerlangorg.fig mcerlangorg.pdf

clean: 
	rm -f *.blg *.aux *.bbl *.log *.dvi *.ps *.pdf *.toc

