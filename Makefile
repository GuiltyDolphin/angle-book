overview :
	sed 's/\\\$$/$$/' < overview.lhs > overview.bk
	lhs2TeX --verb overview.bk -o overview.tex
	pdflatex overview.tex && \
	  bibtex overview.aux && \
	  pdflatex overview.tex && \
	  pdflatex overview.tex && okular overview.pdf

	# lhs2TeX --verb overview.bk -o overview.tex && pdflatex overview.tex && okular overview.pdf
